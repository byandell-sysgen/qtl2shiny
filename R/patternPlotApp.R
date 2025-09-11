#' Shiny Pattern Plot App
#'
#' @param id identifier for shiny reactive
#' @param hotspot_list,dip_par,pairprobs_obj,patterns,snp_action,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' 
#' @importFrom qtl2pattern sdp_to_pattern
#' @importFrom dplyr filter
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny checkboxInput column downloadButton downloadHandler
#'             fluidRow moduleServer NS observeEvent plotOutput radioButtons
#'             reactive renderPlot renderUI req selectInput setProgress tagList
#'             uiOutput updateSelectInput withProgress
#' @importFrom grDevices dev.off pdf
#' @importFrom utils write.csv
#' @importFrom rlang .data
patternPlotApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Pattern Plot",
    bslib::nav_panel(
      title = "Hotspots",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            projectUI("project_df"),            # project
            hotspotPanelInput("hotspot_list")), # class, subject_model, pheno_names, hotspot
          bslib::card(
            hotspotPanelUI("hotspot_list")),    # window_Mbp, radio, win_par, chr_ct, minLOD
          width = 400),
        hotspotPanelOutput("hotspot_list"))
    ),
    bslib::nav_panel(
      title = "Pattern Plot",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            dipParUI("dip_par"),          # snp_action
            snpListInput("snp_list")),    # scan_window, minLOD, pheno_name
          bslib::card(
            patternInput("pattern_list"), # button, blups, pheno_name
            patternUI("pattern_list")),   # pattern
          width = 400),
        bslib::card(patternPlotOutput("pattern_plot"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    dip_par <- dipParServer("dip_par")
    snp_action <- shiny::reactive({dip_par$snp_action})
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    pairprobs_obj <-
      pairProbsServer("pairprobs", hotspot_list$win_par, project_df)
    pattern_list <- patternServer("pattern_list", hotspot_list, dip_par,
      pairprobs_obj, snp_list$patterns, snp_action, project_df)
    patternPlotServer("pattern_plot", pattern_list, pairprobs_obj)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname patternPlotApp
patternPlotServer <- function(id, pattern_list, pairprobs_obj) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    plot_type_msg <- shiny::reactive({
      switch(shiny::req(input$pat_plot_tab),
        LOD             = list(type = "lod", msg = 'Pattern LOD ...'),
        Effects         = list(type = "coef", msg = 'Pattern Effects ...'),
        "LOD & Effects" = list(type = "coef_and_lod", msg = 'Pattern Effects & LOD ...'))
      #** Effects garbled if more that 4.
      #** Warning in matrix(pattern, nrow(lod), ncol(lod)) :
      #** data length differs from size of matrix: [441 != 63 x 3]
      #** Warning: Removed 378 rows containing missing values or values outside the scale range
      #** (`geom_line()`).
    })
    scan_pat_type_progress <- shiny::reactive({
      if(is.null(pattern_list$scan_pat()))
        return(plot_null())
      shiny::req(pattern_list$scan_pat(), pattern_list$pattern_choices(),
                 pattern_list$pat_par$pheno_name, pairprobs_obj(),
                 plot_type_msg())
      shiny::withProgress(message = plot_type_msg()$msg, value = 0, {
        shiny::setProgress(1)
        scan_pat_type(pattern_list$scan_pat(), pairprobs_obj()$map,
                      plot_type_msg()$type,
                      pattern_list$pattern_choices(), 
                      pattern_list$pat_par$pheno_name,
                      pattern_list$haplos())
      })
    })
    output$pattern_plot_lod <- shiny::renderUI({
      shiny::req(input$pat_plot_tab, plot_type_msg(), scan_pat_type_progress())
      shiny::renderPlot(scan_pat_type_progress())
    })
    output$pattern_plot_eff <- shiny::renderUI({
      shiny::req(input$pat_plot_tab, plot_type_msg(), scan_pat_type_progress())
      shiny::renderPlot(scan_pat_type_progress())
    })
    # ** This one is broken for SDP scans. **
    output$pattern_plot_both <- shiny::renderUI({
      shiny::req(input$pat_plot_tab, plot_type_msg(), scan_pat_type_progress())
      shiny::renderPlot(scan_pat_type_progress())
    })
  })
}
#' @export
#' @rdname patternPlotApp
patternPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("pat_plot_tab"),
    # Note that `plot_type_msg()` changes with `input$pat_plot_tab`.
    bslib::nav_panel("LOD", bslib::card(
      shiny::uiOutput(ns("pattern_plot_lod")))),
    bslib::nav_panel("Effects", bslib::card(
      shiny::uiOutput(ns("pattern_plot_eff")))),
    bslib::nav_panel("LOD & Effects", bslib::card(
      shiny::uiOutput(ns("pattern_plot_both")))))
}

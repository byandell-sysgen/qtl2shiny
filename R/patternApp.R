#' Shiny Pattern Panel App
#'
#' @param id identifier for shiny reactive
#' @param dip_par,hotspot_list,snp_list,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom shiny mainPanel moduleServer NS radioButtons reactive renderText
#'             renderUI req selectInput sidebarPanel strong tagList textOutput
#'             uiOutput
#' @importFrom bslib card layout_sidebar navbar_options navset_tab nav_panel
#'             page_navbar sidebar
#' @importFrom downr downloadServer downloadInput
patternApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test pattern",
    navbar_options = bslib::navbar_options(bg = "#2D89C8", theme = "dark"),
    bslib::nav_panel(
      title = "Hotspots",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            projectUI("project_df"),            # project
            hotspotInput("hotspot_list")),      # class, subject_model, pheno_names, hotspot
          bslib::card(
            hotspotUI("hotspot_list")),         # window_Mbp, radio, win_par, chr_ct, minLOD
            width = 400),
        hotspotOutput("hotspot_list"))
    ),
    bslib::nav_panel(
      title = "Pattern Panel",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            patternInput("pattern_panel"),      # <various>
            bslib::card(
              dipParInput("dip_par")),          # snp_action
            bslib::card(
              snpListInput("snp_list")),        # scan_window, minLOD, pheno_name
            bslib::card(
              dipParUI("dip_par")),             # allele_names
            width = 400),
          bslib::card(dipParUI("dip_par")),     # allele_names
        ),
        downr::downloadInput("download"),       # download inputs for Plot or Table
        bslib::card(patternOutput("pattern_panel"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotServer("hotspot_list", project_df)
    dip_par <- dipParServer("dip_par", hotspot_list)
    snp_action <- shiny::reactive({dip_par$snp_action})
    snp_list <- snpListServer("snp_list", hotspot_list, project_df, snp_action)
    pairprobs_obj <-
      pairProbsServer("pairprobs", hotspot_list$win_par, project_df)
    pattern_list <- patternServer("pattern_panel", dip_par, hotspot_list,
                                   snp_list, pairprobs_obj, project_df)
    downr::downloadServer("download", pattern_list$download_list)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname patternApp
patternServer <- function(id, dip_par, hotspot_list, snp_list,
                               pairprobs_obj, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## SDP Patterns
    snppat_list <- snpPatternServer("snp_pattern", snp_list,
                                    hotspot_list$allele_info)
    pattern_list <- patternDataServer("pattern_list", dip_par, hotspot_list,
                                      snp_list, pairprobs_obj, project_df)
    pattern_plot <- patternPlotServer("pattern_plot", pattern_list,
                                      pairprobs_obj)
    
    # Download.
    download_Plot <- shiny::reactive({
      switch(shiny::req(input$pat_tab),
        SNP = shiny::req(snppat_list$Plot()),
        SDP = shiny::req(pattern_plot$Plot()))
    })
    download_Table <- shiny::reactive({
      switch(shiny::req(input$pat_tab),
        SNP = shiny::req(snppat_list$Table()),
        SDP = shiny::req(pattern_list$pattern_table()))
    })
    download_Filename <- shiny::reactive({
      out_pat <- switch(shiny::req(input$pat_tab),
        SNP = shiny::req(snppat_list$Filename()),
        SDP = "Pattern")
      out <- paste(shiny::req(snp_list$snp_par$pheno_name),
                    out_pat, shiny::req(input$pat_tab), sep = "_")
      c(Plot  = ifelse(shiny::req(input$pat_tab) == "SDP",
                       paste(out, shiny::req(pattern_plot$Filename()),
                         sep = "_"),
                       out),
        Table = out)
    })
    download_list <- shiny::reactiveValues(
      Plot = download_Plot,
      Table = download_Table,
      Filename = download_Filename)
    # Return.
    pattern_list$download_list <- download_list
    pattern_list
  })
}
#' @export
#' @rdname patternApp
patternInput <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    patternDataInput(ns("pattern_list")),  # button, blups, pheno_name
    patternDataUI(ns("pattern_list")))     # pattern
}
#' @export
#' @rdname patternApp
patternOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("pat_tab"),
    bslib::nav_panel("SNP Pattern Scan", value = "SNP",
      bslib::card(snpPatternOutput(ns("snp_pattern")))),
    bslib::nav_panel("SDP Pattern Scans", value = "SDP",
      bslib::navset_tab(
        bslib::nav_panel("Plot",
          bslib::card(patternPlotOutput(ns("pattern_plot")))),
        bslib::nav_panel("Summary",
          bslib::card(patternDataOutput(ns("pattern_list")))))))
}
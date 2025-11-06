#' Shiny Allele App
#'
#' @param id identifier for shiny reactive
#' @param hotspot_list,pattern_list,pairprobs_obj,patterns,project_df,snp_action reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny isTruthy moduleServer NS plotOutput reactive renderPlot
#'             renderUI req setProgress sliderInput uiOutput
#'             updateSliderInput withProgress
#' @importFrom ggplot2 autoplot ggtitle
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom bslib card layout_sidebar navset_tab nav_panel page_navbar sidebar
genoPanelApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Geno Panel",
    bslib::nav_panel(
      title = "Hotspots",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            projectUI("project_df"),          # project
            hotspotInput("hotspot_list")),    # class, subject_model, pheno_names, hotspot
          bslib::card(
            hotspotUI("hotspot_list")),       # window_Mbp, radio, win_par, chr_ct, minLOD
          width = 400),
        hotspotOutput("hotspot_list"))
    ),
    bslib::nav_panel(
      title = "Genotypes",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            patternDataInput("pattern_list"), # button, blups, pheno_name
            patternDataUI("pattern_list")),   # pattern
          bslib::card(
            dipParInput("dip_par")),          # snp_action
          bslib::card(
            snpListInput("snp_list")),        # scan_window, minLOD, pheno_name
          bslib::card(
            dipParUI("dip_par")),             # allele_names
          width = 400),
        bslib::card(genoPanelUI("geno_panel")),
        bslib::card(genoPanelInput("geno_panel"), min_height = "100px"), # pos_Mbp
        bslib::card(genoPanelOutput("geno_panel"))
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
    pattern_list <- patternDataServer("pattern_list", hotspot_list, dip_par,
      pairprobs_obj, snp_list$patterns, snp_action, project_df)
    download_list <-
      genoPanelServer("geno_panel", hotspot_list, pattern_list, snp_list,
                      pairprobs_obj, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname genoPanelApp
genoPanelServer <- function(id, hotspot_list, pattern_list, snp_list, pairprobs_obj,
                       project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    geno_list <- 
      genoServer("geno_list", hotspot_list, snp_list, pairprobs_obj, project_df)
    geno_effect <-
      genoEffectServer("geno_effect", hotspot_list, pattern_list, snp_list,
                     geno_list, pairprobs_obj, project_df)
    
    output$download_list <- shiny::renderUI({
      shiny::req(download_table(), geno_effect$plot())
      shiny::tagList(
        shiny::renderText(paste("table:", dim(download_table()()))),
        shiny::renderText(paste("plot:",  class(geno_effect$plot())[1])))
    })

    download_table <- shiny::reactive({
      shiny::req(geno_list$table(),geno_list$table())
      switch(shiny::req(input$gen_tab),
        Genotypes = geno_list$table,
        Summary   = geno_effect$table)
    })
    # # download_list
    # download_list <- shiny::reactiveValues(
    #   filename = "geno",
    #   plot  = shiny::isolate(geno_effect$plot),
    #   table = shiny::isolate(download_table()))
    # # Return.
    # download_list
  })
}
#' @export
#' @rdname genoPanelApp
genoPanelInput <- function(id) {
  ns <- shiny::NS(id)
  genoInput(ns("geno_list"))                # pos_Mbp
}
#' @export
#' @rdname genoPanelApp
genoPanelUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("download_list"))      # download_list
}
#' @export
#' @rdname genoPanelApp
genoPanelOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("gen_tab"),
    bslib::nav_panel("Effects",
      genoEffectOutput(ns("geno_effect"))), # effect_plot
    bslib::nav_panel("Genotypes",
      genoOutput(ns("geno_list"))),         # geno_table
    bslib::nav_panel("Summary",
      genoEffectUI(ns("geno_effect"))))     # effect_table
}

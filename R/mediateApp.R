#' Shiny Mediate Panel App
#'
#' Shiny module for mediation analysis.
#'
#' @param id identifier for shiny reactive
#' @param hotspot_list,snp_list,probs_obj,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom shiny mainPanel moduleServer NS radioButtons renderText renderUI
#'             req sidebarPanel strong tagList textOutput uiOutput
#' @importFrom bslib card layout_sidebar navbar_options navset_tab nav_panel
#'             page_navbar sidebar
mediateApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Mediate Panel",
    navbar_options = bslib::navbar_options(bg = "#2D89C8", theme = "dark"),
    bslib::nav_panel(
      title = "Hotspots",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            projectUI("project_df"),       # project
            hotspotInput("hotspot_list")), # class, subject_model, pheno_names, hotspot
          bslib::card(
            hotspotUI("hotspot_list")),    # window_Mbp, radio, win_par, chr_ct, minLOD
          width = 400),
        hotspotOutput("hotspot_list"))
    ),
    bslib::nav_panel(
      title = "mediate_panel",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          mediateInput("mediate_panel"),   # <various>
          snpListInput("snp_list")),       # scan_window, minLOD, pheno_name
        downr::downloadInput("download"),  # download inputs for Plot or Table
        mediateOutput("mediate_panel")
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotServer("hotspot_list", project_df)
    probs_obj <- probsServer("probs", hotspot_list$win_par, project_df)
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    download_list <- mediateServer("mediate_panel", hotspot_list, snp_list,
                                   probs_obj, project_df)
    downr::downloadServer("download", download_list)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname mediateApp
mediateServer <- function(id, hotspot_list, snp_list, probs_obj,
                               project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Mediation
    mediate_list <- mediateDataServer("mediate_list", hotspot_list, snp_list,
                                      probs_obj, project_df)
    mediate_plot <- mediatePlotServer("mediate_plot", hotspot_list,
                                      mediate_list, probs_obj, project_df)
    triad_plot <- triadServer("triad", hotspot_list, snp_list, mediate_list,
                              probs_obj)
    
    output$mediate_input <- shiny::renderUI({
      bslib::card(
        switch(input$mediate_tab,
          Plot  = mediatePlotInput(ns("mediate_plot")), # static, signif, local, med_plot
          Triad = triadInput(ns("triad"))),             # triad, med_name, triad_plot
        mediateDataInput(ns("mediate_list")))           # qtls, pos_Mbp
    })
    # Download.
    download_Plot <- shiny::reactive({
      switch(shiny::req(input$mediate_tab),
             Summary =,
             Plot    = shiny::req(mediate_plot()),
             Triad   = shiny::req(triad_plot()))
    })
    download_Table <- shiny::reactive({
      shiny::req(mediate_list$mediate_obj())$best
    })
    download_Filename <- shiny::reactive({
      med_pat <- switch(shiny::req(input$mediate_tab),
        Summary =,
        Plot    = "_Mediate",
        Triad   = "_Triad")
      out <- paste0(shiny::req(mediate_list$med_par$pheno_name), med_pat)
      c(Plot = out, Table = out)
    })
    download_list <- shiny::reactiveValues(
      Plot = download_Plot,
      Table = download_Table,
      Filename = download_Filename)
    # Return.
    download_list
  })
}
#' @export
#' @rdname mediateApp
mediateInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("mediate_input"))
}
#' @export
#' @rdname mediateApp
mediateOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("mediate_tab"),
    bslib::nav_panel("Plot",    mediatePlotOutput(ns("mediate_plot"))),
    bslib::nav_panel("Triad",   triadOutput(ns("triad"))),
    bslib::nav_panel("Summary", mediateDataOutput(ns("mediate_list"))))
}  
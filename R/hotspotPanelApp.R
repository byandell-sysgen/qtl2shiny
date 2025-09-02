#' Shiny Hotspot Panel App
#'
#' @param id identifier for shiny reactive
#' @param set_par,peak_df,pmap_obj,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom dplyr filter 
#' @importFrom shiny checkboxInput isTruthy moduleServer NS
#'             observeEvent radioButtons reactive renderText renderUI req
#'             strong tagList textOutput uiOutput
#' @importFrom bslib card page_sidebar sidebar
hotspotPanelApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Hotspot Panel",
    sidebar = bslib::sidebar(
      bslib::card(
        projectUI("project_df"),            # project
        hotspotPanelInput("hotspot_list")), # class, subject_model, pheno_names, hotspot
      bslib::card(
        hotspotPanelUI("hotspot_list")),    # window_Mbp, radio, win_par, chr_ct, minLOD
      width = 400),
    hotspotPanelOutput("hotspot_list")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname hotspotPanelApp
hotspotPanelServer <- function(id, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Modules.    
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    hotspot_df <- 
      hotspotServer("hotspot", set_par, peak_df, pmap_obj, project_df)
    win_par <- winParServer("win_par", hotspot_df, project_df)
    pheno_names <-
      phenoNamesServer("pheno_names", set_par, win_par, peak_df, project_df)
    pheno_mx <- phenoServer("pheno_mx", set_par, pheno_names, project_df)
    covar_df <- covarServer("covar_df", pheno_mx, project_df)
    phenoPlotServer("pheno_plot", pheno_names, pheno_mx, covar_df)
    
    output$version <- shiny::renderText({
      versions()
    })
    
    output$hotspot_switch <- shiny::renderUI({
      switch(shiny::req(input$radio),
        Phenotypes = shiny::uiOutput(ns("pheno_output")),
        Hotspots   = shiny::uiOutput(ns("hotspot_output")))
    })
    output$pheno_output <- shiny::renderUI({
      shiny::tagList(
        phenoPlotOutput(ns("pheno_plot")),   # pheno_plot
        phenoNamesOutput(ns("pheno_names"))) # pheno_names
    })
    output$hotspot_output <- shiny::renderUI({
      shiny::tagList(
        hotspotOutput(ns("hotspot")),        # hotspot_plot
        hotspotUI(ns("hotspot")))            # hotspot_table
    })
    output$radio_input <- shiny::renderUI({
      shiny::radioButtons(ns("radio"), NULL,
        c("Hotspots", "Phenotypes"), input$radio, inline=TRUE)
    })
    
    ## Additional reactives not used yet.
    kinship_list <- kinshipServer("kinship_list", win_par, project_df)
    allele_info <- shiny::reactive(read_project(project_df(), "allele_info"))
    
    ## Return.
    shiny::reactiveValues(
      pheno_names = pheno_names,
      win_par = win_par,
      peak_df = peak_df,
      pmap_obj = pmap_obj,
      pheno_mx = pheno_mx,
      covar_df = covar_df,
      kinship_list = kinship_list,
      allele_info = allele_info)
  })
}
#' @export
#' @rdname hotspotPanelApp
hotspotPanelInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    setParInput(ns("set_par")),           # class, subject_model
    phenoNamesInput(ns("pheno_names"))    # pheno_names
  )
}
#' @export
#' @rdname hotspotPanelApp
hotspotPanelUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    winParInput(ns("win_par")),           # hotspot
    bslib::layout_columns(
      col_widths = c(4, 8),
      setParUI(ns("set_par")),            # window_Mbp
      hotspotInput(ns("hotspot"))))       # chr_ct, minLOD
    #shiny::uiOutput(ns("version"))
}
#' @export
#' @rdname hotspotPanelApp
hotspotPanelOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("radio_input")),   # radio
    shiny::uiOutput(ns("hotspot_switch"))
  )
}
#' @export
#' @rdname hotspotPanelApp
hotspotPhenoOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("pheno_output"))
}
#' @export
#' @rdname hotspotPanelApp
hotspotHotspotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("hotspot_output"))
}

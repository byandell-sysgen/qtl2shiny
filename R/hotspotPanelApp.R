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
    covar_df <- covarServer("covar_df", project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    hotspot_df <- 
      hotspotServer("hotspot_df", set_par, peak_df, pmap_obj, project_df)
    win_par <- winParServer("win_par", hotspot_df, project_df)
    pheno_list <-
      phenoPanelServer("pheno_panel", set_par, win_par, peak_df, covar_df,
                       pmap_obj, hotspot_df, project_df)
    
    ## Return.
    pheno_list
  })
}
#' @export
#' @rdname hotspotPanelApp
hotspotPanelInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    setParInput(ns("set_par")),             # class, subject_model
    phenoPanelInput(ns("pheno_panel"))      # pheno_names
  )
}
#' @export
#' @rdname hotspotPanelApp
hotspotPanelUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    winParInput(ns("win_par")),             # hotspot
    bslib::layout_columns(
      col_widths = c(4, 8),
      setParUI(ns("set_par")),              # window_Mbp
      hotspotInput(ns("hotspot_df"))))      # chr_ct, minLOD
    #shiny::uiOutput(ns("version"))
}
#' @export
#' @rdname hotspotPanelApp
hotspotPanelOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    bslib::nav_panel("Hotspots",
      shiny::tagList(
        hotspotOutput(ns("hotspot_df")),    # hotspot_plot
        hotspotUI(ns("hotspot_df")))),      # hotspot_table
    bslib::nav_panel("Phenotypes", 
      phenoPanelOutput(ns("pheno_panel")))) # pheno_plot, pheno_names
}

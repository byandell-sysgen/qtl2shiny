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
#' @importFrom bslib accordion accordion_panel card page_sidebar sidebar
hotspotAccordionApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Hotspot Panel",
    sidebar = bslib::sidebar(
      bslib::accordion(
        projectUI("project_df"),            # project
        hotspotPanelInput("hotspot_list")), # class, subject_model, pheno_names, hotspot
      bslib::accordion(
        hotspotPanelUI("hotspot_list")),    # window_Mbp, radio, win_par, chr_ct, minLOD
      width = 400),
    bslib::accordion(open = "Hotspots",
      bslib::accordion_panel("Hotspots",
        hotspotHotspotOutput("hotspot_list")),
      bslib::accordion_panel("Phenotypes",
        hotspotPhenoOutput("hotspot_list"))
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
  }
  shiny::shinyApp(ui, server)
}

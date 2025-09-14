#' Shiny QTL2 Shiny App
#'
#' @param id shiny identifier
#' @param projects_df static data frame with project information
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom qtl2mediate get_covar
#' @importFrom dplyr filter 
#' @importFrom shiny moduleServer NS reactive req 
#' @importFrom rlang .data
#' 
qtl2shinyApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Whole App",
    navbar_options = bslib::navbar_options(bg = "red", theme = "dark"),
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
    ## Currently just uses Haplo and Diplo Apps.
    ## Need to rethink dashServer
    bslib::nav_panel(
      title = "Allele and SNP Scans",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          haploUI("haplo"),                     # <various>
          haploInput("haplo")),                # <various>
        haploOutput("haplo")
      )
    ),
    bslib::nav_panel(
      title = "Mediation",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          mediatePanelInput("mediate_panel"),   # <various>
          snpListInput("snp_list")),            # scan_window, minLOD, pheno_name
        mediatePanelOutput("mediate_panel")
      )
    ),
    bslib::nav_panel(
      title = "Strain Distribution Patterns",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(diploUI("diplo")), # <various>
        bslib::card(diploOutput("diplo"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    haploServer("haplo", hotspot_list, project_df)
    # ** Need to reconfigure all these modules.
    mediatePanelServer("mediate_panel", hotspot_list, snp_list, probs_obj,
                       project_df)
    diploServer("diplo", hotspot_list, project_df)
  }
  shiny::shinyApp(ui, server)
}

#' SNP Setup for Diplo App
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom bslib card layout_sidebar nav_panel page_navbar sidebar
snpSetupDiploApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test SNP Setup for Diplo",
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
      title = "snpSetupDiplo",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          dipParInput("dip_par"),      # sex_type
          dipParUI("dip_par"),         # snp_action
          snpSetupInput("snp_setup")), # <various>
        bslib::card(snpSetupOutput("snp_setup"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    dip_par <- dipParServer("dip_par")
    snp_action <- shiny::reactive({dip_par$snp_action})
    patterns <-
      snpSetupServer("snp_setup", hotspot_list, dip_par, project_df, snp_action)
  }
  shiny::shinyApp(ui, server)
}
#' @export
snpListDiploApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test SNP List",
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
      title = "snpList",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          dipParInput("dip_par"),               # sex_type
          dipParUI("dip_par"),                  # snp_action
          snpListInput("snp_list"),             # scan_window
          snpListInput2("snp_list"),            # minLOD
          snpListUI("snp_list")),               # pheno_name
        bslib::card(snpListOutput("snp_list"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    dip_par <- dipParServer("dip_par")
    snp_list <- snpListServer("snp_list", hotspot_list, dip_par, project_df)
  }
  shiny::shinyApp(ui, server)
}

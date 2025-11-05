#' Shiny Scan Panel App
#'
#' @param id identifier for shiny reactive
#' @param hotspot_list,project_df reactive arguments
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
scanPanelApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Scan Panel",
    navbar_options = bslib::navbar_options(bg = "#2D89C8", theme = "dark"),
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
      title = "scanPanel",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          scanPanelInput("scan_panel"),         # <various>
          snpListInput("snp_list")),            # scan_window, minLOD, pheno_name
        scanPanelOutput("scan_panel")
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    probs_obj <- probsServer("probs", hotspot_list$win_par, project_df)
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    scanPanelServer("scan_panel", hotspot_list, snp_list, probs_obj, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname scanPanelApp
scanPanelServer <- function(id, hotspot_list, snp_list, probs_obj, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    scanServer("scan", hotspot_list, probs_obj, project_df)
    snpGeneServer("snp_gene", snp_list, project_df)

    output$scan_input <- shiny::renderUI({
      switch(shiny::req(input$hap_tab),
        "Genome Scans"    = scanInput(ns("scan")),        # blups, pheno_name, scan_window
        "SNP Association" = snpGeneInput(ns("snp_gene"))) # SNP, gene_name
    })
  })
}
#' @export
#' @rdname scanPanelApp
scanPanelInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("scan_input"))          # <various>
}
#' @export
#' @rdname scanPanelApp
scanPanelOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("hap_tab"),
    bslib::nav_panel("Genome Scans",    scanOutput(ns("scan"))),
    bslib::nav_panel("SNP Association", snpGeneOutput(ns("snp_gene"))))
}  
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
#' @importFrom downr downloadServer downloadInput
scanApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Scan Panel",
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
      title = "scan",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          scanInput("scan_panel"),         # <various>
          snpListInput("snp_list")),       # scan_window, minLOD, pheno_name
        downr::downloadInput("download"),  # download inputs for Plot or Table
        scanOutput("scan_panel")
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotServer("hotspot_list", project_df)
    probs_obj <- probsServer("probs", hotspot_list$win_par, project_df)
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    download_list <- scanServer("scan_panel", hotspot_list, snp_list,
                                     probs_obj, project_df)
    downr::downloadServer("download", download_list)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname scanApp
scanServer <- function(id, hotspot_list, snp_list, probs_obj, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    scan_list <- scanDataServer("scan_list", hotspot_list, probs_obj, project_df)
    gene_list <- snpGeneServer("snp_gene", snp_list, project_df)

    output$scan_input <- shiny::renderUI({
      switch(shiny::req(input$hap_tab),
        scan = scanDataInput(ns("scan_list")), # blups, pheno_name, scan_window
        snp  = snpGeneInput(ns("snp_gene")))   # SNP, gene_name
    })
    
    # Download.
    download_Plot <- shiny::reactive({
      switch(shiny::req(input$hap_tab),
        scan = scan_list$Plot(),
        snp  = gene_list$Plot())
    })
    download_Table <- shiny::reactive({
      switch(shiny::req(input$hap_tab),
             scan = scan_list$Table(),
             snp  = gene_list$Table())
    })
    download_Filename <- shiny::reactive({
      switch(shiny::req(input$hap_tab),
             scan = scan_list$Filename(),
             snp  = gene_list$Filename())
    })
    download_Type <- shiny::reactive({
      switch(shiny::req(input$hap_tab),
             scan = scan_list$Type(),
             snp  = gene_list$Type())
    })
    download_list <- shiny::reactiveValues(
      Plot = download_Plot,
      Table = download_Table,
      Filename = download_Filename,
      Type = download_Type)
    
    # Return.
    download_list
  })
}
#' @export
#' @rdname scanApp
scanInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("scan_input"))          # <various>
}
#' @export
#' @rdname scanApp
scanOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("hap_tab"),
    bslib::nav_panel("Genome Scans",    value = "scan",
                     scanDataOutput(ns("scan_list"))),
    bslib::nav_panel("SNP Association", value = "snp",
                     snpGeneOutput(ns("snp_gene"))))
}  
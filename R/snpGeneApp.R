#' Shiny SNP Gene App
#' 
#' Shiny module for SNP association mapping, with interfaces \code{snpGeneInput}, \code{snpGeneUI} and  \code{snpGeneOutput}.
#'
#' @param id identifier for shiny reactive
#' @param snp_list,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return tbl with top SNPs
#'
#' @export
#' @importFrom shiny column fluidRow moduleServer NS reactive
#'             renderUI req tagList uiOutput
#' @importFrom bslib card layout_sidebar navset_tab nav_panel
#'             page_navbar sidebar
snpGeneApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test SNP Gene",
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
      title = "snpGene",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          snpGeneInput("snp_gene"),             # SNP, gene_name
          snpListInput("snp_list")),            # scan_window, minLOD, pheno_name
        bslib::card(snpGeneOutput("snp_gene"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    ass_par <- snpGeneServer("snp_gene", snp_list, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname snpGeneApp
snpGeneServer <- function(id, snp_list, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    snpPlotServer("snp_scan", snp_list)
    snpSumServer("best_snp", snp_list, project_df)
    geneRegionServer("gene_region", snp_list, project_df)
    geneExonServer("gene_exon", snp_list)
    
    output$snp_input <- shiny::renderUI({
      switch(shiny::req(input$gen_tab),
             Genes   = geneRegionInput(ns("gene_region")), # SNP
             Exons   = geneExonInput(ns("gene_exon")))     # gene_name
    })
  })
}
#' @export
#' @rdname snpGeneApp
snpGeneInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("snp_input"))  # SNP, gene_name
}
#' @export
#' @rdname snpGeneApp
snpGeneOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("gen_tab"),
    bslib::nav_panel("SNP Scan", bslib::navset_tab(
      bslib::nav_panel("Plot", bslib::card(
        snpPlotOutput(ns("snp_scan")))),
      bslib::nav_panel("Summary", bslib::card(
        snpSumOutput(ns("best_snp")))))),
    bslib::nav_panel("Genes", bslib::card(
      geneRegionOutput(ns("gene_region")))),   # gene_plot, gene_table
    bslib::nav_panel("Exons", bslib::card(
      geneExonOutput(ns("gene_exon")))))       # exon_plot, exon_table
}

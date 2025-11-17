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
            projectUI("project_df"),        # project
            hotspotInput("hotspot_list")),  # class, subject_model, pheno_names, hotspot
          bslib::card(
            hotspotUI("hotspot_list")),     # window_Mbp, radio, win_par, chr_ct, minLOD
          width = 400),
        hotspotOutput("hotspot_list"))
    ),
    bslib::nav_panel(
      title = "snpGene",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          snpGeneInput("snp_gene"),         # SNP, gene_name
          snpListInput("snp_list")),        # scan_window, minLOD, pheno_name
        bslib::card(
          downr::downloadInput("download"), # download inputs for Plot or Table
          snpGeneOutput("snp_gene"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotServer("hotspot_list", project_df)
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    download_list <- snpGeneServer("snp_gene", snp_list,
                                   project_df)
    downr::downloadServer("download", download_list)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname snpGeneApp
snpGeneServer <- function(id, snp_list, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    snp_plot  <- snpPlotServer("snp_scan", snp_list)
    snp_table <- snpSumServer("best_snp", snp_list, project_df)
    gene_list <- geneRegionServer("gene_region", snp_list, project_df)
    exon_list <- geneExonServer("gene_exon", snp_list)
    
    output$snp_input <- shiny::renderUI({
      switch(shiny::req(input$gen_tab),
             Genes   = geneRegionInput(ns("gene_region")), # SNP
             Exons   = geneExonInput(ns("gene_exon")))     # gene_name
    })
    
    # Download.
    download_Plot <- shiny::reactive({
      switch(shiny::req(input$gen_tab),
             SNP   = shiny::req(snp_plot()),
             Genes = shiny::req(gene_list$Plot()),
             Exons = shiny::req(exon_list$Plot()))
    })
    download_Table <- shiny::reactive({
      switch(shiny::req(input$gen_tab),
             SNP   = shiny::req(snp_table()),
             Genes = shiny::req(gene_list$Table()),
             Exons = shiny::req(exon_list$Table()))
    })
    download_Filename <- shiny::reactive({
      gen_tab <- shiny::req(input$gen_tab)
      pheno_names <- switch(gen_tab,
        SNP = paste(shiny::req(snp_list$pheno_names()), collapse = "_"),
        Genes = shiny::req(snp_list$snp_par$pheno_name),
        Exons = paste(shiny::req(snp_list$snp_par$pheno_name),
                      shiny::req(exon_list$exon_par$gene_name),
                      sep = "_"))
      out <- paste(pheno_names, gen_tab, sep = "_")
      out_table <- pheno_names
      if(gen_tab == "Exons")
        out_table <- shiny::req(snp_list$snp_par$pheno_name)
      out_table <- paste(out_table, gen_tab, sep = "_")
      c(Plot = out, Table = out_table)
    })
    download_Type <- shiny::reactive({
      switch(shiny::req(input$gen_tab),
        SNP   = switch(shiny::req(input$snp_gen_tab),
                       Plot = "Plot",
                       Summary = "Table"),
        Genes = shiny::req(gene_list$Type()),
        Exons = shiny::req(exon_list$Type()))
    })
    download_list <- shiny::reactiveValues(
      Plot     = download_Plot,
      Table    = download_Table,
      Filename = download_Filename,
      Type     = download_Type)
    
    # Return.
    download_list
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
    bslib::nav_panel("SNP Scan", value = "SNP", bslib::navset_tab(
      id = ns("snp_gen_tab"),
      bslib::nav_panel("Plot", bslib::card(
        snpPlotOutput(ns("snp_scan")))),
      bslib::nav_panel("Summary", bslib::card(
        snpSumOutput(ns("best_snp")))))),
    bslib::nav_panel("Genes", bslib::card(
      geneRegionOutput(ns("gene_region")))),   # gene_plot, gene_table
    bslib::nav_panel("Exons", bslib::card(
      geneExonOutput(ns("gene_exon")))))       # exon_plot, exon_table
}

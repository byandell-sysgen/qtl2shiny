#' Shiny Genes in SNP Region module
#'
#' Shiny module for scan1 analysis and plots, with interfaces \code{geneRegionInput}, \code{geneRegionUI} and  \code{geneRegionOutput}.
#'
#' @param id identifier for shiny reactive
#' @param snp_list,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom ggplot2 autoplot ggtitle
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny checkboxInput column
#'             fluidRow isTruthy moduleServer NS plotOutput reactive renderPlot
#'             renderUI req setProgress tagList uiOutput withProgress
#' @importFrom utils write.csv
#' @importFrom grDevices dev.off pdf
#' @importFrom bslib card layout_sidebar navset_tab nav_panel
#'             page_navbar sidebar
geneRegionApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Gene Region",
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
      title = "geneRegion",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          geneRegionInput("gene_region"),  # SNP
          snpListInput("snp_list")),       # scan_window, minLOD, pheno_name
        bslib::card(geneRegionOutput("gene_region"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotServer("hotspot_list", project_df)
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    geneRegionServer("gene_region", snp_list, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname geneRegionApp
geneRegionServer <- function(id, snp_list, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rng <- shiny::reactive({
      range(shiny::req(snp_list$top_snps_tbl())$pos)
    })
    chr_id <- shiny::reactive({
      unique(snp_list$top_snps_tbl()$chr)[1]
    })
    gene_region_tbl <- shiny::reactive({
      wrng <- rng()
      shiny::withProgress(message = 'Extract gene features ...',
                          value = 0, {
                            shiny::setProgress(1)
                            gene_region(chr_id(), wrng, project_df())
                          })
    })
    gene_table <- shiny::reactive({
      summary(shiny::req(gene_region_tbl()))
    })
    output$gene_table <- DT::renderDataTable({
      shiny::req(gene_table())
    })
    chr_pos_all <- shiny::reactive({
      chr <- shiny::req(chr_id())
      scan_w <- round(rng(), 2)
      paste(chr, scan_w[1], scan_w[2], sep = "_")
    })
    chr_pos <- shiny::reactive({
      chr <- shiny::req(chr_id())
      scan_w <- shiny::req(snp_list$snp_par$scan_window)
      scan_w <- round(scan_w, 2)
      paste(chr, scan_w[1], scan_w[2], sep = "_")
    })
    
    output$SNP <- shiny::renderUI({
      shiny::checkboxInput(ns("SNP"), "Add SNPs?", input$SNP)
    })
    gene_plot <- shiny::reactive({
      shiny::req(gene_region_tbl(), snp_list$top_snps_tbl())
      wrng <- shiny::req(snp_list$snp_par$scan_window)
      phename <- shiny::req(snp_list$snp_par$pheno_name)
      use_snp <- shiny::isTruthy(input$SNP)
      plot_gene_region(phename, gene_region_tbl(), snp_list$top_snps_tbl(), 
                       wrng, use_snp, snp_list$snp_action())
    })
    output$gene_plot <- shiny::renderPlot({
      print(shiny::req(gene_plot()))
    })
    
    # Download.
    download_Type <- shiny::reactive({
      switch(shiny::req(input$exon_tab),
        Plot    = "Plot",
        Summary = "Table")
    })
    download_list <- shiny::reactiveValues(
      Plot  = gene_plot,
      Table = gene_table,
      Type  = download_Type)
    
    # Return.
    download_list
  })
}
#' @export
#' @rdname geneRegionApp
geneRegionInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("SNP"))
}
#' @export
#' @rdname geneRegionApp
geneRegionOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("exon_tab"),
    bslib::nav_panel("Plot", bslib::card(
      shiny::plotOutput(ns("gene_plot")))),
    bslib::nav_panel("Summary", bslib::card(
      DT::dataTableOutput(ns("gene_table")))))
}

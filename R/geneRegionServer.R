#' Shiny Genes in SNP Region module
#'
#' Shiny module for scan1 analysis and plots, with interfaces \code{geneRegionInput}, \code{geneRegionUI} and  \code{geneRegionOutput}.
#'
#' @param id identifier for shiny reactive
#' @param snp_par,top_snps_tbl,project_df,snp_action reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom ggplot2 autoplot ggtitle
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny checkboxInput column downloadButton downloadHandler
#'             fluidRow isTruthy moduleServer NS plotOutput reactive renderPlot
#'             renderUI req setProgress tagList uiOutput withProgress
#' @importFrom utils write.csv
#' @importFrom grDevices dev.off pdf
geneRegionServer <- function(id, snp_par, top_snps_tbl, project_df,
                            snp_action = shiny::reactive({"basic"})) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rng <- shiny::reactive({
      range(shiny::req(top_snps_tbl())$pos)
    })
    chr_id <- shiny::reactive({
      unique(top_snps_tbl()$chr)[1]
    })
    gene_region_tbl <- shiny::reactive({
      wrng <- rng()
      shiny::withProgress(message = 'Extract gene features ...',
                          value = 0, {
                            shiny::setProgress(1)
                            gene_region(chr_id(), wrng, project_df())
                          })
    })
    output$gene_table <- DT::renderDataTable({
      summary(gene_region_tbl())
    })
    chr_pos_all <- shiny::reactive({
      chr <- shiny::req(chr_id())
      scan_w <- round(rng(), 2)
      paste(chr, scan_w[1], scan_w[2], sep = "_")
    })
    chr_pos <- shiny::reactive({
      chr <- shiny::req(chr_id())
      scan_w <- shiny::req(snp_par$scan_window)
      scan_w <- round(scan_w, 2)
      paste(chr, scan_w[1], scan_w[2], sep = "_")
    })
    
    output$SNP <- shiny::renderUI({
      shiny::checkboxInput(ns("SNP"), "Add SNPs?", input$SNP)
    })
    output$gene_plot <- shiny::renderPlot({
      shiny::req(gene_region_tbl(), top_snps_tbl())
      wrng <- shiny::req(snp_par$scan_window)
      phename <- shiny::req(snp_par$pheno_name)
      use_snp <- shiny::isTruthy(input$SNP)
      plot_gene_region(phename, gene_region_tbl(), top_snps_tbl(), 
                       wrng, use_snp, snp_action())
    })
    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        file.path(paste0("gene_region_", chr_pos_all(), "_", snp_action(), ".csv")) },
      content = function(file) {
        utils::write.csv(shiny::req(gene_region_tbl()), file)
      }
    )
    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        file.path(paste0("gene_region_", chr_pos(), "_", snp_action(), ".pdf")) },
      content = function(file) {
        shiny::req(gene_region_tbl(), top_snps_tbl())
        wrng <- shiny::req(snp_par$scan_window)
        phename <- shiny::req(snp_par$pheno_name)
        use_snp <- shiny::isTruthy(input$SNP)
        pheno_names <- unique(shiny::req(top_snps_tbl())$pheno)
        grDevices::pdf(file, width = 9)
        for(pheno in pheno_names)
          print(plot_gene_region(pheno, gene_region_tbl(), top_snps_tbl(), 
                                 wrng, use_snp, snp_action()))
        grDevices::dev.off()
      }
    )
  })
}
#' @export
#' @rdname geneRegionServer
geneRegionInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("SNP"))
}
#' @export
#' @rdname geneRegionServer
geneRegionUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::fluidRow(
      shiny::column(6, shiny::downloadButton(ns("downloadData"), "CSV")),
      shiny::column(6, shiny::downloadButton(ns("downloadPlot"), "Plot"))))
}
#' @export
#' @rdname geneRegionServer
geneRegionOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(                         # gene_plot, gene_table
    shiny::plotOutput(ns("gene_plot")),
    DT::dataTableOutput(ns("gene_table"))
  )
}

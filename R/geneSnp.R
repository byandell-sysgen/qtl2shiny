#' Shiny SNP Association
#' 
#' Shiny module for SNP association mapping, with interfaces \code{geneSnpInput}, \code{geneSnpUI} and  \code{geneSnpOutput}.
#'
#' @param id identifier for shiny reactive
#' @param snp_par,chr_pos,pheno_names,snp_scan_obj,snpinfo,top_snps_tbl,gene_exon_tbl,project_info,snp_action reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return tbl with top SNPs
#'
#' @export
#' @importFrom shiny column fluidRow moduleServer NS radioButtons reactive
#'             renderUI req tagList uiOutput
geneSnpServer <- function(id, snp_par, chr_pos, pheno_names,
                         snp_scan_obj, snpinfo, top_snps_tbl, 
                         gene_exon_tbl, project_info,
                         snp_action = shiny::reactive({"basic"})) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Shiny Modules
    ## SNP Association Scan
    shinySNPPlot("snp_scan", snp_par, chr_pos, pheno_names,
                 snp_scan_obj, snpinfo, snp_action)
    ## SNP Summary
    shinySNPSum("best_snp", chr_pos, top_snps_tbl, project_info, snp_action)
    ## Gene Region
    geneRegionServer("gene_region", snp_par, top_snps_tbl, project_info, snp_action)
    ## Genes and Exons
    geneExonServer("gene_exon", snp_par, chr_pos, top_snps_tbl, gene_exon_tbl, snp_action)
    
    output$snp_check <- shiny::renderUI({
      switch(shiny::req(input$button),
             Genes   = geneRegionInput(ns("gene_region")))
    })
    output$snp_input <- shiny::renderUI({
      switch(shiny::req(input$button),
             Exons   = geneExonInput(ns("gene_exon")),
             Scan = shinySNPSumInput(ns("best_snp")))
    })
    output$snp_output <- shiny::renderUI({
      switch(shiny::req(input$button),
             Scan    = shiny::tagList(
               shinySNPPlotOutput(ns("snp_scan")),
               shinySNPSumOutput(ns("best_snp"))),
             Genes   = geneRegionOutput(ns("gene_region")),
             Exons   = geneExonOutput(ns("gene_exon")))
    })
    
    ## Downloads
    output$download_csv_plot <- shiny::renderUI({
      switch(shiny::req(input$button),
             Scan    = shiny::tagList(shiny::fluidRow(
               shiny::column(6, shinySNPSumUI(ns("best_snp"))),
               shiny::column(6, shinySNPPlotUI(ns("snp_scan"))))),
             Genes   = geneRegionUI(ns("gene_region")),
             Exons   = geneExonUI(ns("gene_exon")))
    })
    output$radio <- shiny::renderUI({
      shiny::radioButtons(ns("button"), "",
                          c("Scan", "Genes", "Exons"),
                          input$button)
    })
    input
  })
}
#' @export
#' @rdname geneSnpServer
geneSnpInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("radio"))),
      shiny::column(6, shiny::uiOutput(ns("snp_check")))),
    shiny::uiOutput(ns("snp_input"))
  )
}
#' @export
#' @rdname geneSnpServer
geneSnpUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("download_csv_plot"))
}
#' @export
#' @rdname geneSnpServer
geneSnpOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("snp_output"))
}

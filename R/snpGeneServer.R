#' Shiny SNP Association
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
#' @importFrom shiny column fluidRow moduleServer NS radioButtons reactive
#'             renderUI req tagList uiOutput
snpGeneServer <- function(id, snp_list, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Shiny Modules
    ## SNP Association Scan
    snpPlotServer("snp_scan", snp_list) #**#
    ## SNP Summary
    snpSumServer("best_snp", snp_list, project_df) #**#
    ## Gene Region
    geneRegionServer("gene_region", snp_list, project_df) #**#
    ## Genes and Exons
    geneExonServer("gene_exon", snp_list) #**#
    
    output$snp_check <- shiny::renderUI({
      switch(shiny::req(input$button),
             Genes   = geneRegionInput(ns("gene_region"))) # SNP
    })
    output$snp_input <- shiny::renderUI({
      switch(shiny::req(input$button),
             Exons   = geneExonInput(ns("gene_exon")),
             Scan = snpSumInput(ns("best_snp")))
    })
    output$snp_output <- shiny::renderUI({
      switch(shiny::req(input$button),
             Scan    = shiny::tagList(
               snpPlotOutput(ns("snp_scan")),
               snpSumOutput(ns("best_snp"))),
             Genes   = geneRegionOutput(ns("gene_region")),
             Exons   = geneExonOutput(ns("gene_exon")))
    })
    
    ## Downloads
    output$download_csv_plot <- shiny::renderUI({
      switch(shiny::req(input$button),
             Scan    = shiny::tagList(shiny::fluidRow(
               shiny::column(6, snpSumUI(ns("best_snp"))),
               shiny::column(6, snpPlotUI(ns("snp_scan"))))),
             Genes   = geneRegionUI(ns("gene_region")), # gene_plot, gene_table
             Exons   = geneExonUI(ns("gene_exon")))     # exon_plot, exon_table
    })
    output$button_input <- shiny::renderUI({
      shiny::radioButtons(ns("button"), "",
                          c("Scan", "Genes", "Exons"),
                          input$button)
    })
    input
  })
}
#' @export
#' @rdname snpGeneServer
snpGeneInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("button_input"))),
      shiny::column(6, shiny::uiOutput(ns("snp_check")))),
    shiny::uiOutput(ns("snp_input"))
  )
}
#' @export
#' @rdname snpGeneServer
snpGeneUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("download_csv_plot"))
}
#' @export
#' @rdname snpGeneServer
snpGeneOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("snp_output"))
}

#' Shiny SNP summary module
#'
#' Shiny module for SNP summary, with interfaces \code{shinySNPInput}, \code{shinySNPUI} and  \code{shinySNPOutput}.
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
#' @importFrom dplyr filter
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny downloadButton downloadHandler moduleServer NS reactive
#'             renderUI req selectInput setProgress tagList uiOutput
#'             withProgress
#' @importFrom ggplot2 autoplot
#' @importFrom utils write.csv
#' @importFrom rlang .data
#' 
snpSumServer <- function(id, snp_list, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    best_snps <- shiny::reactive({
      shiny::req(snp_list$top_snps_tbl())
      summary(snp_list$top_snps_tbl(),"best")
    })
    best_href <- shiny::reactive({
      best <- shiny::req(best_snps())
      ensembl_gene(best, project_df(), TRUE)
    })
    best_http <- shiny::reactive({
      best <- shiny::req(best_snps())
      ensembl_gene(best, project_df())
    })
    
    output$top_snps_tbl <- DT::renderDataTable({
      shiny::req(snp_list$top_snps_tbl())
      shiny::withProgress(message = "Top SNP Range ...", value = 0,
                          {
                            shiny::setProgress(1)
                            summary(snp_list$top_snps_tbl())
                          })
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
    output$top_snps_best <- DT::renderDataTable({
      shiny::withProgress(message = "Top SNP Best ...", value = 0,
                          {
                            shiny::setProgress(1)
                            shiny::req(best_href())
                          })
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
    output$top_indels <- DT::renderDataTable({
      shiny::withProgress(message = "Top InDels ...", value = 0, {
        shiny::setProgress(1)
        # This might change from .data$type to .data$variant_type someday
        dplyr::filter(shiny::req(best_href()), .data$type != "SNP")
      })
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
    output$top_snps_peak <- DT::renderDataTable({
      shiny::req(snp_list$top_snps_tbl())
      shiny::withProgress(message = "Top SNP Peaks ...", value = 0,
                          {
                            shiny::setProgress(1)
                            summary(snp_list$top_snps_tbl(),"peak")
                          })
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
    output$snp_sum_input <- shiny::renderUI({
      switch(input$snp_sum,
             best   = DT::dataTableOutput(ns("top_snps_best")),
             indels = DT::dataTableOutput(ns("top_indels")),
             peaks  = DT::dataTableOutput(ns("top_snps_peak")),
             range  = DT::dataTableOutput(ns("top_snps_tbl")))
    })
    
    ## Downloads.
    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        file.path(paste0("top_snps_", snp_list$chr_pos(), "_",
                         snp_list$snp_action(), ".csv")) },
      content = function(file) {
        utils::write.csv(best_http(), file)
      }
    )
  })
}
#' @export
#' @rdname snpSumServer
snpSumInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("snp_sum"), "Summary",
                       c("best","indels","peaks","range"))
  )
}
#' @export
#' @rdname snpSumServer
snpSumUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::downloadButton(ns("downloadData"), "CSV")
}
#' @export
#' @rdname snpSumServer
snpSumOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("snp_sum_input"))
}

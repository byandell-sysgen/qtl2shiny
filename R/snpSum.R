#' Shiny SNP summary module
#'
#' Shiny module for SNP summary, with interfaces \code{shinySNPInput}, \code{shinySNPUI} and  \code{shinySNPOutput}.
#'
#' @param id identifier for shiny reactive
#' @param chr_pos,top_snps_tbl,project_info,snp_action reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom dplyr filter select
#' @importFrom shiny moduleServer NS reactive req 
#'   selectInput
#'   dataTableOutput uiOutput
#'   renderDataTable renderUI
#'   tagList
#'   withProgress setProgress
#'   downloadButton downloadHandler
#' @importFrom ggplot2 autoplot
#' @importFrom utils write.csv
#' @importFrom rlang .data
#' 
shinySNPSum <- function(id, chr_pos, top_snps_tbl, project_info,
                        snp_action = shiny::reactive({"basic"})) {
  shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns
  
  best_snps <- shiny::reactive({
    shiny::req(top_snps_tbl())
    summary(top_snps_tbl(),"best")
  })
  best_href <- shiny::reactive({
    best <- shiny::req(best_snps())
    ensembl_gene(best, project_info(), TRUE)
  })
  best_http <- shiny::reactive({
    best <- shiny::req(best_snps())
    ensembl_gene(best, project_info())
  })

  output$top_snps_tbl <- shiny::renderDataTable({
    shiny::req(top_snps_tbl())
    shiny::withProgress(message = "Top SNP Range ...", value = 0,
    {
      shiny::setProgress(1)
      summary(top_snps_tbl())
    })
  }, escape = FALSE,
  options = list(scrollX = TRUE, pageLength = 5))
  output$top_snps_best <- shiny::renderDataTable({
    shiny::withProgress(message = "Top SNP Best ...", value = 0,
    {
      shiny::setProgress(1)
      shiny::req(best_href())
  })
  }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
  output$top_indels <- shiny::renderDataTable({
    shiny::withProgress(message = "Top InDels ...", value = 0,
                 {
                   shiny::setProgress(1)
                   # This might change from .data$type to .data$variant_type someday
                   dplyr::filter(shiny::req(best_href()), .data$type != "SNP")
                 })
  }, escape = FALSE,
  options = list(scrollX = TRUE, pageLength = 5))
  output$top_snps_peak <- shiny::renderDataTable({
    shiny::req(top_snps_tbl())
    shiny::withProgress(message = "Top SNP Peaks ...", value = 0,
    {
      shiny::setProgress(1)
      summary(top_snps_tbl(),"peak")
    })
  }, escape = FALSE,
  options = list(scrollX = TRUE, pageLength = 5))
  output$snp_sum <- shiny::renderUI({
    switch(input$snp_sum,
           best   = shiny::dataTableOutput(ns("top_snps_best")),
           indels = shiny::dataTableOutput(ns("top_indels")),
           peaks  = shiny::dataTableOutput(ns("top_snps_peak")),
           range  = shiny::dataTableOutput(ns("top_snps_tbl")))
  })
  
  ## Downloads.
  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      file.path(paste0("top_snps_", chr_pos(), "_", snp_action(), ".csv")) },
    content = function(file) {
      utils::write.csv(best_http(), file)
    }
  )
})
}

shinySNPSumInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("snp_sum"), "Summary", c("best","indels","peaks","range"))
  )
}
shinySNPSumUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::downloadButton(ns("downloadData"), "CSV")
}
shinySNPSumOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("snp_sum")),
    shiny::uiOutput(ns("radio")),
  )
}

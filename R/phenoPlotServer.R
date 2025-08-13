#' Shiny Phenotype Plot module
#'
#' Shiny module to plot phenotypes, with interface \code{phenoPlotUI}.
#'
#' @param id identifier for shiny reactive
#' @param pheno_names,phe_mx,cov_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @return 2-element vector of scan window
#'
#' @export
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny moduleServer NS plotOutput renderPlot renderUI req
#'             setProgress tagList uiOutput withProgress
phenoPlotServer <- function(id, pheno_names, phe_mx, cov_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Scatter plot or density
    output$pheno_table <- DT::renderDataTable({
      shiny::req(phe_mx())
      shiny::withProgress(message = 'Pheno Summary ...', value = 0, {
        shiny::setProgress(1)
        summary_na(phe_mx())
      })
    }, escape = FALSE, 
    options = list(scrollX = TRUE, 
                   pageLength = 5,
                   lengthMenu = list(c(5,10,-1), c(5,10,"all"))))
    output$pheno_plot <- shiny::renderPlot({
      if(!shiny::isTruthy(pheno_names()))
        return(plot_null("need to\nChoose phenotype"))
      shiny::req(phe_mx(), cov_df())
      shiny::withProgress(message = 'Pheno Plot ...', value = 0, {
        shiny::setProgress(1)
        plot_sex(phe_mx(), cov_df())
      })
    })
    output$pheno_plot_table <- shiny::renderUI({
      shiny::tagList(
        shiny::plotOutput(ns("pheno_plot")),
        DT::dataTableOutput(ns("pheno_table")))
    })
  })
}
#' @export
#' @rdname phenoPlotServer
phenoPlotUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("pheno_plot_table"))
}

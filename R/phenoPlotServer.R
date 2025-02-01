#' Shiny Phenotype Plot module
#'
#' Shiny module to plot phenotypes, with interface \code{phenoPlotUI}.
#'
#' @param id identifier for shiny reactive
#' @param set_par,phe_mx,cov_df reactive arguments
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
phenoPlotServer <- function(id, set_par, phe_mx, cov_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Scatter plot or density
    output$phe_sum <- DT::renderDataTable({
      shiny::req(phe_mx())
      shiny::withProgress(message = 'Pheno Summary ...', value = 0, {
        shiny::setProgress(1)
        summary_na(phe_mx())
      })
    }, escape = FALSE, 
    options = list(scrollX = TRUE, 
                   pageLength = 5,
                   lengthMenu = list(c(5,10,-1), c(5,10,"all"))))
    output$phePlot <- shiny::renderPlot({
      # If no df, gives blank area.
      # Better to either do nothing or do plot_null()
      # use uiOutput and renderUI?
      if(!shiny::isTruthy(set_par$pheno_names))
        return(plot_null("need to\nChoose phenotype"))
      shiny::withProgress(message = 'Pheno Plot ...', value = 0, {
        shiny::setProgress(1)
        plot_sex(phe_mx(), cov_df())
      })
    })
    output$phePlotTable <- shiny::renderUI({
      shiny::tagList(
        shiny::plotOutput(ns("phePlot")),
        DT::dataTableOutput(ns("phe_sum")))
    })
  })
}
#' @export
#' @rdname phenoPlotServer
phenoPlotUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("phePlotTable"))
}

#' Shiny phenotype selection
#'
#' Shiny module for phenotype selection, with interfaces \code{phenoUI} and  \code{phenoOutput}.
#'
#' @param id identifier for shiny reactive
#' @param set_par,win_par,peaks_df,analyses_tbl,cov_df,project_info reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom dplyr arrange desc filter select
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny moduleServer NS radioButtons reactive req tagList uiOutput
#' @importFrom rlang .data
phenoServer <- function(id, set_par, win_par, peaks_df, analyses_tbl, cov_df,
                        project_info) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Output the peaks table
    output$peaks <- DT::renderDataTable({
      dplyr::arrange(
        dplyr::select(
          peaks_df(), .data$pheno, .data$chr, .data$pos, .data$lod),
        dplyr::desc(.data$lod))
    }, options = list(scrollX = TRUE, pageLength = 5,
                      lengthMenu = c(5,10,25)))
    
    ## Density or scatter plot of phenotypes.
    analyses_plot <- shiny::reactive({
      shiny::req(analyses_tbl())
      phename <- shiny::req(set_par$pheno_names)
      dplyr::filter(analyses_tbl(), .data$pheno %in% phename)
    })
    phe_mx <- shiny::reactive({
      pheno_read(project_info(), analyses_plot())
    })
    raw_phe_mx <- shiny::reactive({
      pheno_read(project_info(), analyses_plot(), FALSE)
    })
    phenoPlotServer("PhenoPlotRaw", set_par, raw_phe_mx, cov_df)
    phenoPlotServer("PhenoPlotTrans", set_par, phe_mx, cov_df)
    
    # Show data.
    output$radio_input <- renderUI({
      shiny::radioButtons(ns("radio"), NULL,
                          c("LOD Peaks","Covariates",
                            "Trans Data","Raw Data"),
                          input$radio)
    })
    output$show_data <- renderUI({
      shiny::tagList(
        switch(shiny::req(input$radio),
               "Raw Data"   = phenoPlotUI(ns("PhenoPlotRaw")),
               "Trans Data" = phenoPlotUI(ns("PhenoPlotTrans")),
               "Covariates" = DT::dataTableOutput(ns("analyses_tbl"))),
        if(!(input$radio %in% c("Raw Data","Trans Data")))
          DT::dataTableOutput(ns("peaks")))
    })
    
    # Output the analyses table
    output$analyses_tbl <- DT::renderDataTable({
      collapse_covar(analyses_plot())
    }, options = list(scrollX = TRUE, pageLength = 5))
  })
}
#' @export
#' @rdname phenoServer
phenoUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("radio_input"))
}
#' @export
#' @rdname phenoServer
phenoOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("show_data"))
}

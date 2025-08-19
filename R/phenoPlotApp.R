#' Shiny Phenotype Plot App
#'
#' Shiny module to plot phenotypes.
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
phenoPlotApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Phenotype Plot",
    sidebar = bslib::sidebar(
      projectUI("project_df"),        # project
      setParInput("set_par"),         # class, subject_model 
      phenoNamesInput("pheno_names"), # pheno_names
      winParInput("win_par"),         # local, chr_id, peak_Mbp, window_Mbp
      hotspotInput("hotspot")         # chr_ct, minLOD, window_Mbp
    ),
    phenoPlotOutput("pheno_plot")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    pmap_obj <- pmapServer("pmap_obj", project_df)
    hotspot_df <- 
      hotspotServer("hotspot", set_par, peak_df, pmap_obj, project_df)
    win_par <- 
      winParServer("win_par", set_par, peak_df, pmap_obj, hotspot_df, project_df)
    pheno_names <-
      phenoNamesServer("pheno_names", set_par, win_par, peak_df, project_df)
    pheno_mx <- phenoServer("pheno_mx", set_par, pheno_names, project_df)
    covar_df <- covarServer("covar_df", pheno_mx, project_df)
    phenoPlotServer("pheno_plot", pheno_names, pheno_mx, covar_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname phenoPlotApp
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
#' @rdname phenoPlotApp
phenoPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("pheno_plot_table"))
}

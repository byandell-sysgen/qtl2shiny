#' Shiny Phenotype Plot App
#'
#' Shiny module to plot phenotypes.
#'
#' @param id identifier for shiny reactive
#' @param pheno_names,pheno_mx,covar_df reactive arguments
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
#' @importFrom bslib card layout_columns page_sidebar sidebar
phenoPlotApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Phenotype Plot",
    sidebar = bslib::sidebar(
      projectUI("project_df"),        # project
      setParInput("set_par"),         # class, subject_model 
      phenoNamesInput("pheno_names"), # pheno_names
      bslib::layout_columns(
        col_widths = c(6, 4),
        winParInput("win_par"),       # hotspot
        setParUI("set_par")           # window_Mbp 
      ),
      hotspotInput("hotspot"),        # chr_ct, minLOD
      phenoPlotInput("pheno_plot")    # raw_data
    ),
    bslib::card(phenoPlotOutput("pheno_plot")),
    bslib::card(phenoPlotUI("pheno_plot"))
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    covar_df <- shiny::reactive(read_project(project_df(), "covar"))
    hotspot_df <- 
      hotspotServer("hotspot", set_par, peak_df, pmap_obj, project_df)
    win_par <- 
      winParServer("win_par", hotspot_df, project_df)
    peak_filter_df <- peakFilterServer("peak_filter_df", set_par, win_par,
                                       peak_df, project_df)
    pheno_mx <- phenoServer("pheno_mx", set_par, peak_filter_df, project_df)
    covar_df <- shiny::reactive(read_project(shiny::req(project_df()), "covar"))
    pheno_names <- phenoNamesServer("pheno_names", set_par, peak_filter_df,
                                    pheno_mx, covar_df, project_df)
    pheno_rankz_mx <- 
      phenoPlotServer("pheno_plot", pheno_names, pheno_mx, covar_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname phenoPlotApp
phenoPlotServer <- function(id, pheno_names, pheno_mx, covar_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pheno_mx_names <- shiny::reactive({
      shiny::req(pheno_mx(), pheno_names())
      if(!all(pheno_names() %in% colnames(pheno_mx()))) return(NULL)
      pheno_mx()[, pheno_names(), drop = FALSE]
    })
    pheno_rankz_mx <- shiny::reactive({
      out <- shiny::req(pheno_mx_names())
      rout <- row.names(out)
      if(!shiny::isTruthy(input$raw_data)) {
        out <- apply(out, 2, rankZ)
      }
      row.names(out) <- rout
      out
    })
    pheno_plot_mx <- shiny::reactive({
      if(shiny::isTruthy(input$raw_data))
        pheno_mx_names()
      else
        pheno_rankz_mx()
    })
    
    ## Scatter plot or density
    output$pheno_table <- DT::renderDataTable({
      shiny::req(pheno_plot_mx())
      shiny::withProgress(message = 'Pheno Summary ...', value = 0, {
        shiny::setProgress(1)
        summary_na(pheno_plot_mx())
      })
    }, escape = FALSE, 
    options = list(scrollX = TRUE, 
                   pageLength = 5,
                   lengthMenu = list(c(5,10,-1), c(5,10,"all"))))
    output$pheno_plot <- shiny::renderPlot({
      if(!shiny::isTruthy(pheno_plot_mx()))
        return(plot_null("need to\nChoose phenotype"))
      shiny::req(pheno_plot_mx(), covar_df())
      shiny::withProgress(message = 'Pheno Plot ...', value = 0, {
        shiny::setProgress(1)
        plot_sex(pheno_plot_mx(), covar_df())
      })
    })
    # Return.
    pheno_rankz_mx
  })
}
#' @export
#' @rdname phenoApp
phenoPlotInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::checkboxInput(ns("raw_data"), "Raw Data?", FALSE)
}
#' @export
#' @rdname phenoPlotApp
phenoPlotUI <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("pheno_table"))
}
#' @export
#' @rdname phenoPlotApp
phenoPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("pheno_plot"))
}

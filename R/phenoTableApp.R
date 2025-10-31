#' Shiny Phenotype Table App
#'
#' Shiny module to plot phenotypes.
#'
#' @param id identifier for shiny reactive
#' @param pheno_mx,covar_df reactive arguments
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
phenoTableApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Phenotype Table",
    sidebar = bslib::sidebar(
      projectUI("project_df"),        # project
      setParInput("set_par"),         # class, subject_model 
      phenoNamesInput("pheno_names"), # pheno_names
      bslib::layout_columns(
        col_widths = c(6, 4),
        winParInput("win_par"),       # hotspot
        setParUI("set_par")           # window_Mbp 
      ),
      hotspotInput("hotspot_obj"),    # chr_ct, minLOD
      phenoDataInput("pheno_data")    # raw_data
    ),
    phenoTableOutput("pheno_table")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    covar_df <- shiny::reactive(read_project(project_df(), "covar"))
    hotspot_obj <- 
      hotspotServer("hotspot_obj", set_par, peak_df, pmap_obj, project_df)
    hotspot_df <- 
      hotspotTableServer("hotspot_df", hotspot_obj)
    win_par <- 
      winParServer("win_par", hotspot_df, project_df)
    peak_filter_df <- peakFilterServer("peak_filter_df", set_par, win_par,
                                       peak_df, project_df)
    pheno_mx <- phenoServer("pheno_mx", set_par, peak_filter_df, project_df)
    covar_df <- shiny::reactive(read_project(shiny::req(project_df()), "covar"))
    pheno_names <- phenoNamesServer("pheno_names", set_par, peak_filter_df,
                                    pheno_mx, covar_df, project_df)
    pheno_data_mx <- 
      phenoDataServer("pheno_data", pheno_names, pheno_mx, covar_df)
    pheno_table <- 
      phenoTableServer("pheno_plot", pheno_data_mx, covar_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname phenoTableApp
phenoTableServer <- function(id, pheno_mx, covar_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Scatter plot or density
    pheno_table <- shiny::reactive({
      shiny::req(pheno_mx(), covar_df())
      shiny::withProgress(message = 'Pheno Summary ...', value = 0, {
        shiny::setProgress(1)
        summary_na(pheno_mx(), covar_df())
      })
    })
    output$pheno_render_table <- DT::renderDataTable(
      shiny::req(pheno_table()),
      escape = FALSE, 
      options = list(scrollX = TRUE, 
                     pageLength = 5,
                     lengthMenu = list(c(5,10,-1), c(5,10,"all"))))

    # Return.
    pheno_table
  })
}
#' @export
#' @rdname phenoTableApp
phenoTableOutput <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("pheno_render_table"))
}

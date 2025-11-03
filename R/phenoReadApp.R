#' Shiny Phenotype Read App
#'
#' Shiny module for peak selection.
#'
#' @param id identifier for shiny reactive
#' @param set_par,peak_df,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom shiny  column moduleServer NS reactive req
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom bslib layout_columns page_sidebar sidebar
phenoReadApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Pheno Read",
    sidebar = bslib::sidebar(
      projectUI("project_df"),           # project
      setParInput("set_par")             # class, subject_model
    ),
    phenoReadOutput("pheno_mx"),
    peakReadOutput("peak_df")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_read_df <- peakReadServer("peak_df", set_par, project_df)
    pheno_mx <- phenoReadServer("pheno_mx", set_par, peak_read_df, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname phenoReadApp
phenoReadServer <- function(id, set_par, peak_df, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Phenotypes filtered by `peak_df`.
    pheno_mx <- shiny::reactive({
      shiny::req(project_df(), set_par$class, peak_df())
      pheno_names <- peak_df()$phenotype
      read_pheno(project_df(), set_par$class, columns = pheno_names,
                 peak_df = peak_df())
    })
    
    output$pheno_mx_output <- shiny::renderUI({
      shiny::req(pheno_mx(), set_par$class)
      shiny::renderText(paste("dim of pheno:", paste(dim(pheno_mx()), collapse = ",")))
    })

    ## Return.
    pheno_mx
  })
}
#' @export
#' @rdname phenoReadApp
phenoReadOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("pheno_mx_output"))
}

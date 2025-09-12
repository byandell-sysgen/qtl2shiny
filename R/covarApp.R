#' Shiny Covariate App
#'
#' @param id identifier for shiny reactive
#' @param project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom shiny moduleServer NS reactive req
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom bslib page_sidebar sidebar
covarApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Covariate Read",
    sidebar = bslib::sidebar(
      projectUI("project_df")),       # project  
    covarOutput("covar_df")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    covar_df <- covarServer("covar_df", project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname covarApp
covarServer <- function(id, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    covar_df <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "covar")
    })

    # Output Peak Table.
    output$covar_table <- DT::renderDataTable({
      shiny::req(covar_df())      
    }, options = list(scrollX = TRUE, pageLength = 5,
                      lengthMenu = c(5,10,25)))
    
    ## Return.
    covar_df
  })
}
#' @export
#' @rdname covarApp
covarOutput <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("covar_table")) # covar_table
}

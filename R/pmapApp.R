#' Shiny Pmap App
#'
#' @param id identifier for shiny reactive
#' @param project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom shiny moduleServer NS reactive renderTable req
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom bslib page_sidebar sidebar
pmapApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Physical Map Read",
    sidebar = bslib::sidebar(
      projectUI("project_df")), # project
    pmapOutput("pmap_obj")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    pmap_obj <- pmapServer("pmap_obj", project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname pmapApp
pmapServer <- function(id, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pmap_obj <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "pmap")
    })
    
    # Output Peak Table.
    output$pmap_table <- DT::renderDataTable({
      t(sapply(shiny::req(pmap_obj()),
             function(x) {c(markers = length(x), Mb = diff(range(x)))}))
    })
    
    ## Return.
    pmap_obj
  })
}
#' @export
#' @rdname pmapApp
pmapOutput <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("pmap_table")) # pmap_table
}

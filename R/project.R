#' Shiny project selection
#'
#' Shiny module for selection of project, with interface \code{shinyProjectUI}.
#'
#' @param id identifier for shiny reactive
#' @param projects_info reactive project info data frame
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom shiny isTruthy moduleServer NS renderUI selectInput uiOutput
#' @importFrom dplyr distinct filter
#' @importFrom rlang .data
#' 
shinyProject <- function(id, projects_info) {
  shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns
  
  output$project <- shiny::renderUI({
    shiny::req(projects_info())
    choices <- unique(projects_info()$project)
    if(is.null(selected <- input$project)) {
      selected <- choices[1]
    }
    shiny::selectInput(ns("project"), "Project", choices, selected)
  })
  
  shiny::reactive({
    shiny::req(projects_info())
    project_id <- NULL
    if(shiny::isTruthy(input$project)) {
      project_id <- input$project
    }
    if(is.null(project_id)) {
      project_id <- projects_info()$project[1]
    }
    dplyr::distinct(
      dplyr::filter(
        projects_info(),
        .data$project == project_id),
      .data$project, .keep_all = TRUE)
  })
})
}
shinyProjectUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("project"))
}

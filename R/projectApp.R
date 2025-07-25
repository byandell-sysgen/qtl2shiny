#' Shiny Project App
#'
#' Shiny module for selection of project, with interface \code{projectUI}.
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
#' @importFrom shiny isTruthy moduleServer NS renderTable renderUI selectInput
#'             tableOutput uiOutput
#' @importFrom dplyr distinct filter
#' @importFrom rlang .data
#' @importFrom bslib page
#' @importFrom DT dataTableOutput renderDataTable
projectApp <- function() {
  projects <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page(
    title =  "Test Project",
    projectUI("project"),
    shiny::tableOutput("table")
  )
  server <- function(input, output, session) {
    projects_info <- shiny::reactive({projects})
    project_info <- projectServer("project", projects_info)
    message("project_info ", 
            paste(shiny::isolate(names(project_info())), collapse = ", "))
    output$table <- shiny::renderTable(project_info())
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname projectApp
projectServer <- function(id, projects_info) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$project_input <- shiny::renderUI({
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
#' @export
#' @rdname projectApp
projectUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("project_input"))
}

#' Shiny Project App
#'
#' Shiny module for selection of project, with interface \code{projectUI}.
#'
#' @param id identifier for shiny reactive
#' @param projects_df static project info data frame
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom shiny isTruthy moduleServer NS renderTable renderUI selectInput
#'             tableOutput tagList uiOutput
#' @importFrom dplyr distinct filter
#' @importFrom rlang .data
#' @importFrom bslib page
#' @importFrom DT dataTableOutput renderDataTable
projectApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page(
    title =  "Test Project",
    projectUI("project_df"),
    projectOutput("project_df")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname projectApp
projectServer <- function(id, projects_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$project_input <- shiny::renderUI({
      choices <- unique(projects_df$project)
      if(is.null(selected <- input$project)) {
        selected <- choices[1]
      }
      shiny::selectInput(ns("project"), "Project", choices, selected)
    })
    
    output$project_table <- shiny::renderTable(project_df())
    output$pheno_table <- shiny::renderTable({
      data.frame(phenotype_class = project_classes(project_df()),
                 filename = project_phenos(project_df()))
    })
    output$peak_table <- DT::renderDataTable({
      project_peaks(project_df())
    })
    output$project_output <- shiny::renderUI({
      shiny::tagList(
        shiny::tableOutput(ns("project_table")),
        shiny::strong("Phenotype Classes"),
        shiny::tableOutput(ns("pheno_table")),
        shiny::strong("Peak Phenotype Classes"),
        DT::dataTableOutput(ns("peak_table"))
      )
    })
    
    project_df <- shiny::reactive({
      project_id <- NULL
      if(shiny::isTruthy(input$project)) {
        project_id <- input$project
      }
      if(is.null(project_id)) {
        project_id <- projects_df$project[1]
      }
      dplyr::distinct(
        dplyr::filter(
          projects_df,
          .data$project == project_id),
        .data$project, .keep_all = TRUE)
    })
    
    ## Return.
    project_df
  })
}
#' @export
#' @rdname projectApp
projectUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("project_input"))
}
#' @export
#' @rdname projectApp
projectOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("project_output"))
}


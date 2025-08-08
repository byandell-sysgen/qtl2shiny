#' Shiny Setup Parameters App
#'
#' Shiny module for phenotype selection, with interfaces \code{setupInput} and  \code{setupUI}.
#'
#' @param id identifier for shiny reactive
#' @param peak_df,pmap_obj,covar,projects_info reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom dplyr filter 
#' @importFrom shiny checkboxInput moduleServer NS reactive
#'             renderText renderUI req uiOutput
#' @importFrom bslib page_sidebar sidebar
setParApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Setup Parameters",
    sidebar = bslib::sidebar(
      projectUI("project"),
      setParInput("set_par")), # class
    setParOutput("set_par")
  )
  server <- function(input, output, session) {

    project_df <- projectServer("project", projects_df)
    set_par <- setParServer("set_par", project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname setupApp
setParServer <- function(id, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$class_input <- shiny::renderUI({
      shiny::req(project_df())
      choices <- project_classes(project_df())
      if(is.null(selected <- input$class))
        selected <- NULL
      shiny::selectInput(ns("class"), "Phenotype Class",
        choices = as.list(choices), selected = selected, multiple = TRUE)
    })

    output$show_set_par <- shiny::renderUI({
      shiny::renderText(paste("phenotype class: ", input$class))
    })
    ## Return.
    input
  })
}
#' @export
#' @rdname setParApp
setParInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("class_input")) # class
}
#' @export
#' @rdname setParApp
setParOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("show_set_par"))
}
#' Shiny Setup Parameters App
#'
#' @param id identifier for shiny reactive
#' @param peak_df,pmap_obj,covar,projects_info reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @export
#' @importFrom dplyr distinct filter 
#' @importFrom tidyr unite
#' @importFrom shiny moduleServer NS observeEvent reactive renderText
#'             renderUI req selectInput uiOutput updateSelectInput
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
        selected <- choices[1]
      shiny::selectInput(ns("class"), "Phenotype Class",
        choices = as.list(choices), selected = selected, multiple = TRUE)
    })
    shiny::observeEvent(shiny::req(project_df()), {
      choices <- project_classes(project_df())
      shiny::updateSelectInput(session, "class",
        choices = choices, selected = choices[1])
    })
    project_class_df <- shiny::reactive({
      shiny::req(project_df(), input$class)
      tidyr::unite(
        dplyr::distinct(
          dplyr::filter(project_peaks(project_df()),
                        .data$class %in% input$class),
          subjects, covars),
        subject_covar)
    })
    output$subject_covar_input <- shiny::renderUI({
      shiny::req(project_class_df())
      choices <- project_class_df()$subject_covar
      if(is.null(selected <- input$subject_covar))
        selected <- choices[1]
      shiny::selectInput(ns("subject_covar"), "Subjects & Model",
        choices = choices, selected = selected, multiple = TRUE)
    })
    shiny::observeEvent(shiny::req(project_class_df()), {
      choices <- project_class_df()$subject_covar
      shiny::updateSelectInput(session, "subject_covar",
        choices = choices, selected = choices[1])
    })
    
    output$show_set_par <- shiny::renderUI({
      shiny::tagList(
        shiny::renderText(paste("phenotype class: ",
                                paste(input$class, collapse = ", "))),
        shiny::renderText(paste("subjects & model: ",
                                paste(input$subject_covar, collapse = ", ")))
      )
    })
    ## Return.
    input
  })
}
#' @export
#' @rdname setParApp
setParInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("class_input")),        # class
    shiny::uiOutput(ns("subject_covar_input")) # subject_covar
  )
}
#' @export
#' @rdname setParApp
setParOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("show_set_par"))
}
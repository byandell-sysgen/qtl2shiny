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
#'             renderUI req selectInput tagList uiOutput updateSelectInput
#' @importFrom bslib layout_columns page_sidebar sidebar
setParApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Setup Parameters",
    sidebar = bslib::sidebar(
      projectUI("project"),
      setParInput("set_par"), # class, subject_model
      setParUI("set_par")),   # window_Mbp
    setParOutput("set_par")
  )
  server <- function(input, output, session) {

    project_df <- projectServer("project", projects_df)
    set_par <- setParServer("set_par", project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname setParApp
setParServer <- function(id, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$class_input <- shiny::renderUI({
      shiny::req(project_df())
      choices <- project_classes(project_df())
      if(is.null(selected <- input$class))
        selected <- choices[1]
      shiny::selectInput(ns("class"), "Class",
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
        subject_model)
    })
    output$subject_model_input <- shiny::renderUI({
      shiny::req(project_class_df())
      choices <- project_class_df()$subject_model
      if(is.null(selected <- input$subject_model))
        selected <- choices[1]
      shiny::selectInput(ns("subject_model"), "Subject_Model",
        choices = choices, selected = selected, multiple = TRUE)
    })
    shiny::observeEvent(shiny::req(project_class_df()), {
      choices <- project_class_df()$subject_model
      shiny::updateSelectInput(session, "subject_model",
        choices = choices, selected = choices[1])
    })
    
    ## Window numeric
    output$window_Mbp_input <- shiny::renderUI({
      shiny::req(project_df())
      if(is.null(win <- input$window_Mbp))
        win <- 1
      shiny::numericInput(ns("window_Mbp"), "width",
                          win, 0.1, 100)
    })
    
    output$show_set_par <- shiny::renderUI({
      shiny::tagList(
        shiny::renderText(paste("phenotype class: ",
                                paste(input$class, collapse = ", "))),
        shiny::renderText(paste("subjects & model: ",
                                paste(input$subject_model, collapse = ", "))),
        shiny::renderText(paste("window width (Mbp):", input$window_Mbp))
      )
    })
    ## Return.
    input
  })
}
#' @export
#' @rdname setParApp
setParInput <- function(id) {                  # class, subject_model
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("class_input")),        # class
    shiny::uiOutput(ns("subject_model_input")) # subject_model
  )
}
#' @export
#' @rdname setParApp
setParUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("window_Mbp_input"))      # window_Mbp
}
#' @export
#' @rdname setParApp
setParOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("show_set_par"))
}
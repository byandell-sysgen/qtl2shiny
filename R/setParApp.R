#' Shiny Setup Parameters App
#'
#' Shiny module for phenotype selection, with interfaces \code{setupInput} and  \code{setupUI}.
#'
#' @param id identifier for shiny reactive
#' @param peak_df,pmap_obj,analyses_df,covar,projects_info reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom dplyr filter 
#' @importFrom shiny checkboxInput moduleServer NS observeEvent reactive
#'             renderText renderUI req tagList uiOutput
#' @importFrom bslib page_sidebar sidebar
setParApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Setup Parameters",
    sidebar = bslib::sidebar(
      projectUI("project"),
      setParInput("set_par")),
    setParOutput("set_par")
  )
  server <- function(input, output, session) {
    analyses_df <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "analyses")
    })

    project_df <- projectServer("project", projects_df)
    set_par <- setParServer("set_par", analyses_df, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname setupApp
setParServer <- function(id, analyses_df, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Input `input$pheno_group`.
    output$pheno_group_input <- shiny::renderUI({
      shiny::req(analyses_df())
      choices <- sort(unique(analyses_df()$pheno_group))
      if(is.null(selected <- input$pheno_group))
        selected <- choices[1]
      shiny::selectInput(ns("pheno_group"), "",
        choices = as.list(choices), selected = selected, multiple = TRUE)
    })
    
    # Input `input$dataset`.
    pheno_type <- shiny::reactive({
      # Find `pheno_type`s for `dataset` choice.
      phe_gp <- shiny::req(input$pheno_group)
      shiny::req(analyses_df())
      analyses_group <- dplyr::filter(analyses_df(), pheno_group %in% phe_gp)
      sort(unique(analyses_group$pheno_type))
    })
    output$dataset_input <- shiny::renderUI({
      shiny::req(project_df())
      choices <- c("all", shiny::req(pheno_type()))
      if(is.null(selected <- input$dataset))
        selected <- NULL
      shiny::selectInput(ns("dataset"), "Phenotype Set",
        choices = as.list(choices), selected = selected, multiple = TRUE)
    })
    shiny::observeEvent(input$pheno_group, {
      choices <- c("all", shiny::req(pheno_type()))
      shiny::updateSelectInput(session, "dataset",
        choices = choices, selected = NULL)
    })
    
    output$show_set_par <- shiny::renderUI({
      shiny::tagList(
        shiny::renderText(paste("pheno_group:", input$pheno_group)),
        shiny::renderText(paste("dataset: ", input$dataset))
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
    shiny::uiOutput(ns("pheno_group_input")),
    shiny::uiOutput(ns("dataset_input"))
  )
}
#' @export
#' @rdname setParApp
setParOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("show_set_par"))
}
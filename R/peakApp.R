#' Shiny Peak App
#'
#' Shiny module for peak selection.
#'
#' @param id identifier for shiny reactive
#' @param set_par,peak_df,pmap_obj,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom shiny  column moduleServer NS reactive req
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom bslib page_sidebar sidebar
peakApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Peak Read",
    sidebar = bslib::sidebar(
      projectUI("project"),
      setParInput("set_par")),
    peakOutput("peak_df")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname peakApp
peakServer <- function(id, set_par, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    peak_df <- shiny::reactive({
      class <- shiny::req(set_par$class)
      subject_model <- set_par$subject_model
      if(!shiny::isTruthy(subject_model)) subject_model <- NULL
      
      out <- read_peaks(shiny::req(project_df()), class,
                        subject_model = subject_model)
      out
    })
    
    # Output Peak Table.
    output$peak_table <- DT::renderDataTable({
      peakDataTable(peak_df())
    }, options = list(scrollX = TRUE, pageLength = 5,
                      lengthMenu = c(5,10,25)))
    
    ## Return.
    peak_df
  })
}
#' @export
#' @rdname peakApp
peakOutput <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("peak_table")) # peak_table
}

#' Shiny Hotspot Table App
#'
#' Shiny module to select hotspots for peak selection.
#'
#' @param id identifier for shiny reactive
#' @param hotspot_obj reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @return list of inputs and scan summary
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom dplyr arrange desc distinct filter
#' @importFrom shiny column fluidRow isTruthy moduleServer
#'             numericInput observeEvent reactive renderPlot renderTable
#'             renderUI req selectInput setProgress tableOutput
#'             uiOutput updateNumericInput updateSelectInput withProgress
#' @importFrom DT dataTableOutput renderDataTable 
#' @importFrom rlang .data
#' @importFrom bslib page_sidebar sidebar
hotspotTableApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Hotspot",
    sidebar = bslib::sidebar(
      projectUI("project_df"),          # project
      setParInput("set_par"),           # class, subject_model 
      setParUI("set_par"),              # window_Mbp 
      hotspotDataInput("hotspot_obj")), # chr_ct, minLOD
    hotspotTableOutput("hotspot_df"),   # hotspot_table
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    hotspot_obj <- 
      hotspotDataServer("hotspot_obj", set_par, peak_df, pmap_obj, project_df)
    hotspot_df <- 
      hotspotTableServer("hotspot_df", hotspot_obj)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname hotspotTableApp
hotspotTableServer <- function(id, hotspot_obj) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    hotspot_table <- shiny::reactive({
      shiny::req(hotspot_obj())
      shiny::withProgress(message = 'Hotspot summary ...', value = 0, {
        shiny::setProgress(1)
        summary(hotspot_obj())
      })
    })
    output$hotspot_render_table <- DT::renderDataTable({
      shiny::req(hotspot_table())
     }, escape = FALSE,
    options = list(lengthMenu = c(5,10,20,50), pageLength = 5))

    ## Return.
    hotspot_table
  })
}
#' @export
#' @rdname hotspotTableApp
hotspotTableOutput <- function(id) { 
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("hotspot_render_table"))
}

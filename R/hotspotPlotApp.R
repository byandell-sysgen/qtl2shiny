#' Shiny Hotspot Plot App
#'
#' Shiny module to view hotspots for peak selection.
#'
#' @param id identifier for shiny reactive
#' @param set_par,hotspot_obj reactive arguments
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
hotspotPlotApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Hotspot",
    sidebar = bslib::sidebar(
      projectUI("project_df"),          # project
      setParInput("set_par"),           # class, subject_model 
      setParUI("set_par"),              # window_Mbp 
      hotspotDataInput("hotspot_obj")), # chr_ct, minLOD
    hotspotPlotOutput("hotspot_plot")   # hotspot_plot
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_read_df <- peakReadServer("peak_read_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    hotspot_obj<- 
      hotspotDataServer("hotspot_obj", set_par, peak_read_df, pmap_obj, project_df)
    hotspot_plot <- 
      hotspotPlotServer("hotspot_plot", set_par, hotspot_obj, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname hotspotPlotApp
hotspotPlotServer <- function(id, set_par, hotspot_obj) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    hotspot_plot <- shiny::reactive({
      shiny::req(hotspot_obj())
      window_Mbp <- shiny::req(set_par$window_Mbp)
      class <- shiny::req(set_par$class)
      shiny::withProgress(message = 'Hotspot render plot ...',
                          value = 0, {
                            shiny::setProgress(1)
                            plot_hot(hotspot_obj(), class, window_Mbp)
                          })
    })
    output$hotspot_render_plot <- shiny::renderPlot({
      print(shiny::req(hotspot_plot()))
    })

    ## Return.
    hotspot_plot
  })
}
#' @export
#' @rdname hotspotPlotApp
hotspotPlotOutput <- function(id) { 
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("hotspot_render_plot"))
}

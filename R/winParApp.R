#' Shiny Window Parameters App
#'
#' Shiny module for peak selection.
#'
#' @param id identifier for shiny reactive
#' @param hotspot_df,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@wisc.edu}
#' @keywords utilities
#'
#' @return list of inputs and scan summary
#'
#' @export
#' 
#' @importFrom dplyr filter
#' @importFrom shiny checkboxInput column fluidRow observeEvent moduleServer NS
#'             numericInput renderUI req selectInput strong tagList textInput
#'             uiOutput updateNumericInput updateSelectInput
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom rlang .data
#' @importFrom bslib layout_columns page_sidebar sidebar
winParApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Window Parameters",
    sidebar = bslib::sidebar(
      projectUI("project"),     # project
      setParInput("set_par"),   # class, subject_model
      bslib::layout_columns(
        col_widths = c(6, 4),
        winParInput("win_par"),  # hotspot
        setParUI("set_par")      # window_Mbp 
      ),
      winParUI("win_par"),      # local
      hotspotInput("hotspot")), # chr_ct, minLOD
    winParOutput("win_par")     # peak_table
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    hotspot_df <-
      hotspotServer("hotspot", set_par, peak_df, pmap_obj, project_df)
    win_par <- winParServer("win_par", hotspot_df, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname winParApp
winParServer <- function(id, hotspot_df, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observeEvent(project_df(), {
      choices <- shiny::req(hotspot_df())$chr_pos
      shiny::updateSelectInput(session, "hotspot",
                               choices = choices,
                               selected = NULL)
    })

    # Select chromosome. Defaults to blank.
    output$hotspot_input <- shiny::renderUI({
      shiny::req(project_df())
      choices <- shiny::req(hotspot_df())$chr_pos
      shiny::selectInput(ns("hotspot"), "hotspot",
        choices = choices, selected = input$hotspot)
    })

    # Output Hotspot Table.
    output$hot_table <- DT::renderDataTable({
      shiny::req(hotspot_df())
    }, options = list(scrollX = TRUE, pageLength = 5,
                      lengthMenu = c(5,10,25)))
    
    ## Return.
    input
  })
}
#' @export
#' @rdname winParApp
winParInput <- function(id) { # local, chr_id, peak_Mbp
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("hotspot_input"))
}
#' @export
#' @rdname winParApp
winParUI <- function(id) { # local, chr_id, peak_Mbp
  ns <- shiny::NS(id)
  shiny::checkboxInput(ns("local"), "Local Peaks in Window?", TRUE)
}
#' @export
#' @rdname winParApp
winParOutput <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("hot_table")) # peak_table
}

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
#' @importFrom bslib card layout_columns page_sidebar sidebar
winParApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Window Parameters",
    sidebar = bslib::sidebar(
      bslib::card(
        projectUI("project_df"),         # project
        setParInput("set_par")),         # class, subject_model
      bslib::card(
        bslib::layout_columns(
          col_widths = c(6, 4),
          winParInput("win_par"),        # hotspot
          setParUI("set_par")),          # window_Mbp 
        hotspotDataInput("hotspot_obj")),    # chr_ct, minLOD
      width = 400),
    bslib::card(
      hotspotTableOutput("hotspot_df")), # hotspot_table
    winParOutput("win_par")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_read_df <- peakReadServer("peak_read_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    hotspot_obj <- 
      hotspotDataServer("hotspot_obj", set_par, peak_read_df, pmap_obj, project_df)
    hotspot_df <- 
      hotspotTableServer("hotspot_df", hotspot_obj)
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
      choices <- shiny::req(hotspot_df())$hotspot
      shiny::updateSelectInput(session, "hotspot",
                               choices = choices,
                               selected = NULL)
    })

    # Select chromosome. Defaults to blank.
    output$hotspot_code_input <- shiny::renderUI({
      shiny::req(project_df())
      choices <- shiny::req(hotspot_df())$hotspot
      selected <- input$hotspot
      if(!shiny::isTruthy(selected))
        selected <- choices[1]
      shiny::selectInput(ns("hotspot_code"), "hotspot",
        choices = choices, selected = selected, multiple = TRUE)
    })

    output$win_par_output <- shiny::renderUI({
      win_par <- shiny::req(win_par())
      shiny::tagList(
        shiny::renderText(paste("chr_id: ",
          paste(win_par$chr_id, collapse = ", "))),
        shiny::renderText(paste("peak_Mbp: ",
          paste(win_par$peak_Mbp, collapse = ", "))),
        shiny::renderText(paste("window_Mbp: ", win_par$window_Mbp)),
        shiny::renderText(paste("count_hotspot: ",
          paste(win_par$count_hotspot, collapse = ", ")))
      )
    })
    
    win_par <- shiny::reactive({
      decode_hotspot(shiny::req(input$hotspot_code), shiny::req(hotspot_df()))
    })
    ## Return.
    win_par
  })
}
#' @export
#' @rdname winParApp
winParInput <- function(id) { # chr_id, peak_Mbp
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("hotspot_code_input"))
}
#' @export
#' @rdname winParApp
winParOutput <- function(id) { # chr_id, peak_Mbp
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("win_par_output"))
  )
}

#' Shiny Phenotype App
#'
#' Shiny module for peak selection.
#'
#' @param id identifier for shiny reactive
#' @param set_par,peak_project_df,pmap_obj,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom shiny  column moduleServer NS reactive req
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom bslib layout_columns page_sidebar sidebar
peakPanelApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Pheno Read",
    sidebar = bslib::sidebar(
      projectUI("project_df"),           # project
      setParInput("set_par"),            # class, subject_model
      peakPanelInput("peak_df"), # filter
      bslib::layout_columns(
        col_widths = c(6, 4),
        winParInput("win_par"),          # hotspot
        setParUI("set_par")              # window_Mbp 
      ),
      hotspotDataInput("hotspot_obj")    # chr_ct, minLOD
    ),
    peakPanelOutput("peak_df")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_project_df <- peakServer("peak_project_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    hotspot_obj <- hotspotDataServer("hotspot_obj", set_par, peak_project_df,
                                     pmap_obj, project_df)
    hotspot_df <- 
      hotspotTableServer("hotspot_df", hotspot_obj)
    win_par <- winParServer("win_par", hotspot_df, project_df)
    peak_df <- peakPanelServer("peak_df", set_par, win_par,
                               peak_project_df, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname peakPanelApp
peakPanelServer <- function(id, set_par, win_par, peak_project_df, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filter peaks to hotspots.
    output$filter <- shiny::renderUI({
      shiny::checkboxInput(ns("filter"),
                           "Skip Filter by hotspot(s)?", FALSE)
    })
    
    peak_df <- shiny::reactive({
      shiny::req(project_df(), peak_project_df(), win_par())
      chr_id <- win_par()$chr_id
      peak_Mbp <- win_par()$peak_Mbp
      window_Mbp <- shiny::req(set_par$window_Mbp)
      peaks_in_pos(peak_project_df(), !shiny::isTruthy(input$filter),
                   chr_id, peak_Mbp, window_Mbp)
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
#' @rdname peakPanelApp
peakPanelInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("filter"))               # filter
}
#' @export
#' @rdname peakApp
peakPanelOutput <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("peak_table")) # peak_table
}

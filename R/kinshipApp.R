#' Shiny Kinship App
#'
#' @param id identifier for shiny reactive
#' @param win_par,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom shiny moduleServer NS reactive renderTable req
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom bslib page_sidebar sidebar
kinshipApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Kinship Read",
    sidebar = bslib::sidebar(
      projectUI("project_df"),  # project
      setParInput("set_par"),   # class, subject_model
      winParInput("win_par"),   # chr_id, peak_Mbp
      hotspotInput("hotspot")), # chr_ct, minLOD
    kinshipOutput("kinship_list")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    hotspot_df <- 
      hotspotServer("hotspot", set_par, peak_df, pmap_obj, project_df)
    win_par <-
      winParServer("win_par", set_par, peak_df, pmap_obj, hotspot_df, project_df)
    kinship_list <- kinshipServer("kinship_list", win_par, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname kinshipApp
kinshipServer <- function(id, win_par, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    chr_id <- shiny::reactive(shiny::req(win_par())$chr_id)
    kinship_list <- shiny::reactive({
      shiny::req(project_df(), chr_id())
      read_project(project_df(), "kinship")[chr_id()]
    })

    # Output Kinship Size.
    output$kinship_size <- shiny::renderUI({
      shiny::renderText(paste(
        "kinship for chr", shiny::req(chr_id()),
        "is size", sapply(shiny::req(kinship_list()), nrow)))
    })
    
    ## Return.
    kinship_list
  })
}
#' @export
#' @rdname kinshipApp
kinshipOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("kinship_size"))
}

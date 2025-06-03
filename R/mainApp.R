#' Shiny app for qtl2
#'
#' Run shiny app for qtl2 data.
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @details 
#' See qtl2shinyData vignette for data setup.
#' 
#' @param id identifier for shiny module
#' @param projects static data frame with project information
#' @importFrom shiny icon includeMarkdown moduleServer NS reactive shinyApp tags
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#'             dashboardBody menuItem sidebarMenu tabItem tabItems
#' @export
mainApp <- function() {
  projects <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  
  ui <-   shinydashboard::dashboardPage(skin="red",
    shinydashboard::dashboardHeader(title = "qtl2shiny"),
    shinydashboard::dashboardSidebar(mainInput("main")),
    shinydashboard::dashboardBody(mainOutput("main"))
  )
  server <- function(input, output, session) {
    mainServer("main", projects)
  }
  shiny::shinyApp(ui, server)
}
#' @rdname mainApp
#' @export
mainServer <- function(id, projects) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    projects_info <- shiny::reactive({projects})
    dashServer("dash", projects_info)
  })
}
#' @rdname mainApp
#' @export
mainInput <- function(id) {
  ns <- shiny::NS(id)
  shinydashboard::sidebarMenu(
    dashInput(ns("dash")),
    shinydashboard::menuItem(
      "Phenotypes and Region",
      tabName = "phenos",
      icon = shiny::icon("dashboard", verify_fa = FALSE)),
    shinydashboard::menuItem(
      "Haplotype Scans",
      tabName = "hap_scan",
      icon = shiny::icon("dashboard", verify_fa = FALSE)),
    shinydashboard::menuItem(
      "SNP/Gene Action",
      tabName = "dip_scan",
      icon = shiny::icon("dashboard", verify_fa = FALSE)),
    shiny::tags$div(
      id = "popup",
      helpPopup(
        "qtl2shiny help",
        shiny::includeMarkdown(system.file(file.path("qtl2shinyApp", "about.md"), package='qtl2shiny')),
        placement = "right", trigger = "click"))
  )
}
#' @rdname mainApp
#' @export
mainOutput <- function(id) {
  ns <- shiny::NS(id)
  shinydashboard::tabItems(
    ## Phenotypes and Region
    shinydashboard::tabItem("phenos", dashUI(ns("dash"))),
    ## Scans
    shinydashboard::tabItem("hap_scan", dashOutput(ns("dash"))),
    ## Diploid Analysis
    shinydashboard::tabItem("dip_scan", dashOutput2(ns("dash")))
  )
}

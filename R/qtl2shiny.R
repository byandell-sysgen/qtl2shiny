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
#' @importFrom shiny includeMarkdown, moduleServer, NS, reactive, shinyApp
#' @importFrom shinydashboard ashboardPage, dashboardHeader, dashboardSidebar,
#'             dashboardBody, menuItem, sidebarMenu, tabItem, tabItems
#' @export
qtl2shinyServer <- function(id, projects) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    projects_info <- shiny::reactive({projects})
    qtl2dashServer("qtl2dash", projects_info)
  })
}
#' @rdname qtl2shinyServer
#' @export
qtl2shinyInput <- function(id) {
  ns <- shiny::NS(id)
  shinydashboard::sidebarMenu(
    qtl2dashInput(ns("qtl2shiny")),
    shinydashboard::menuItem(
      "Phenotypes and Region",
      tabName = "phenos",
      icon = icon("dashboard", verify_fa = FALSE)),
    shinydashboard::menuItem(
      "Haplotype Scans",
      tabName = "hap_scan",
      icon = icon("dashboard", verify_fa = FALSE)),
    shinydashboard::menuItem(
      "SNP/Gene Action",
      tabName = "dip_scan",
      icon = icon("dashboard", verify_fa = FALSE)),
    tags$div(
      id = "popup",
      helpPopup(
        "qtl2shiny help",
        shiny::includeMarkdown(system.file(file.path("qtl2shinyApp", "about.md"), package='qtl2shiny')),
        placement = "right", trigger = "click"))
  )
}
#' @rdname qtl2shinyServer
#' @export
qtl2shinyOutput <- function(id) {
  ns <- shiny::NS(id)
  shinydashboard::tabItems(
    ## Phenotypes and Region
    shinydashboard::tabItem("phenos", qtl2dashUI(ns("qtl2dash"))),
    ## Scans
    shinydashboard::tabItem("hap_scan", qtl2dashOutput(ns("qtl2dash"))),
    ## Diploid Analysis
    shinydashboard::tabItem("dip_scan", qtl2dashOutput2(ns("qtl2dash")))
  )
}
#' @rdname qtl2shinyServer
#' @export
qtl2shinyApp <- function() {
  projects <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  
  ui <-   shinydashboard::dashboardPage(skin="red",
    shinydashboard::dashboardHeader(title = "qtl2shiny"),
    shinydashboard::dashboardSidebar(qtl2shinyInput("qtl2shiny")),
    shinydashboard::dashboardBody(qtl2shinyOutput("qtl2shiny"))
  )
  server <- function(input, output, session) {
    qtl2shinyServer("qtl2shiny", projects)
  }
  shiny::shinyApp(ui, server)
}

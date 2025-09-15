#' Shiny Diplotype Parameter module
#'
#' @param id identifier for shiny reactive
#' @param win_par,phe_mx,cov_df,K_chr,analyses_df,project_df,allele_info reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom shiny mainPanel moduleServer NS radioButtons reactive renderText
#'             renderUI req selectInput sidebarPanel strong tagList textOutput
#'             uiOutput
dipParApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Diplo Parameters",
    sidebar = bslib::sidebar(
      projectUI("project_df"), # project
      dipParInput("dip_par"),  # snp_action
      dipParUI("dip_par")),    # allele_names
    dipParOutput("dip_par")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    dipParServer("dip_par", hotspot_list)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname dipParApp
dipParServer <- function(id, hotspot_list) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$snp_action_input <- shiny::renderUI({
      shiny::selectInput(ns("snp_action"), "",
                         c("add+dom","additive","non-add",
                           "recessive","dominant"),
                         input$snp_action)
    })
    output$sex_type_input <- shiny::renderUI({
      choices <- c("A","I","F","M")
      shiny::radioButtons(ns("sex_type"), "Sex:",
                          choices,
                          input$sex_type, inline = TRUE)
    })
    
    output$show_dip_par <- shiny::renderUI({
      shiny::renderText(paste("snp_action: ", input$snp_action))
    })
    
    output$allele_names <- shiny::renderText({
      allele_info <- shiny::req(hotspot_list$allele_info())
      paste(allele_info$code, allele_info$shortname, sep = "=", collapse = ", ")
    })
    
    # Return.
    input
  })
}
#' @export
#' @rdname dipParApp
dipParInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("snp_action_input")) # snp_action
}
#' @export
#' @rdname dipParApp
dipParUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::textOutput(ns("allele_names"))   # allele_names
}
#' @export
#' @rdname dipParApp
dipParOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("show_dip_par"))
}

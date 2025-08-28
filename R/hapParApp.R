#' Shiny Haplotype Analysis App
#'
#' Shiny module for analysis based on haplotype alleles, with interface \code{haploUI}.
#'
#' @param id identifier for shiny reactive
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom shiny moduleServer NS radioButtons renderText renderUI
#'             req uiOutput
#' @importFrom bslib page_sidebar sidebar
hapParApp <- function() {
  ui <- bslib::page_sidebar(
    title =  "Test Haplo Parameters",
    sidebar = bslib::sidebar(
      hapParInput("hap_par"), # sex_type
      hapParUI("hap_par")),   # button
    hapParOutput("hap_par")
  )
  server <- function(input, output, session) {
    hapParServer("hap_par")
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname hapParApp
hapParServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$button_input <- shiny::renderUI({
      shiny::radioButtons(ns("button"), "",
        c("Genome Scans","SNP Association","Allele Pattern","Mediation"),
        input$button)
    })
    output$sex_type_input <- shiny::renderUI({
      choices <- c("A","I","F","M")
      shiny::radioButtons(ns("sex_type"), "Sex:", choices,
        input$sex_type, inline = TRUE)
    })
    
    output$show_hap_par <- shiny::renderUI({
      shiny::tagList(
        shiny::renderText(paste("sex_type: ", input$sex_type)),
        shiny::renderText(paste("button: ", input$button))
      )
    })
    ## Return.
    input
  })
}
#' @export
#' @rdname hapParApp
hapParInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("sex_type_input")) # sex_type
}
#' @export
#' @rdname hapParApp
hapParUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("button_input"))   # button
}
#' @export
#' @rdname hapParApp
hapParOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("show_hap_par"))
}

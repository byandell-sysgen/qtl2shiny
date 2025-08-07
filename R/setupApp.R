#' Shiny setup module
#'
#' Shiny module for phenotype selection, with interfaces \code{setupInput} and  \code{setupUI}.
#'
#' @param id identifier for shiny reactive
#' @param set_par,peak_df,pmap_obj,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom dplyr filter 
#' @importFrom shiny checkboxInput isTruthy moduleServer NS
#'             observeEvent radioButtons reactive renderText renderUI req
#'             strong tagList textOutput uiOutput
#' @importFrom bslib page_sidebar sidebar
setupApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Setup",
    sidebar = bslib::sidebar(
      projectUI("project"),
      setParInput("set_par"),
      setupInput("setup"),
      setupUI("setup")),
    shiny::uiOutput("pheno_names"),
    setupOutput("setup")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project", projects_df)
    set_par <- setParServer("set_par", project_df)
    
    pmap_obj <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "pmap")
    })
    peak_df <- shiny::reactive({
      shiny::req(project_df(), set_par$class)
      read_project(project_df(), "peaks", class = set_par$class)
    })

    set_list <- setupServer("setup", set_par, peak_df, pmap_obj, project_df)

    output$pheno_names <- shiny::renderUI({
      paste("pheno_names: ", paste(set_list$pheno_names(), collapse = ", "))
    })
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname setupApp
setupServer <- function(id, set_par, peak_df, pmap_obj, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Find Hotspots.    
    hotspot_df <- hotspotServer("hotspot", set_par, peak_df, pmap_obj,
                                project_df)
    ## Locate Peak.
    win_par <- peakServer("peak", set_par, peak_df, pmap_obj, hotspot_df,
                          project_df)

    chr_pos <- shiny::reactive({
      shiny::req(project_df())
      make_chr_pos(win_par$chr_id, 
                   win_par$peak_Mbp, win_par$window_Mbp)
    })
    output$chr_pos <- shiny::renderText({
      paste0("Region: ", chr_pos(), "Mbp")
    })
    output$version <- shiny::renderText({
      versions()
    })
    
    ## Use window as input to phenoServer.
    pheno_names <- phenoServer("pheno", set_par, win_par, peak_df,
                               project_df)
    
    ## Setup input logic.
    output$sidebar_setup <- shiny::renderUI({
      switch(shiny::req(input$radio),
             Phenotypes = shiny::tagList(
               shiny::uiOutput(ns("filter")),
               phenoInput(ns("pheno"))),
             Region     = peakInput(ns("peak"))) # local, chr_id, peak_Mbp, window_Mbp
    })
    output$sidebar_hot <- shiny::renderUI({
      switch(shiny::req(input$radio),
             Region     = hotspotInput(ns("hotspot"))) # chr_ct, minLOD, window_Mbp
    })
    output$main_setup <- shiny::renderUI({
      switch(shiny::req(input$radio),
             Phenotypes = phenoOutput(ns("pheno")),
             Region     = shiny::tagList(
               peakOutput(ns("peak"))#,
             #  hotspotOutput(ns("hotspot"))
             )
       ) # peak_table, hotspot_plot, hotspot_table
    })
    
    output$radio_input <- shiny::renderUI({
      shiny::radioButtons(ns("radio"), NULL,
                          c("Region", "Phenotypes"),
                          input$radio,
                          inline=TRUE)
    })
    
    ## Return.
    shiny::reactiveValues(
      pheno_names = pheno_names,
      win_par = win_par)
  })
}
#' @export
#' @rdname setupApp
setupInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("chr_pos"))
}
#' @export
#' @rdname setupApp
setupUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("radio_input")),
    shiny::uiOutput(ns("sidebar_setup")),
    shiny::uiOutput(ns("pheno_group_input")),
    shiny::uiOutput(ns("dataset_input")),
    shiny::uiOutput(ns("sidebar_hot")),
    shiny::uiOutput(ns("version"))
  )
}
#' @export
#' @rdname setupApp
setupOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("main_setup"))
}
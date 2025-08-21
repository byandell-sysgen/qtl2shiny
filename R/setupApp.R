#' Shiny Setup App
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
      projectUI("project_df"), # project
      setParInput("set_par"),  # class, subject_model
      setupInput("set_list"),  # pheno_names, chr_pos
      setupUI("set_list")),    # radio, local, win_par, chr_ct, minLOD
    setupOutput("set_list")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    set_list <- setupServer("set_list", set_par, peak_df, pmap_obj, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname setupApp
setupServer <- function(id, set_par, peak_df, pmap_obj, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Modules.    
    hotspot_df <- 
      hotspotServer("hotspot", set_par, peak_df, pmap_obj, project_df)
    win_par <-
      winParServer("win_par", set_par, peak_df, pmap_obj, hotspot_df, project_df)
    pheno_names <-
      phenoNamesServer("pheno_names", set_par, win_par, peak_df, project_df)
    pheno_mx <- phenoServer("pheno_mx", set_par, pheno_names, project_df)
    covar_df <- shiny::reactive({
      shiny::req(project_df(), pheno_mx())
      read_project(project_df(), "covar")
    })
    phenoPlotServer("pheno_plot", pheno_names, pheno_mx, covar_df)
    
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
    
    ## Setup input logic.
    output$sidebar_setup <- shiny::renderUI({
      switch(shiny::req(input$radio),
             Phenotypes = shiny::uiOutput(ns("filter")),
             Region     = winParInput(ns("win_par"))) # local, chr_id, peak_Mbp, window_Mbp
    })
    output$sidebar_hot <- shiny::renderUI({
      switch(shiny::req(input$radio),
             Region     = hotspotInput(ns("hotspot"))) # chr_ct, minLOD, window_Mbp
    })
    output$main_setup <- shiny::renderUI({
      switch(shiny::req(input$radio), # peak_table, hotspot_plot, hotspot_table
             Phenotypes = {
               shiny::tagList(
                 phenoNamesOutput(ns("pheno_names")),
                 phenoPlotOutput(ns("pheno_plot")))
             },
             Region     = winParOutput(ns("win_par")))
    })
    
    output$radio_input <- shiny::renderUI({
      shiny::radioButtons(ns("radio"), NULL,
                          c("Region", "Phenotypes"), input$radio, inline=TRUE)
    })
    
    ## Return.
    shiny::reactiveValues(
      pheno_names = pheno_names,
      win_par = win_par)
  })
}
#' @export
#' @rdname setupApp
setupInput <- function(id) {             # pheno_names, chr_pos
  ns <- shiny::NS(id)
  shiny::tagList(
    phenoNamesInput(ns("pheno_names")),  # pheno_names
    shiny::uiOutput(ns("chr_pos"))       # chr_pos
  )
}
#' @export
#' @rdname setupApp
setupUI <- function(id) {                # radio, local, chr_id, peak_Mbp, window_Mbp, chr_ct, minLOD, window_Mbp
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("radio_input")),   # radio
    shiny::uiOutput(ns("sidebar_setup")), # local, chr_id, peak_Mbp, window_Mbp
    shiny::uiOutput(ns("sidebar_hot"))#,  # chr_ct, minLOD, window_Mbp
    #shiny::uiOutput(ns("version"))
  )
}
#' @export
#' @rdname setupApp
setupOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("main_setup"))
}
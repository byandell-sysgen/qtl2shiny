#' Shiny setup module
#'
#' Shiny module for phenotype selection, with interfaces \code{setupInput} and  \code{setupUI}.
#'
#' @param id identifier for shiny reactive
#' @param peak_df,pmap_obj,analyses_df,covar,projects_info reactive arguments
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
      setupInput("setup"),
      projectUI("project"),
      setupUI("setup")),
    shiny::uiOutput("set_par"),
    setupOutput("setup")
  )
  server <- function(input, output, session) {
    peak_df <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "peaks")
    })
    pmap_obj <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "pmap")
    })
    analyses_df <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "analyses")
    })
    covar <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "covar")
    })

    project_df <- projectServer("project", projects_df)
    set_par <- setupServer("setup", peak_df, pmap_obj, analyses_df, covar,
                           project_df)

    output$set_par <- shiny::renderUI({
      paste("pheno_names: ", paste(set_par$pheno_names(), collapse = ", "))
    })
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname setupApp
setupServer <- function(id, peak_df, pmap_obj, analyses_df, covar,
                        project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Input `input$pheno_group`.
    output$pheno_group_input <- shiny::renderUI({
      shiny::req(analyses_df())
      choices <- sort(unique(analyses_df()$pheno_group))
      if(is.null(selected <- input$pheno_group))
        selected <- choices[1]
      shiny::selectInput(ns("pheno_group"), "",
        choices = as.list(choices), selected = selected, multiple = TRUE)
    })
    
    # Input `input$dataset`.
    pheno_type <- shiny::reactive({
      # Find `pheno_type`s for `dataset` choice.
      phe_gp <- shiny::req(input$pheno_group)
      shiny::req(analyses_df())
      analyses_group <- dplyr::filter(analyses_df(), pheno_group %in% phe_gp)
      sort(unique(analyses_group$pheno_type))
    })
    output$dataset_input <- shiny::renderUI({
      shiny::req(project_df())
      choices <- c("all", shiny::req(pheno_type()))
      if(is.null(selected <- input$dataset))
        selected <- NULL
      shiny::selectInput(ns("dataset"), "Phenotype Set",
        choices = as.list(choices), selected = selected, multiple = TRUE)
    })
    
    ## Set up analyses data frame.
    analyses_set <- shiny::reactive({
      shiny::req(project_df(), analyses_df())
      set_analyses(input$dataset, input$pheno_group, analyses_df())
    })
    # Restrict peaks to region.
    peak_dataset_df <- shiny::reactive({
      shiny::req(project_df(), analyses_set(), peak_df())
      chr_id <- shiny::req(win_par$chr_id)
      peak_Mbp <- shiny::req(win_par$peak_Mbp)
      window_Mbp <- shiny::req(win_par$window_Mbp)
      peaks_in_pos(analyses_set(), peak_df(),
                   shiny::isTruthy(input$filter),
                   chr_id, peak_Mbp, window_Mbp)
    })
    output$filter <- shiny::renderUI({
      shiny::checkboxInput(ns("filter"),
                           paste0("Peak on chr ", win_par$chr_id, " in ",
                                  paste(win_par$peak_Mbp + c(-1,1) * win_par$window_Mbp,
                                        collapse = "-"), "?"),
                           TRUE)
    })

    ## Find Hotspots.    
    hotspot_df <- hotspotServer("hotspot", input, peak_df, pmap_obj,
                                project_df)
    ## Locate Peak.
    win_par <- peakServer("peak", input, peak_df, pmap_obj, hotspot_df,
                          project_df)

    chr_pos <- shiny::reactive({
      shiny::req(project_df())
      make_chr_pos(win_par$chr_id, 
                   win_par$peak_Mbp, win_par$window_Mbp)
    })
    output$chr_pos <- shiny::renderText({
      paste0("Region: ", chr_pos(), "Mbp")
    })
    output$num_pheno <- shiny::renderText({
      shiny::req(project_df())
      num_pheno(character(), analyses_df())
    })
    output$version <- shiny::renderText({
      versions()
    })
    
    ## Use window as input to phenoServer.
    pheno_names <- phenoServer("pheno", input, win_par,
      peak_df, analyses_df, cov_df, project_df)
    
    ## Setup input logic.
    output$project_name <- renderUI({
      shiny::strong(paste("Project:", 
                          shiny::req(project_df()$project),
                          "\n"))
    })
    output$sidebar_setup <- shiny::renderUI({
      switch(shiny::req(input$radio),
             Phenotypes = shiny::tagList(
               shiny::uiOutput(ns("filter")),
               phenoInput(ns("pheno")),
               phenoUI(ns("pheno"))),
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
  shiny::tagList(
    shiny::uiOutput(ns("project_name")),
    shiny::textOutput(ns("num_pheno")),
    shiny::uiOutput(ns("chr_pos"))
  )
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
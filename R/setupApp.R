#' Shiny setup module
#'
#' Shiny module for phenotype selection, with interfaces \code{setupInput} and  \code{setupUI}.
#'
#' @param id identifier for shiny reactive
#' @param peak_df,pmap_obj,analyses_tbl,cov_df,projects_info reactive arguments
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
  projects <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
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
    projects_info <- shiny::reactive({projects})

    peak_df <- shiny::reactive({
      shiny::req(project_info())
      read_project(project_info(), "peaks")
    })
    pmap_obj <- shiny::reactive({
      shiny::req(project_info())
      read_project(project_info(), "pmap")
    })
    analyses_tbl <- shiny::reactive({
      shiny::req(project_info())
      ## The analyses_tbl should only have one row per pheno.
      read_project(project_info(), "analyses")
    })
    covar <- shiny::reactive({
      shiny::req(project_info())
      read_project(project_info(), "covar")
    })
    analyses_df <- shiny::reactive({
      phename <- set_par$pheno_names()
      if(is.null(phename)) return(NULL)
      dplyr::filter(analyses_tbl(), .data$pheno %in% phename)
    })
    cov_df <- shiny::reactive({
      analyses <- analyses_df() 
      if(is.null(analyses)) return(NULL)
      qtl2mediate::get_covar(covar(), analyses_df())
    })
    
    project_info <- projectServer("project", projects_info)
    set_par <- setupServer("setup", peak_df, pmap_obj, analyses_tbl, cov_df,
                           project_info)

    output$set_par <- shiny::renderUI({
      paste("pheno_names: ", paste(set_par$pheno_names(), collapse = ", "))
    })
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname setupApp
setupServer <- function(id, peak_df, pmap_obj, analyses_tbl, 
                       cov_df, project_info) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Input `input$pheno_group`.
    pheno_group <- shiny::reactive({
      shiny::req(project_info())
      sort(unique(shiny::req(analyses_tbl())$pheno_group))
    }, label = "pheno_group")
    output$pheno_group_input <- shiny::renderUI({
      shiny::req(choices <- pheno_group())
      if(is.null(selected <- input$pheno_group)) {
        selected <- choices[1]
      }
      shiny::selectInput(ns("pheno_group"), "",
                         choices = as.list(choices),
                         selected = selected,
                         multiple = TRUE)
    })
    
    ## Reactive `pheno_type()`.
    pheno_type <- shiny::reactive({
      shiny::req(project_info())
      phe_gp <- shiny::req(input$pheno_group)
      analyses_group <- 
        dplyr::filter(
          shiny::req(analyses_tbl()),
          pheno_group %in% phe_gp)
      sort(unique(analyses_group$pheno_type))
    })
    
    # Input `input$dataset`.
    output$dataset_input <- shiny::renderUI({
      shiny::req(project_info())
      choices <- c("all", shiny::req(pheno_type()))
      if(is.null(selected <- input$dataset))
        selected <- NULL
      shiny::selectInput(ns("dataset"), "Phenotype Set",
                         choices = as.list(choices),
                         selected = selected,
                         multiple = TRUE)
    })
    
    ## Set up analyses data frame.
    analyses_set <- shiny::reactive({
      shiny::req(project_info(), analyses_tbl())
      set_analyses(input$dataset, input$pheno_group, analyses_tbl())
    })
    # Restrict peaks to region.
    peak_dataset_df <- shiny::reactive({
      shiny::req(project_info(), analyses_set(), peak_df())
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
    
    # Pick phenotype names
    output$pheno_names_input <- shiny::renderUI({
      shiny::req(project_info(), win_par$chr_id, win_par$peak_Mbp, win_par$window_Mbp)
      out <- select_phenames(input$pheno_names, peak_dataset_df(), win_par$local,
                             win_par$chr_id, win_par$peak_Mbp, win_par$window_Mbp)
      shiny::selectInput(ns("pheno_names"), out$label,
                         choices = out$choices,
                         selected = out$selected,
                         multiple = TRUE)
    })
    
    ## Locate Peak.
    win_par <- peakServer("peak", input, peak_df, pmap_obj, 
                          project_info)
    
    chr_pos <- shiny::reactive({
      shiny::req(project_info())
      make_chr_pos(win_par$chr_id, 
                   win_par$peak_Mbp, win_par$window_Mbp)
    })
    output$chr_pos <- shiny::renderText({
      paste0("Region: ", chr_pos(), "Mbp")
    })
    output$num_pheno <- shiny::renderText({
      shiny::req(project_info())
      num_pheno(character(), analyses_tbl())
    })
    output$version <- shiny::renderText({
      versions()
    })
    
    shiny::observeEvent(project_info(), {
      output$num_pheno <- shiny::renderText({
        num_pheno(input$pheno_names, analyses_tbl())
      })
      shiny::req(win_par$chr_id, win_par$peak_Mbp, win_par$window_Mbp)
      out <- select_phenames("none", peak_dataset_df(), win_par$local,
                             win_par$chr_id, win_par$peak_Mbp, win_par$window_Mbp)
      updateSelectInput(session, "pheno_names", out$label,
                        choices = out$choices, selected = "none")
    })
    shiny::observeEvent(input$pheno_names, {
      output$num_pheno <- shiny::renderText({
        num_pheno(input$pheno_names, analyses_tbl())
      })
    })
    
    ## Use window as input to phenoServer.
    phenoServer("pheno", input, win_par, peak_dataset_df, analyses_tbl, cov_df, project_info)
    
    ## Setup input logic.
    output$project_name <- renderUI({
      shiny::strong(paste("Project:", 
                          shiny::req(project_info()$project),
                          "\n"))
    })
    output$sidebar_setup <- shiny::renderUI({
      switch(shiny::req(input$radio),
             Phenotypes = shiny::tagList(
               shiny::uiOutput(ns("filter")),
               phenoUI(ns("pheno"))),
             Region     = peakInput(ns("peak")))
    })
    output$sidebar_hot <- shiny::renderUI({
      switch(shiny::req(input$radio),
             Region     = peakUI(ns("peak")))
    })
    output$main_setup <- shiny::renderUI({
      switch(shiny::req(input$radio),
             Phenotypes = phenoOutput(ns("pheno")),
             Region     = peakOutput(ns("peak")))
    })
    
    output$radio_input <- shiny::renderUI({
      shiny::radioButtons(ns("radio"), NULL,
                          c("Region", "Phenotypes"),
                          input$radio,
                          inline=TRUE)
    })
    
    ## Return.
    pheno_names <- shiny::reactive(input$pheno_names)
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
    shiny::uiOutput(ns("pheno_names_input")),
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
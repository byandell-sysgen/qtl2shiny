#' Shiny setup module
#'
#' Shiny module for phenotype selection, with interfaces \code{setupInput} and  \code{setupUI}.
#'
#' @param id identifier for shiny reactive
#' @param pheno_typer,peaks_tbl,pmap_obj,analyses_tbl,cov_df,projects_info reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom dplyr filter 
#' @importFrom shiny checkboxInput isTruthy mainPanel moduleServer NS
#'             observeEvent radioButtons reactive renderText renderUI req
#'             sidebarPanel strong tagList textOutput uiOutput
setupServer <- function(id, pheno_typer, peaks_tbl, pmap_obj, analyses_tbl, 
                       cov_df, projects_info) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    project_info <- projectServer("project", projects_info)
    
    # Select phenotype dataset
    pheno_group <- shiny::reactive({
      shiny::req(project_info())
      sort(unique(shiny::req(analyses_tbl())$pheno_group))
    }, label = "pheno_group")
    pheno_type <- shiny::reactive({
      shiny::req(project_info())
      phe_gp <- shiny::req(input$pheno_group)
      analyses_group <- 
        dplyr::filter(
          shiny::req(analyses_tbl()),
          pheno_group %in% phe_gp)
      sort(unique(analyses_group$pheno_type))
    })
    output$pheno_group <- shiny::renderUI({
      shiny::req(choices <- pheno_group())
      if(is.null(selected <- input$pheno_group)) {
        selected <- choices[1]
      }
      shiny::selectInput(ns("pheno_group"), "",
                         choices = as.list(choices),
                         selected = selected,
                         multiple = TRUE)
    })
    output$dataset <- shiny::renderUI({
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
    peaks_df <- shiny::reactive({
      shiny::req(project_info(), analyses_set(), peaks_tbl())
      chr_id <- shiny::req(win_par$chr_id)
      peak_Mbp <- shiny::req(win_par$peak_Mbp)
      window_Mbp <- shiny::req(win_par$window_Mbp)
      peaks_in_pos(analyses_set(), peaks_tbl(),
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
    output$pheno_names <- shiny::renderUI({
      shiny::req(project_info(), win_par$chr_id, win_par$peak_Mbp, win_par$window_Mbp)
      out <- select_phenames(input$pheno_names, peaks_df(), win_par$local,
                             win_par$chr_id, win_par$peak_Mbp, win_par$window_Mbp)
      shiny::selectInput(ns("pheno_names"), out$label,
                         choices = out$choices,
                         selected = out$selected,
                         multiple = TRUE)
    })
    
    ## Locate Peak.
    win_par <- peaksServer("peaks", input, pheno_type, peaks_tbl, pmap_obj, 
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
      out <- select_phenames("none", peaks_df(), win_par$local,
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
    phenoServer("pheno", input, win_par, peaks_df, analyses_tbl, cov_df, project_info)
    
    ## Setup input logic.
    output$project_name <- renderUI({
      shiny::strong(paste("Project:", 
                          shiny::req(project_info()$project),
                          "\n"))
    })
    output$project_name2 <- renderUI({
      shiny::strong(paste("Project:", 
                          shiny::req(project_info()$project),
                          "\n"))
    })
    output$title <- shiny::renderUI({
      switch(shiny::req(input$radio),
             Region = {
               shiny::tagList(
                 projectUI(ns("project")),
                 shiny::strong(shiny::req(input$radio)))
             },
             Phenotypes = {
               shiny::tagList(
                 shiny::uiOutput(ns("project_name2")),
                 shiny::strong(shiny::req(input$radio)))
             })
    })
    output$sidebar_setup <- shiny::renderUI({
      switch(shiny::req(input$radio),
             Phenotypes = shiny::tagList(
               shiny::uiOutput(ns("filter")),
               phenoUI(ns("pheno"))),
             Region     = peaksInput(ns("peaks")))
    })
    output$sidebar_hot <- shiny::renderUI({
      switch(shiny::req(input$radio),
             Region     = peaksUI(ns("peaks")))
    })
    output$main_setup <- shiny::renderUI({
      switch(shiny::req(input$radio),
             Phenotypes = phenoOutput(ns("pheno")),
             Region     = peaksOutput(ns("peaks")))
    })
    
    output$radio <- shiny::renderUI({
      shiny::radioButtons(ns("radio"), NULL,
                          c("Region", "Phenotypes"),
                          input$radio,
                          inline=TRUE)
    })
    
    ## Return.
    shiny::reactive({
      list(project_info = project_info(),
           pheno_names = input$pheno_names,
           win_par = win_par)
    })
  })
}
#' @export
#' @rdname setupServer
setupInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("project_name")),
    shiny::textOutput(ns("num_pheno")),
    shiny::uiOutput(ns("chr_pos"))
  )
}
#' @export
#' @rdname setupServer
setupUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarPanel(
      shiny::uiOutput(ns("title")),
      shiny::uiOutput(ns("radio")),
      shiny::uiOutput(ns("sidebar_setup")),
      shiny::uiOutput(ns("pheno_names")),
      shiny::uiOutput(ns("pheno_group")),
      shiny::uiOutput(ns("dataset")),
      shiny::uiOutput(ns("sidebar_hot")),
      shiny::uiOutput(ns("version"))
    ),
    shiny::mainPanel(shiny::uiOutput(ns("main_setup")))
  )
}

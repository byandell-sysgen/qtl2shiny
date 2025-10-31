#' Shiny Phenotype Names App
#'
#' Shiny module for phenotype name selection.
#'
#' @param id identifier for shiny reactive
#' @param set_par,win_par,peak_df,analyses_df,covar,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom dplyr arrange desc filter select
#' @importFrom shiny moduleServer NS observeEvent reactive renderText req
#'             selectizeInput tagList uiOutput updateSelectizeInput
#' @importFrom rlang .data
#' @importFrom bslib card layout_columns page_sidebar sidebar
phenoNamesApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Phenotype Names",
    sidebar = bslib::sidebar(
      bslib::card(
        projectUI("project_df"),         # project
        setParInput("set_par"),          # class, subject_model
        phenoNamesInput("pheno_names")), # pheno_names
      bslib::card(
        winParInput("win_par"),          # hotspot
        bslib::layout_columns(
          col_widths = c(4, 8),
          setParUI("set_par"),           # window_Mbp 
          hotspotInput("hotspot_obj"))), # chr_ct, minLOD
      width = 400
    ),
    phenoNamesOutput("pheno_names"),
    peakFilterOutput("peak_filter_df")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    hotspot_obj <- 
      hotspotServer("hotspot_obj", set_par, peak_df, pmap_obj, project_df)
    hotspot_df <- 
      hotspotTableServer("hotspot_df", hotspot_obj)
    win_par <- winParServer("win_par", hotspot_df, project_df)
    peak_filter_df <- peakFilterServer("peak_filter_df", set_par, win_par,
                                       peak_df, project_df)
    pheno_mx <- phenoServer("pheno_mx", set_par, peak_filter_df, project_df)
    covar_df <- shiny::reactive(read_project(shiny::req(project_df()), "covar"))
    pheno_names <- 
      phenoNamesServer("pheno_names", set_par, peak_filter_df, pheno_mx,
                       covar_df, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname phenoNamesApp
phenoNamesServer <- function(id, set_par, peak_df, pheno_mx, covar_df,
                             project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pheno_names <- shiny::reactive({
      shiny::req(set_par$class)
      c(shiny::req(input$pheno_name), cor_remove(input$pheno_names))
    })

    # Input primary and secondary phenotype names.
    # Primary `input$pheno_name`.
    output$pheno_name_input <- shiny::renderUI({
      # Primary phenotype.
      shiny::selectizeInput(ns("pheno_name"), "", choices = "", multiple = FALSE)
    })
    shiny::observeEvent(shiny::req(project_df(), peak_df()), {
      out <- select_phenames(shiny::req(peak_df()))
      if(!is.null(out)) {
        shiny::updateSelectizeInput(session, "pheno_name", out$label,
          choices = out$choices, selected = out$selected, server = TRUE)
      }
    })
    shiny::observeEvent(set_par$class, {
      if(!shiny::isTruthy(set_par$class)) {
        shiny::updateSelectizeInput(session, "pheno_name", "Primary phenotype",
          choices = NULL, selected = NULL, server = TRUE)
      } else {
        out <- select_phenames(shiny::req(peak_df()))
        if(!is.null(out)) {
          shiny::updateSelectizeInput(session, "pheno_name", out$label,
                                      choices = out$choices, selected = out$selected, server = TRUE)
        }
      }
    }, ignoreNULL = FALSE)
    # Primary `input$pheno_names`.
    output$pheno_names_input <- shiny::renderUI({
      # Additional phenotypes.
      shiny::selectizeInput(ns("pheno_names"), "", choices = "", multiple = TRUE)
    })
    # Use correlation of residuals after covariates.
    shiny::observeEvent(shiny::req(project_df(), peak_df(), pheno_mx(),
                                   covar_df(), input$pheno_name), {
      out <- select_phenames(peak_df(), input$pheno_name,
                             shiny::req(pheno_mx()),
                             cor_covar = TRUE, shiny::req(covar_df()))
      if(!is.null(out)) {
        shiny::updateSelectizeInput(session, "pheno_names", out$label,
          choices = out$choices, selected = out$selected, server = TRUE)
      }
    })
    output$pheno_names_output <- shiny::renderUI({
      shiny::renderText(paste("phenotype names: ",
        paste(shiny::req(pheno_names()), collapse = ", ")))
    })
    ## Return.
    pheno_names
  })
}
#' @export
#' @rdname phenoNamesApp
phenoNamesInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("pheno_name_input")),     # pheno_name
    shiny::uiOutput(ns("pheno_names_input")))    # pheno_names
}
#' @export
#' @rdname phenoNamesApp
phenoNamesOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("pheno_names_output"))      # pheno_names
}

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
          hotspotInput("hotspot"))),     # chr_ct, minLOD
      width = 400
    ),
    phenoNamesUI("pheno_names"),
    phenoNamesOutput("pheno_names")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    hotspot_df <- 
      hotspotServer("hotspot", set_par, peak_df, pmap_obj, project_df)
    win_par <- winParServer("win_par", hotspot_df, project_df)
    pheno_names <- 
      phenoNamesServer("pheno_names", set_par, win_par, peak_df, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname phenoNamesApp
phenoNamesServer <- function(id, set_par, win_par, peak_df, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pheno_names <- shiny::reactive(input$pheno_names)
    
    # Filter peaks to hotspots.
    output$filter <- shiny::renderUI({
      shiny::checkboxInput(ns("filter"),
        "Filter by hotspot(s)?", TRUE)
    })
    peak_filter_df <- shiny::reactive({
      shiny::req(project_df(), peak_df(), win_par())
      chr_id <- win_par()$chr_id
      peak_Mbp <- win_par()$peak_Mbp
      window_Mbp <- shiny::req(set_par$window_Mbp)
      peaks_in_pos(peak_df(), shiny::isTruthy(input$filter),
                   chr_id, peak_Mbp, window_Mbp)
    })
    output$peak_table <- DT::renderDataTable({
      peakDataTable(peak_filter_df())
    }, options = list(scrollX = TRUE, pageLength = 5,
                      lengthMenu = c(5,10,25)))
    
    # Input `input$pheno_names`.
    output$pheno_names_input <- shiny::renderUI({
      shiny::selectizeInput(ns("pheno_names"), "", choices = "", multiple = TRUE)
    })
    shiny::observeEvent(shiny::req(project_df(), win_par(),
      set_par$window_Mbp, peak_filter_df()), {
      out <- select_phenames(NULL, peak_filter_df())
      shiny::updateSelectizeInput(session, "pheno_names", out$label,
                               choices = out$choices,
                               selected = out$selected, server = TRUE)
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
    shiny::uiOutput(ns("pheno_names_input")),
    shiny::uiOutput(ns("filter"))
  )
}
#' @export
#' @rdname phenoNamesApp
phenoNamesUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("pheno_names_output")) # pheno_names
}
#' @export
#' @rdname phenoNamesApp
phenoNamesOutput <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("peak_table")) # peak_table
}

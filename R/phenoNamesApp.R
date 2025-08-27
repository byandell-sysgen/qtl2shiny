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
#' @importFrom bslib layout_columns page_sidebar sidebar
phenoNamesApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Phenotype Names",
    sidebar = bslib::sidebar(
      projectUI("project_df"),        # project
      setParInput("set_par"),         # class, subject_model 
      phenoNamesInput("pheno_names"), # pheno_names
      bslib::layout_columns(
        col_widths = c(6, 4),
        winParInput("win_par"),       # hotspot
        setParUI("set_par")           # window_Mbp 
      ),
      winParUI("win_par"),            # local
      hotspotInput("hotspot")         # chr_ct, minLOD
    ),
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
    
    # Filter peaks to region.
    output$filter <- shiny::renderUI({
      shiny::checkboxInput(ns("filter"),
        "Filter by hotspot(s)?", TRUE)
    })
    chr_pos <- shiny::reactive({
      decode_chr_pos(shiny::req(win_par$hotspot))
    })
    peak_filter_df <- shiny::reactive({
      shiny::req(project_df(), peak_df(), chr_pos())
      chr_id <- chr_pos()$chr
      peak_Mbp <- chr_pos()$pos
      window_Mbp <- shiny::req(set_par$window_Mbp)
      peaks_in_pos(peak_df(), shiny::isTruthy(input$filter),
                   chr_id, peak_Mbp, window_Mbp)
    })

    # Input `input$pheno_names`.
    output$pheno_names_input <- shiny::renderUI({
      shiny::selectizeInput(ns("pheno_names"), "", choices = "", multiple = TRUE)
    })
    shiny::observeEvent(shiny::req(project_df(), chr_pos(),
      set_par$window_Mbp, peak_filter_df()), {
      out <- select_phenames(NULL, peak_filter_df(),
        win_par$local, chr_pos()$chr, chr_pos()$pos, set_par$window_Mbp)
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
phenoNamesOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("pheno_names_output"))
}

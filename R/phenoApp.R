#' Shiny phenotype selection
#'
#' Shiny module for phenotype selection.
#'
#' @param id identifier for shiny reactive
#' @param set_par,win_par,peak_df,analyses_df,covar,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom dplyr arrange desc filter select
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny moduleServer NS radioButtons reactive req selectInput
#'             tagList uiOutput updateSelectInput
#' @importFrom rlang .data
phenoApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Pheno",
    sidebar = bslib::sidebar(
      projectUI("project"),   # project
      setParInput("set_par"), # class, subject_model 
      phenoInput("pheno"),    # pheno_names
      winParInput("win_par"), # local, chr_id, peak_Mbp, window_Mbp
      hotspotInput("hotspot") # chr_ct, minLOD, window_Mbp
    ),
    phenoOutput("pheno")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    
    pmap_obj <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "pmap")
    })

    hotspot_df <- hotspotServer("hotspot", set_par, peak_df, pmap_obj,
                                project_df)
    win_par <- winParServer("win_par", set_par, peak_df, pmap_obj, hotspot_df,
                          project_df)
    pheno_names <- phenoServer("pheno", set_par, win_par, peak_df, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname phenoApp
phenoServer <- function(id, set_par, win_par, peak_df, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pheno_names <- shiny::reactive(input$pheno_names)
    
    # Filter peaks to region.
    output$filter <- shiny::renderUI({
      shiny::checkboxInput(ns("filter"),
        paste0("Filter by hotspot chr ", win_par$chr_id, " hotspot", "?"),
        TRUE)
    })
    peak_filter_df <- shiny::reactive({
      shiny::req(project_df(), peak_df())
      chr_id <- shiny::req(win_par$chr_id)
      peak_Mbp <- shiny::req(win_par$peak_Mbp)
      window_Mbp <- shiny::req(win_par$window_Mbp)
      peaks_in_pos(peak_df(), shiny::isTruthy(input$filter),
                   chr_id, peak_Mbp, window_Mbp)
    })

    # Input `input$pheno_names`.
    output$pheno_names_input <- shiny::renderUI({
      shiny::req(project_df(), win_par$chr_id, win_par$peak_Mbp,
                 win_par$window_Mbp,  peak_filter_df())
      out <- select_phenames(input$pheno_names, peak_filter_df(),
        win_par$local, win_par$chr_id, win_par$peak_Mbp, win_par$window_Mbp)
      shiny::selectInput(ns("pheno_names"), out$label,
                         choices = out$choices,
                         selected = out$selected,
                         multiple = TRUE)
    })
    shiny::observeEvent(peak_filter_df(), {
      out <- select_phenames(NULL, peak_filter_df(),
        win_par$local, win_par$chr_id, win_par$peak_Mbp, win_par$window_Mbp)
      shiny::updateSelectInput(session, "pheno_names",
                               choices = out$choices,
                               selected = out$selected)
    })

    ## Density or scatter plot of phenotypes.
    pheno_mx <- shiny::reactive({
      shiny::req(project_df(), set_par$class, pheno_names())
      read_project(project_df(), "pheno", class = set_par$class,
                   columns = pheno_names())
    })
    # Use `sex` column from `covar_df()` for `pheno_plot`.
    covar_df <- shiny::reactive({
      shiny::req(project_df(), pheno_mx())
      read_project(project_df(), "covar")
    })
    phenoPlotServer("pheno_plot", pheno_names, pheno_mx, covar_df)

    ## Return.
    pheno_names
  })
}
#' @export
#' @rdname phenoApp
phenoInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("pheno_names_input")),
    shiny::uiOutput(ns("filter"))
  )
}
#' @export
#' @rdname phenoApp
phenoOutput <- function(id) {
  ns <- shiny::NS(id)
  phenoPlotUI(ns("pheno_plot"))
}

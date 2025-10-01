#' Shiny Phenotype App
#'
#' Shiny module for peak selection.
#'
#' @param id identifier for shiny reactive
#' @param set_par,peak_df,pmap_obj,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom shiny  column moduleServer NS reactive req
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom bslib layout_columns page_sidebar sidebar
phenoApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Pheno Read",
    sidebar = bslib::sidebar(
      projectUI("project_df"),        # project
      setParInput("set_par"),         # class, subject_model
      phenoInput("pheno_mx"),         # filter
      bslib::layout_columns(
        col_widths = c(6, 4),
        winParInput("win_par"),       # hotspot
        setParUI("set_par")           # window_Mbp 
      ),
      hotspotInput("hotspot"),        # chr_ct, minLOD
      phenoUI("pheno_mx")             # raw_data
    ),
    phenoOutput("pheno_mx"),
    peakOutput("peak_df")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    hotspot_df <- 
      hotspotServer("hotspot", set_par, peak_df, pmap_obj, project_df)
    win_par <- winParServer("win_par", hotspot_df, project_df)
    pheno_mx <- phenoServer("pheno_mx", set_par, win_par, peak_df, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname phenoApp
phenoServer <- function(id, set_par, win_par, peak_df, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
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

    # Phenotypes filtered by `peak_df`.
    pheno_raw_mx <- shiny::reactive({
      shiny::req(project_df(), set_par$class, peak_filter_df())
      pheno_names <- peak_filter_df()$phenotype
      read_pheno(project_df(), set_par$class, columns = pheno_names,
                 peak_df = peak_filter_df())
    })
    pheno_mx <- shiny::reactive({
      out <- shiny::req(pheno_raw_mx())
      rout <- row.names(out)
      if(!shiny::isTruthy(input$raw_data)) {
        out <- apply(out, 2, rankZ)
      }
      row.names(out) <- rout
      out
    })
    
    output$pheno_mx_output <- shiny::renderUI({
      shiny::req(pheno_mx(), set_par$class)
      shiny::renderText(paste("dim of pheno:", paste(dim(pheno_mx()), collapse = ",")))
    })

    ## Return.
    pheno_mx
  })
}
#' @export
#' @rdname phenoApp
phenoInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("filter"))               # filter
}
#' @export
#' @rdname phenoApp
phenoUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::checkboxInput(ns("raw_data"), "Raw Data?", FALSE)
}
#' @export
#' @rdname phenoApp
phenoOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("pheno_mx_output"))
}

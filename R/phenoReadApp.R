#' Shiny Pheno Read App
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
#' @importFrom bslib page_sidebar sidebar
phenoReadApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Pheno Read",
    sidebar = bslib::sidebar(
      projectUI("project"),
      setParInput("set_par"),
      winParInput("win_par"),   # local, chr_id, peak_Mbp, window_Mbp
      hotspotInput("hotspot"), # chr_ct, minLOD, window_Mbp
      phenoInput("pheno")),             # pheno_names
    phenoReadOutput("pheno_mx")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakReadServer("peak_df", set_par, project_df)
    
    pmap_obj <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "pmap")
    })
    
    hotspot_df <- hotspotServer("hotspot", set_par, peak_df, pmap_obj,
                                project_df)
    
    win_par <- winParServer("win_par", set_par, peak_df, pmap_obj, hotspot_df,
                            project_df)
    
    pheno_names <- phenoServer("pheno", set_par, win_par, peak_df, project_df)
    pheno_mx <- phenoReadServer("pheno_mx", set_par, pheno_names, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname phenoReadApp
phenoReadServer <- function(id, set_par, pheno_names, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pheno_mx <- shiny::reactive({
      shiny::req(project_df(), set_par$class, pheno_names())
      read_pheno(project_df(), set_par$class, columns = pheno_names())
    })

    # Output Peak Table.
    output$pheno_table <- DT::renderDataTable({
      shiny::req(pheno_mx())      
    }, options = list(scrollX = TRUE, pageLength = 5,
                      lengthMenu = c(5,10,25)))
    
    ## Return.
    pheno_mx
  })
}
#' @export
#' @rdname phenoReadApp
phenoReadOutput <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("pheno_table")) # peak_table
}

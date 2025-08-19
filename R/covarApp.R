#' Shiny Covariate App
#'
#' @param id identifier for shiny reactive
#' @param pheno_mx,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom shiny moduleServer NS reactive req
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom bslib page_sidebar sidebar
covarApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Covariate Read",
    sidebar = bslib::sidebar(
      projectUI("project_df"),        # project  
      setParInput("set_par"),         # class, subject_model 
      phenoNamesInput("pheno_names"), # pheno_names
      winParInput("win_par"),         # local, chr_id, peak_Mbp, window_Mbp
      hotspotInput("hotspot_df")      # chr_ct, minLOD, window_Mbp
    ),
    covarOutput("covar_df")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    pmap_obj <- pmapServer("pmap_obj", project_df)
    hotspot_df <- 
      hotspotServer("hotspot_df", set_par, peak_df, pmap_obj, project_df)
    win_par <- 
      winParServer("win_par", set_par, peak_df, pmap_obj, hotspot_df, project_df)
    pheno_names <- 
      phenoNamesServer("pheno_names", set_par, win_par, peak_df, project_df)
    pheno_mx <- phenoServer("pheno_mx", set_par, pheno_names, project_df)
    covar_df <- covarServer("covar_df", pheno_mx, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname covarApp
covarServer <- function(id, pheno_mx, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    covar_df <- shiny::reactive({
      shiny::req(project_df(), pheno_mx())
      read_project(project_df(), "covar")
    })

    # Output Peak Table.
    output$covar_table <- DT::renderDataTable({
      shiny::req(covar_df())      
    }, options = list(scrollX = TRUE, pageLength = 5,
                      lengthMenu = c(5,10,25)))
    
    ## Return.
    covar_df
  })
}
#' @export
#' @rdname covarApp
covarOutput <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("covar_table")) # covar_table
}

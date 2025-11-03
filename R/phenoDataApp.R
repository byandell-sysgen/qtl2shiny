#' Shiny Phenotype Data App
#'
#' Shiny module to filter phenotypes by selected names
#' and transform using `rankZ`.
#'
#' @param id identifier for shiny reactive
#' @param pheno_names,pheno_mx,covar_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @return 2-element vector of scan window
#'
#' @export
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny moduleServer NS plotOutput renderPlot renderUI req
#'             setProgress tagList uiOutput withProgress
#' @importFrom bslib card layout_columns page_sidebar sidebar
phenoDataApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Phenotype Plot",
    sidebar = bslib::sidebar(
      projectUI("project_df"),         # project
      setParInput("set_par"),          # class, subject_model 
      phenoNamesInput("pheno_names"),  # pheno_names
      bslib::layout_columns(
        col_widths = c(6, 4),
        winParInput("win_par"),        # hotspot
        setParUI("set_par")            # window_Mbp 
      ),
      hotspotDataInput("hotspot_obj"), # chr_ct, minLOD
      phenoDataInput("pheno_data")     # raw_data
    ),
    phenoDataOutput("pheno_data")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_project_df <- peakServer("peak_project_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    covar_df <- shiny::reactive(read_project(project_df(), "covar"))
    hotspot_obj <- hotspotDataServer("hotspot_obj", set_par, peak_project_df,
                                     pmap_obj, project_df)
    hotspot_df <- 
      hotspotTableServer("hotspot_df", hotspot_obj)
    win_par <- 
      winParServer("win_par", hotspot_df, project_df)
    peak_df <- peakPanelServer("peak_df", set_par, win_par,
                                       peak_project_df, project_df)
    pheno_mx <- phenoServer("pheno_mx", set_par, peak_df, project_df)
    covar_df <- shiny::reactive(read_project(shiny::req(project_df()), "covar"))
    pheno_names <- phenoNamesServer("pheno_names", set_par, peak_df,
                                    pheno_mx, covar_df, project_df)
    pheno_data_mx <- 
      phenoDataServer("pheno_data", pheno_names, pheno_mx, covar_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname phenoDataApp
phenoDataServer <- function(id, pheno_names, pheno_mx, covar_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pheno_mx_names <- shiny::reactive({
      shiny::req(pheno_mx(), pheno_names())
      if(!all(pheno_names() %in% colnames(pheno_mx()))) return(NULL)
      pheno_mx()[, pheno_names(), drop = FALSE]
    })
    pheno_rankz_mx <- shiny::reactive({
      out <- shiny::req(pheno_mx_names())
      rout <- row.names(out)
      if(!shiny::isTruthy(input$raw_data)) {
        out <- apply(out, 2, rankZ)
      }
      row.names(out) <- rout
      out
    })
    pheno_data_mx <- shiny::reactive({
      if(shiny::isTruthy(input$raw_data))
        pheno_mx_names()
      else
        pheno_rankz_mx()
    })
    output$data_output <- shiny::renderUI({
      pheno_data <- shiny::req(pheno_data_mx())
      shiny::tagList(
        shiny::renderText(paste("pheno data: ",
                                paste(dim(pheno_data), collapse = ", "))),
      )
    })
    
    # Return.
    pheno_data_mx
  })
}
#' @export
#' @rdname phenoDataApp
phenoDataInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::checkboxInput(ns("raw_data"), "Raw Data?", FALSE)
}
#' @export
#' @rdname phenoDataApp
phenoDataOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("data_output"))
}
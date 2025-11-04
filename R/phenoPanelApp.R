#' Shiny Hotspot Panel App
#'
#' @param id identifier for shiny reactive
#' @param set_par,win_par,peak_df,pmap_obj,hotspot_df,project_df reactive arguments
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
#' @importFrom bslib card page_sidebar sidebar
phenoPanelApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Pheno Panel",
    sidebar = bslib::sidebar(
      projectUI("project_df"),              # project
      setParInput("set_par"),               # class, subject_model
      phenoPanelInput("pheno_panel"),       # pheno_names
      winParInput("win_par"),               # hotspot
      bslib::layout_columns(
        col_widths = c(4, 8),
        setParUI("set_par"),                # window_Mbp
        hotspotDataInput("hotspot_obj")),   # chr_ct, minLOD
      width = 400),
    downloadInput("download"),              # download inputs for Plot or Table
    phenoPanelOutput("pheno_panel"),
    phenoPanelUI("pheno_panel"),
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_read_df <- peakReadServer("peak_read_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    hotspot_obj <- hotspotDataServer("hotspot_obj", set_par, peak_read_df,
                                     pmap_obj, project_df)
    hotspot_df <- 
      hotspotTableServer("hotspot_df", hotspot_obj)
    win_par <- winParServer("win_par", hotspot_df, project_df)
    peak_df <- peakPanelServer("peak_df", set_par, win_par,
                               peak_read_df, project_df)
    pheno_list <-
      phenoPanelServer("pheno_panel", set_par, win_par, peak_df,
                       pmap_obj, hotspot_df, project_df)
    
    # Download.
    download_Filename <- shiny::reactive({
      out <- paste0(
        paste(shiny::req(pheno_list$pheno_names()), collapse = "_"),
        "_Phenotypes")
      c(Plot = out, Table = out)
    })
    download_list <- shiny::reactiveValues(
      Plot = shiny::isolate(pheno_list$pheno_plot),
      Table = shiny::isolate(pheno_list$pheno_table),
      Filename = download_Filename)
    downloadServer("download", download_list)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname phenoPanelApp
phenoPanelServer <- function(id, set_par, win_par, peak_df,
                             pmap_obj, hotspot_df, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pheno_mx <- phenoReadServer("pheno_mx", set_par, peak_df, project_df)
    covar_df <- shiny::reactive(read_project(project_df(), "covar"))
    pheno_names <- phenoNamesServer("pheno_names", set_par, peak_df,
                                    pheno_mx, covar_df, project_df)
    pheno_data_mx <- 
      phenoDataServer("pheno_data", pheno_names, pheno_mx, covar_df)
    pheno_table <- 
      phenoTableServer("pheno_table", pheno_data_mx, covar_df)
    pheno_plot <- 
      phenoPlotServer("pheno_plot", pheno_data_mx, covar_df)

    output$version <- shiny::renderText({
      versions()
    })

    ## Additional reactives not used yet.
    kinship_list <- kinshipServer("kinship_list", win_par, project_df)
    allele_info <- shiny::reactive(read_project(project_df(), "allele_info"))
    
    ## Return.
    shiny::reactiveValues(
      set_par = set_par,
      win_par = win_par,
      peak_df = peak_df, # filtered by `win_par`
      pmap_obj = pmap_obj,
      covar_df = covar_df,
      hotspot_df = hotspot_df,
      pheno_names = pheno_names,
      pheno_mx = pheno_data_mx, # filtered by `pheno_names`
      kinship_list = kinship_list,
      allele_info = allele_info,
      pheno_plot = pheno_plot,
      pheno_table = pheno_table)
  })
}
#' @export
#' @rdname phenoPanelApp
phenoPanelInput <- function(id) {
  ns <- shiny::NS(id)
  phenoNamesInput(ns("pheno_names"))     # pheno_names
}
#' @export
#' @rdname phenoPanelApp
phenoPanelUI <- function(id) {
  ns <- shiny::NS(id)
  phenoTableOutput(ns("pheno_table"))    # pheno_table
}
#' @export
#' @rdname phenoPanelApp
phenoPanelOutput <- function(id) {
  ns <- shiny::NS(id)
  phenoPlotOutput(ns("pheno_plot"))      # pheno_plot
}

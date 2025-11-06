#' Shiny Hotspot Panel App
#'
#' @param id identifier for shiny reactive
#' @param set_par,peak_df,pmap_obj,project_df reactive arguments
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
hotspotApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Hotspot Panel",
    sidebar = bslib::sidebar(
      bslib::card(
        projectUI("project_df"),            # project
        hotspotInput("hotspot_list")), # class, subject_model, pheno_names, hotspot
      bslib::card(
        hotspotUI("hotspot_list")),    # window_Mbp, radio, win_par, chr_ct, minLOD
      width = 400),
    downloadInput("download"),              # download inputs for Plot or Table
    hotspotOutput("hotspot_list")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotServer("hotspot_list", project_df)
    downloadServer("download", hotspot_list$download)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname hotspotApp
hotspotServer <- function(id, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Modules.    
    set_par <- setParServer("set_par", project_df)
    peak_read_df <- peakReadServer("peak_read_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    hotspot_obj <- hotspotDataServer("hotspot_obj", set_par, peak_read_df,
                                     pmap_obj, project_df)
    hotspot_df <- hotspotTableServer("hotspot_df", hotspot_obj)
    hotspot_plot <- hotspotPlotServer("hotspot_plot", set_par, hotspot_obj)
    win_par <- winParServer("win_par", hotspot_df, project_df)
    peak_df <- peakServer("peak_df", set_par, win_par, peak_read_df, project_df)
    pheno_list <-
      phenoServer("pheno_panel", set_par, win_par, peak_df,
                       pmap_obj, hotspot_df, project_df)
    
    # Download.
    download_Plot <- shiny::reactive({
      switch(shiny::req(input$hot_tab),
        Hotspots = hotspot_plot(),
        Phenotypes = pheno_list$pheno_plot())
    })
    download_Table <- shiny::reactive({
      switch(shiny::req(input$hot_tab),
             Hotspots = hotspot_df(),
             Phenotypes = pheno_list$pheno_table())
    })
    download_Filename <- shiny::reactive({
      out <- switch(shiny::req(input$hot_tab),
        Hotspots = shiny::req(set_par$class),
        Phenotypes = paste(shiny::req(pheno_list$pheno_names()),
                                      collapse = "_"))
      out <- paste(out, shiny::req(input$hot_tab), sep = "_")
      c(Plot = out, Table = out)
    })
    hotspot_list <- pheno_list
    hotspot_list$download <- shiny::reactiveValues(
      Plot = download_Plot,
      Table = download_Table,
      Filename = download_Filename
    )
    
    ## Return.
    hotspot_list
    
    ## Return.
    # shiny::reactiveValues(
    #   set_par = set_par,
    #   win_par = win_par,
    #   peak_df = peak_df, # filtered by `win_par`
    #   pmap_obj = pmap_obj,
    #   covar_df = covar_df,
    #   hotspot_df = hotspot_df,
    #   pheno_names = pheno_names,
    #   pheno_mx = shiny::isolate(pheno_list$pheno_mx), # filtered by `pheno_names`
    #   kinship_list = kinship_list,
    #   allele_info = allele_info,
    #   plot = shiny::isolate(pheno_list$plot),
    #   table = shiny::isolate(pheno_list$table))
  })
}
#' @export
#' @rdname hotspotApp
hotspotInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    setParInput(ns("set_par")),             # class, subject_model
    phenoInput(ns("pheno_panel"))      # pheno_names
  )
}
#' @export
#' @rdname hotspotApp
hotspotUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    winParInput(ns("win_par")),                 # hotspot
    bslib::layout_columns(
      col_widths = c(4, 8),
      setParUI(ns("set_par")),                  # window_Mbp
      hotspotDataInput(ns("hotspot_obj"))))     # chr_ct, minLOD
    #shiny::uiOutput(ns("version"))
}
#' @export
#' @rdname hotspotApp
hotspotOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("hot_tab"),
    bslib::nav_panel("Hotspots",
      shiny::tagList(
        hotspotPlotOutput(ns("hotspot_plot")),  # hotspot_plot
        hotspotTableOutput(ns("hotspot_df")))), # hotspot_table
    bslib::nav_panel("Phenotypes", 
      shiny::tagList(
        phenoOutput(ns("pheno_panel")),         # pheno_plot
        phenoUI(ns("pheno_panel")))))           # pheno_table
}

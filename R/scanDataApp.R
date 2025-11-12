#' Shiny Scan Module
#'
#' Shiny module for scan1 LOD and coefficient plots.
#'
#' @param id identifier for shiny reactive
#' @param hotspot_list,probs_obj,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom qtl2mediate scan1covar
#' @importFrom ggplot2 autoplot
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny checkboxInput column
#'             fluidRow isTruthy moduleServer NS plotOutput
#'             reactive renderPlot renderUI req selectInput setProgress
#'             sliderInput strong tagList uiOutput updateSliderInput
#'             withProgress
#' @importFrom bslib card layout_sidebar navbar_options navset_tab nav_panel
#'             page_navbar sidebar
#' @importFrom downr downloadServer downloadInput
scanDataApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title = "Test Scan",
    bslib::nav_panel(
      title = "Hotspots",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            projectUI("project_df"),        # project
            hotspotInput("hotspot_list")),  # class, subject_model, pheno_names, hotspot
          bslib::card(
            hotspotUI("hotspot_list")),     # window_Mbp, radio, win_par, chr_ct, minLOD
          width = 400),
        hotspotOutput("hotspot_list"))
    ),
    bslib::nav_panel(
      title = "Scan",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          scanDataInput("scan")),           # blups, pheno_name, scan_window
        bslib::card(
          downr::downloadInput("download"), # download inputs for Plot or Table
          scanDataOutput("scan")))
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotServer("hotspot_list", project_df)
    probs_obj <- probsServer("probs", hotspot_list$win_par, project_df)
    download_list <- scanDataServer("scan", hotspot_list, probs_obj, project_df)
    downr::downloadServer("download", download_list)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname scanDataApp
scanDataServer <- function(id, hotspot_list, probs_obj, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    win_par <- shiny::isolate(hotspot_list$win_par)
    
    ## Genome scan 
    scan_obj <- shiny::reactive({
      shiny::req(hotspot_list$pheno_mx(), probs_obj(),
                 hotspot_list$kinship_list(), hotspot_list$covar_df(),
                 hotspot_list$peak_df())
      shiny::withProgress(message = "Genome Scan ...", value = 0, {
        shiny::setProgress(1)
        scan1covar(hotspot_list$pheno_mx(), hotspot_list$covar_df(),
                   probs_obj()$probs, hotspot_list$kinship_list(),
                   hotspot_list$peak_df())
      })
    })
    
    # Scan Window slider
    #** Faulty behavior if more than one hotspot.
    output$scan_window_input <- shiny::renderUI({
      shiny::req(project_df(), hotspot_list$pheno_mx(), win_par())
      chr_id <- shiny::req(win_par())$chr_id
      map <- shiny::req(probs_obj())$map[[chr_id]]
      rng <- round(2 * range(map)) / 2
      selected <- select_range(input$scan_window, rng)
      
      shiny::sliderInput(ns("scan_window"), NULL, rng[1], rng[2],
                         selected, step=.5)
    })
    ## Reset scan_window if chromosome changes.
    observeEvent(probs_obj()$map, {
      map <- shiny::req(probs_obj()$map)
      chr <- shiny::req(win_par())$chr_id
      rng <- round(2 * range(map[[chr]])) / 2
      shiny::updateSliderInput(session, "scan_window", NULL, rng, 
                               rng[1], rng[2], step=.5)
    })
    
    ## Select phenotype for plots.
    output$pheno_name_input <- shiny::renderUI({
      shiny::req(hotspot_list$pheno_mx())
      shiny::selectInput(ns("pheno_name"), NULL,
                         choices = colnames(hotspot_list$pheno_mx()))
    })
    
    ## Scan1 plot
    scan_plot <- shiny::reactive({
      if(!shiny::isTruthy(win_par()$chr_id) || !shiny::isTruthy(hotspot_list$pheno_mx()))
        return(plot_null("need to select\nRegion & Phenotype"))
      shiny::req(win_par(), input$scan_window, scan_obj(), probs_obj())
      shiny::withProgress(message = 'Genome LOD Plot ...', value = 0, {
        shiny::setProgress(1)
        plot_scan(scan_obj(), 
                  probs_obj()$map, 
                  seq(ncol(scan_obj())), 
                  win_par()$chr_id, 
                  input$scan_window, 
                  hotspot_list$pheno_mx())
      })
    })
    output$scan_plot <- shiny::renderPlot({
      print(shiny::req(scan_plot()))
    })
    
    ## Coefficient Effects.
    eff_obj <- shiny::reactive({
      shiny::req(hotspot_list$pheno_mx(), probs_obj(),
                 hotspot_list$kinship_list(), hotspot_list$covar_df(),
                 hotspot_list$peak_df())
      shiny::withProgress(message = 'Effect scans ...', value = 0, {
        shiny::setProgress(1)
        scan1_effect(probs_obj()$probs, hotspot_list$pheno_mx(),
                     hotspot_list$kinship_list(), hotspot_list$covar_df(),
                     hotspot_list$peak_df(), input$blups)
      })
    })
    coef_plot <- shiny::reactive({
      shiny::req(input$pheno_name, scan_obj(), eff_obj(),
                 win_par(), hotspot_list$allele_info())
      map <- shiny::req(probs_obj())$map
      shiny::withProgress(message = 'Effect plots ...', value = 0, {
        shiny::setProgress(1)
        plot_eff(input$pheno_name, eff_obj(), map, scan_obj(), 
                 input$scan_window,, hotspot_list$allele_info())
      })
    })
    output$coef_plot <- shiny::renderPlot({
      print(shiny::req(coef_plot()))
    })
    scan_table <- shiny::reactive({
      shiny::req(eff_obj(), scan_obj(), probs_obj())
      shiny::withProgress(message = 'Effect summary ...', value = 0, {
        shiny::setProgress(1)
        summary(eff_obj(), scan_obj(), probs_obj()$map)
      })
    })
    output$scan_table <- DT::renderDataTable(
      shiny::req(scan_table()),
      escape = FALSE,
      options = list(scrollX = TRUE, pageLength = 5))
    
    ## Effect and LOD Plot
    scan_coef_plot <- shiny::reactive({
      shiny::req(input$pheno_name, input$scan_window, win_par(),
                 eff_obj(), scan_obj(), hotspot_list$allele_info())
      map <- shiny::req(probs_obj())$map
      shiny::withProgress(message = 'Effect & LOD plots ...', value = 0, {
        shiny::setProgress(1)
        plot_eff(input$pheno_name, eff_obj(), map, scan_obj(), input$scan_window,
                 addlod = TRUE, hotspot_list$allele_info())
      })
    })
    output$scan_coef_plot <- shiny::renderPlot({
      print(shiny::req(scan_coef_plot()))
    })
    
    output$pheno_choice <- shiny::renderUI({
      switch(shiny::req(input$scan_tab),
             Both    =,
             Summary =,
             Effects = shiny::tagList(
               shiny::uiOutput(ns("blups_input")),       # blups
               shiny::uiOutput(ns("pheno_name_input")))) # pheno_name
    })
    output$win_choice <- shiny::renderUI({
      switch(shiny::req(input$scan_tab),
             LOD     =,
             Both    =,
             Effects = shiny::uiOutput(ns("scan_window_input")))
    })

    output$blups_input <- shiny::renderUI({
      shiny::checkboxInput(ns("blups"), "BLUPs?")
    })
    
    # Download.
    download_Plot <- shiny::reactive({
      switch(shiny::req(input$scan_tab),
             LOD = scan_plot(),
             Effects = coef_plot(),
             Summary =,
             Both = scan_coef_plot())
    })
    download_Filename <- shiny::reactive({
      scan_tab <- shiny::req(input$scan_tab)
      out <- shiny::req(hotspot_list$set_par$class)
      c(Plot = paste(out,
          ifelse(scan_tab == "Summary", "Both", scan_tab), sep = "_"),
        Table = paste0(out, "_Summary"))
    })
    download_list <- shiny::reactiveValues(
      Plot = download_Plot,
      Table = scan_table,
      Filename = download_Filename
    )
    
    # Return.
    download_list
  })
}
#' @export
#' @rdname scanDataApp
scanDataInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("pheno_choice")), # blups, pheno_name
    shiny::uiOutput(ns("win_choice")))   # scan_window
}
#' @export
#' @rdname scanDataApp
scanDataOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("scan_tab"),
    bslib::nav_panel("LOD", shiny::plotOutput(ns("scan_plot"))),
    bslib::nav_panel("Effects", shiny::plotOutput(ns("coef_plot"))),
    bslib::nav_panel("LOD & Effects", value = "Both",
                     shiny::plotOutput(ns("scan_coef_plot"))),
    bslib::nav_panel("Summary", DT::dataTableOutput(ns("scan_table"))))
}

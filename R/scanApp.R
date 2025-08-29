#' Shiny Scan Module
#'
#' Shiny module for scan1 LOD and coefficient plots, with interfaces \code{scanUI} and  \code{scanOutput}.
#'
#' @param id identifier for shiny reactive
#' @param hotspot_list,hap_par,probs_obj,project_df reactive arguments
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
#'             fluidRow isTruthy moduleServer NS plotOutput radioButtons
#'             reactive renderPlot renderUI req selectInput setProgress
#'             sliderInput strong tagList uiOutput updateSliderInput
#'             withProgress
#' @importFrom utils write.csv
#' @importFrom grDevices dev.off pdf
#' @importFrom qtl2mediate scan1covar
#' @importFrom bslib page_sidebar sidebar
scanApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Scan",
    sidebar = bslib::sidebar(
      projectUI("project"),              # project
      hotspotPanelInput("hotspot_list"), # class, subject_model, pheno_names, hotspot
      hotspotPanelUI("hotspot_list"),    # window_Mbp, radio, win_par, chr_ct, minLOD
      hapParInput("hap_par"),
      scanUI("scan")),
    scanOutput("scan")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    hap_par <- hapParServer("hap_par")
    probs_obj <- probsServer("probs", hotspot_list$win_par, project_df)
    scanServer("scan", hotspot_list, hap_par, probs_obj, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname scanApp
scanServer <- function(id, hotspot_list, hap_par, probs_obj, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Genome scan 
    scan_obj <- shiny::reactive({
      shiny::req(hotspot_list$pheno_mx(), probs_obj(), hotspot_list$kinship_list(), hotspot_list$covar_df(), hotspot_list$peak_df(),
                 hap_par$sex_type)
      shiny::withProgress(message = "Genome Scan ...", value = 0, {
        shiny::setProgress(1)
        scan1covar(hotspot_list$pheno_mx(), hotspot_list$covar_df(), probs_obj()$probs, hotspot_list$kinship_list(),
                   hotspot_list$peak_df())
      })
    })
    
    # Scan Window slider
    output$scan_window_input <- shiny::renderUI({
      shiny::req(project_df(), hotspot_list$pheno_mx(), hotspot_list$win_par$window_Mbp)
      chr_id <- shiny::req(hotspot_list$win_par$chr_id)
      map <- shiny::req(probs_obj())$map[[chr_id]]
      rng <- round(2 * range(map)) / 2
      selected <- select_range(input$scan_window, rng)
      
      shiny::sliderInput(ns("scan_window"), NULL, rng[1], rng[2],
                         selected, step=.5)
    })
    ## Reset scan_window if chromosome changes.
    observeEvent(probs_obj()$map, {
      map <- shiny::req(probs_obj()$map)
      chr <- shiny::req(hotspot_list$win_par$chr_id)
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
    output$scan_plot <- shiny::renderPlot({
      if(!shiny::isTruthy(hotspot_list$win_par$chr_id) || !shiny::isTruthy(hotspot_list$pheno_mx()))
        return(plot_null("need to select\nRegion & Phenotype"))
      shiny::req(hotspot_list$win_par$chr_id, input$scan_window, scan_obj(), probs_obj())
      shiny::withProgress(message = 'Genome LOD Plot ...', value = 0, {
        shiny::setProgress(1)
        plot_scan(scan_obj(), 
                  probs_obj()$map, 
                  seq(ncol(scan_obj())), 
                  hotspot_list$win_par$chr_id, 
                  input$scan_window, 
                  hotspot_list$pheno_mx())
      })
    })
    
    ## Coefficient Effects.
    eff_obj <- shiny::reactive({
      shiny::req(hotspot_list$pheno_mx(), probs_obj(), hotspot_list$kinship_list(), hotspot_list$covar_df(),
                 hap_par$sex_type)
      shiny::withProgress(message = 'Effect scans ...', value = 0, {
        shiny::setProgress(1)
        scan1_effect(probs_obj()$probs, hotspot_list$pheno_mx(), hotspot_list$kinship_list(), hotspot_list$covar_df(),
                     hap_par$sex_type, input$blups)
      })
    })
    output$coef_plot <- shiny::renderPlot({
      shiny::req(input$pheno_name, scan_obj(), eff_obj(), hotspot_list$win_par$chr_id, hotspot_list$allele_info())
      map <- shiny::req(probs_obj())$map
      shiny::withProgress(message = 'Effect plots ...', value = 0, {
        shiny::setProgress(1)
        plot_eff(input$pheno_name, eff_obj(), map, scan_obj(), 
                 input$scan_window,, hotspot_list$allele_info())
      })
    })
    output$scan_table <- DT::renderDataTable({
      shiny::req(eff_obj(), scan_obj(), probs_obj())
      shiny::withProgress(message = 'Effect summary ...', value = 0, {
        shiny::setProgress(1)
        summary(eff_obj(), scan_obj(), probs_obj()$map)
      })
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
    
    ## Effect and LOD Plot
    output$scan_coef_plot <- shiny::renderPlot({
      shiny::req(input$pheno_name, input$scan_window, hotspot_list$win_par$chr_id,
                 eff_obj(), scan_obj(), hotspot_list$allele_info())
      map <- shiny::req(probs_obj())$map
      shiny::withProgress(message = 'Effect & LOD plots ...', value = 0, {
        shiny::setProgress(1)
        plot_eff(input$pheno_name, eff_obj(), map, scan_obj(), input$scan_window,
                 addlod = TRUE, hotspot_list$allele_info())
      })
    })
    
    output$pheno_choice <- shiny::renderUI({
      switch(shiny::req(input$button),
             "LOD & Effects" =,
             Effects = shiny::uiOutput(ns("pheno_name_input")))
    })
    output$win_choice <- shiny::renderUI({
      switch(shiny::req(input$button),
             LOD     =,
             "LOD & Effects" =,
             Effects = shiny::uiOutput(ns("scan_window_input")))
    })
    output$LOD <- shiny::renderUI({
      switch(shiny::req(input$button),
             LOD             = shiny::plotOutput(ns("scan_plot")))
    })
    output$Effects <- shiny::renderUI({
      switch(shiny::req(input$button),
             Effects         = shiny::plotOutput(ns("coef_plot")),
             "LOD & Effects" = shiny::plotOutput(ns("scan_coef_plot")))
    })
    output$Summary <- shiny::renderUI({
      switch(shiny::req(input$button),
             Summary = DT::dataTableOutput(ns("scan_table")))
    })
    
    
    output$blups_input <- shiny::renderUI({
      shiny::checkboxInput(ns("blups"), "BLUPs?")
    })
    output$button_input <- shiny::renderUI({
      shiny::radioButtons(ns("button"), "",
                          c("LOD","Effects","LOD & Effects","Summary"),
                          input$button)
    })
    
    ## Downloads.
    filepath <- shiny::reactive({
      # Catch if `chr_id` is not valid and return "".
      tryCatch(file.path(paste0("scan_", shiny::req(hotspot_list$win_par$chr_id))),
               error = function(e) return(""))
    })
    download_Plot <- shiny::reactiveValues()
    download_Plot$scan <- shiny::reactive({
      shiny::req(hotspot_list$win_par$chr_id, hotspot_list$allele_info())
      effs <- shiny::req(eff_obj())
      scans <- shiny::req(scan_obj())
      win <- shiny::req(input$scan_window)
      map <- shiny::req(probs_obj())$map
      ggplot2::autoplot(scans, map, lodcolumn = seq_along(names(effs)),
                        chr = shiny::req(hotspot_list$win_par$chr_id), xlim = win)
    })
    # Trick to get Effect plots into `download_Plot` reactiveValues.
    plot_effs <- shiny::reactive({
      # Catch if not valid and return NULL.
      effs <- tryCatch(shiny::req(eff_obj()), error = function(e) return(NULL))
      for(pheno in names(effs)) {
        download_Plot[[pheno]] <- shiny::reactive({
          shiny::req(hotspot_list$win_par$chr_id, hotspot_list$allele_info())
          scans <- shiny::req(scan_obj())
          win <- shiny::req(input$scan_window)
          map <- shiny::req(probs_obj())$map
          plot_eff(pheno, effs, map, scans, win, addlod = TRUE, hotspot_list$allele_info())
        })
      }
    })
    shiny::isolate(plot_effs())
    download_list <- shiny::reactiveValues(
      filename = shiny::isolate(filepath()),
      Plot = download_Plot,
      Table = shiny::reactiveValues(
        scan = shiny::reactive({
          shiny::req(eff_obj(), scan_obj(), probs_obj())
          summary(eff_obj(), scan_obj(), probs_obj()$map)
        }))
    )
    # downloadServer(ns("download"), download_list,
    #                selected_item = shiny::reactive("scan"),
    #                plot_table = shiny::reactive(input$plot_table),
    #                title_download = shiny::reactive("Scan"))
  })
}
#' @export
#' @rdname scanApp
scanUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::strong("Genome Scans"),
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("button_input"))),
      shiny::column(6, shiny::uiOutput(ns("blups_input")))),
    shiny::uiOutput(ns("pheno_choice")),
    shiny::uiOutput(ns("win_choice")),
    shiny::selectInput(ns("plot_table"), "", c("Plot","Table")),
    downloadOutput(ns("download")))      # downloadButton, filename
}
#' @export
#' @rdname scanApp
scanOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("LOD")),
    shiny::uiOutput(ns("Effects")),
    shiny::uiOutput(ns("Summary")))
}

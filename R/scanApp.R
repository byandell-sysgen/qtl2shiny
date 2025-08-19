#' Shiny Scan Module
#'
#' Shiny module for scan1 LOD and coefficient plots, with interfaces \code{scanUI} and  \code{scanOutput}.
#'
#' @param id identifier for shiny reactive
#' @param hap_par,set_list$,pheno_mx,covar_df,probs_obj,K_chr,project_df,allele_info reactive arguments
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
      projectUI("project"),
      projectUI("project"),
      setParInput("set_par"),
      setupInput("setup"),
      setupUI("setup"),
      hapParInput("hap_par"),
      scanUI("scan")),
    scanOutput("scan")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    
    pmap_obj <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "pmap")
    })

    # set_list returns pheno_names(), win_par.
    set_list <- setupServer("setup", set_par, peak_df, pmap_obj, project_df)

    pheno_mx <- shiny::reactive({
      shiny::req(project_df(), set_par$class)
      pheno_names <- shiny::req(set_list$pheno_names())
      read_project(project_df(), "pheno", class = set_par$class,
                   columns = pheno_names)
    })
    covar_df <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "covar")
    })
    K_chr <- shiny::reactive({
      shiny::req(project_df())
      chr_id <- shiny::req(set_list$win_par$chr_id)
      read_project(project_df(), "kinship")[chr_id]
    })
    ## Allele names.
    allele_info <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "allele_info")
    })

    hap_par <- hapParServer("hap_par")
    probs_obj <- probsServer("probs", set_list$win_par, project_df)
    scanServer("scan", hap_par, set_list$win_par, peak_df, pheno_mx, covar_df,
               K_chr, probs_obj, allele_info, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname scanApp
scanServer <- function(id, hap_par, win_par,
  peak_df, pheno_mx, covar_df, K_chr, probs_obj, allele_info, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Genome scan 
    scan_obj <- shiny::reactive({
      shiny::req(pheno_mx(), probs_obj(), K_chr(), covar_df(), peak_df(),
                 hap_par$sex_type)
      shiny::withProgress(message = "Genome Scan ...", value = 0, {
        shiny::setProgress(1)
        scan1covar(pheno_mx(), covar_df(), probs_obj()$probs, K_chr(),
                   peak_df())
      })
    })
    
    # Scan Window slider
    output$scan_window_input <- shiny::renderUI({
      shiny::req(project_df(), pheno_mx(), win_par$window_Mbp)
      chr_id <- shiny::req(win_par$chr_id)
      map <- shiny::req(probs_obj())$map[[chr_id]]
      rng <- round(2 * range(map)) / 2
      selected <- select_range(input$scan_window, rng)
      
      shiny::sliderInput(ns("scan_window"), NULL, rng[1], rng[2],
                         selected, step=.5)
    })
    ## Reset scan_window if chromosome changes.
    observeEvent(probs_obj()$map, {
      map <- shiny::req(probs_obj()$map)
      chr <- shiny::req(win_par$chr_id)
      rng <- round(2 * range(map[[chr]])) / 2
      shiny::updateSliderInput(session, "scan_window", NULL, rng, 
                               rng[1], rng[2], step=.5)
    })
    
    ## Select phenotype for plots.
    output$pheno_name_input <- shiny::renderUI({
      shiny::req(pheno_mx())
      shiny::selectInput(ns("pheno_name"), NULL,
                         choices = colnames(pheno_mx()))
    })
    
    ## Scan1 plot
    output$scan_plot <- shiny::renderPlot({
      if(!shiny::isTruthy(win_par$chr_id) || !shiny::isTruthy(pheno_mx()))
        return(plot_null("need to select\nRegion & Phenotype"))
      shiny::req(win_par$chr_id, input$scan_window, scan_obj(), probs_obj())
      shiny::withProgress(message = 'Genome LOD Plot ...', value = 0, {
        shiny::setProgress(1)
        plot_scan(scan_obj(), 
                  probs_obj()$map, 
                  seq(ncol(scan_obj())), 
                  win_par$chr_id, 
                  input$scan_window, 
                  pheno_mx())
      })
    })
    
    ## Coefficient Effects.
    eff_obj <- shiny::reactive({
      shiny::req(pheno_mx(), probs_obj(), K_chr(), covar_df(),
                 hap_par$sex_type)
      shiny::withProgress(message = 'Effect scans ...', value = 0, {
        shiny::setProgress(1)
        scan1_effect(probs_obj()$probs, pheno_mx(), K_chr(), covar_df(),
                     hap_par$sex_type, input$blups)
      })
    })
    output$coef_plot <- shiny::renderPlot({
      shiny::req(input$pheno_name, scan_obj(), eff_obj(), win_par$chr_id, allele_info())
      map <- shiny::req(probs_obj())$map
      shiny::withProgress(message = 'Effect plots ...', value = 0, {
        shiny::setProgress(1)
        plot_eff(input$pheno_name, eff_obj(), map, scan_obj(), 
                 input$scan_window,, allele_info())
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
      shiny::req(input$pheno_name, input$scan_window, win_par$chr_id,
                 eff_obj(), scan_obj(), allele_info())
      map <- shiny::req(probs_obj())$map
      shiny::withProgress(message = 'Effect & LOD plots ...', value = 0, {
        shiny::setProgress(1)
        plot_eff(input$pheno_name, eff_obj(), map, scan_obj(), input$scan_window,
                 addlod = TRUE, allele_info())
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
      tryCatch(file.path(paste0("scan_", shiny::req(win_par$chr_id))),
               error = function(e) return(""))
    })
    download_Plot <- shiny::reactiveValues()
    download_Plot$scan <- shiny::reactive({
      shiny::req(win_par$chr_id, allele_info())
      effs <- shiny::req(eff_obj())
      scans <- shiny::req(scan_obj())
      win <- shiny::req(input$scan_window)
      map <- shiny::req(probs_obj())$map
      ggplot2::autoplot(scans, map, lodcolumn = seq_along(names(effs)),
                        chr = shiny::req(win_par$chr_id), xlim = win)
    })
    # Trick to get Effect plots into `download_Plot` reactiveValues.
    plot_effs <- shiny::reactive({
      # Catch if not valid and return NULL.
      effs <- tryCatch(shiny::req(eff_obj()), error = function(e) return(NULL))
      for(pheno in names(effs)) {
        download_Plot[[pheno]] <- shiny::reactive({
          shiny::req(win_par$chr_id, allele_info())
          scans <- shiny::req(scan_obj())
          win <- shiny::req(input$scan_window)
          map <- shiny::req(probs_obj())$map
          plot_eff(pheno, effs, map, scans, win, addlod = TRUE, allele_info())
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

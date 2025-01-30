#' Shiny coefficient analysis and plot module
#'
#' Shiny module for scan1 LOD and coefficient plots, with interfaces \code{shinyScanCoefUI} and  \code{shinyScanCoefOutput}.
#'
#' @param id identifier for shiny reactive
#' @param job_par,win_par,phe_mx,cov_df,probs_obj,K_chr,analyses_df,project_info,allele_info reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom qtl2ggplot listof_scan1coef
#' @importFrom qtl2mediate scan1covar
#' @importFrom qtl2 scan1
#' @importFrom ggplot2 autoplot
#' @importFrom shiny moduleServer NS reactive req 
#'   radioButtons selectInput sliderInput updateSliderInput
#'   dataTableOutput plotOutput uiOutput
#'   renderDataTable renderPlot renderUI
#'   fluidRow column strong tagList
#'   withProgress setProgress
#'   downloadButton downloadHandler
#' @importFrom utils write.csv
#' @importFrom grDevices dev.off pdf
#' @importFrom qtl2mediate sexcovar
#' 
shinyScanCoef <- function(id, job_par, win_par, phe_mx, cov_df, probs_obj, K_chr,
                          analyses_df, project_info, allele_info) {
  shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns
  
  ## Genome scan 
  scan_obj <- shiny::reactive({
    shiny::req(phe_mx(), probs_obj(), K_chr(), cov_df(), win_par$window_Mbp,
               job_par$sex_type)
    shiny::withProgress(message = "Genome Scan ...", value = 0, {
      shiny::setProgress(1)
      qtl2mediate::scan1covar(phe_mx(), cov_df(), probs_obj()$probs, K_chr(), analyses_df(),
                  sex_type = job_par$sex_type)
    })
  })
  
  # Scan Window slider
  output$scan_window <- shiny::renderUI({
    shiny::req(project_info(), phe_mx(), win_par$window_Mbp)
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
  output$pheno_name <- shiny::renderUI({
    shiny::req(phe_mx())
    shiny::selectInput(ns("pheno_name"), NULL,
                choices = colnames(phe_mx()))
  })

  ## Scan1 plot
  output$scanPlot <- shiny::renderPlot({
    if(!shiny::isTruthy(win_par$chr_id) || !shiny::isTruthy(phe_mx()))
      return(plot_null("need to select\nRegion & Phenotype"))
    shiny::req(win_par$chr_id, input$scan_window, scan_obj(), probs_obj())
    shiny::withProgress(message = 'Genome LOD Plot ...', value = 0, {
      shiny::setProgress(1)
      plot_scan(scan_obj(), 
                probs_obj()$map, 
                seq(ncol(scan_obj())), 
                win_par$chr_id, 
                input$scan_window, 
                phe_mx())
    })
  })

  ## Coefficient Effects.
  eff_obj <- shiny::reactive({
    shiny::req(phe_mx(), probs_obj(), K_chr(), cov_df(),
               job_par$sex_type)
    shiny::withProgress(message = 'Effect scans ...', value = 0, {
      shiny::setProgress(1)
      scan1_effect(probs_obj()$probs, phe_mx(), K_chr(), cov_df(),
                   job_par$sex_type, input$blups)
    })
  })
  output$effPlot <- shiny::renderPlot({
    shiny::req(input$pheno_name, scan_obj(), eff_obj(), win_par$chr_id, allele_info())
    map <- shiny::req(probs_obj())$map
    shiny::withProgress(message = 'Effect plots ...', value = 0, {
      shiny::setProgress(1)
      plot_eff(input$pheno_name, eff_obj(), map, scan_obj(), 
               input$scan_window,, allele_info())
    })
  })
  output$effSummary <- shiny::renderDataTable({
    shiny::req(eff_obj(), scan_obj(), probs_obj())
    shiny::withProgress(message = 'Effect summary ...', value = 0, {
      shiny::setProgress(1)
      summary(eff_obj(), scan_obj(), probs_obj()$map)
    })
  }, escape = FALSE,
  options = list(scrollX = TRUE, pageLength = 5))
  
  ## Effect and LOD Plot
  output$lod_effPlot <- shiny::renderPlot({
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
           Effects = shiny::uiOutput(ns("pheno_name")))
  })
  output$win_choice <- shiny::renderUI({
    switch(shiny::req(input$button),
           LOD     =,
           "LOD & Effects" =,
           Effects = shiny::uiOutput(ns("scan_window")))
  })
  output$LOD <- shiny::renderUI({
    switch(shiny::req(input$button),
           LOD             = shiny::plotOutput(ns("scanPlot")))
  })
  output$Effects <- shiny::renderUI({
    switch(shiny::req(input$button),
           Effects         = shiny::plotOutput(ns("effPlot")),
           "LOD & Effects" = shiny::plotOutput(ns("lod_effPlot")))
  })
  output$Summary <- shiny::renderUI({
    switch(shiny::req(input$button),
           Summary = shiny::dataTableOutput(ns("effSummary")))
  })

  ## Downloads.
  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      file.path(paste0("sum_effects_", win_par$chr_id, ".csv")) },
    content = function(file) {
      shiny::req(eff_obj(), scan_obj(), probs_obj())
      utils::write.csv(summary(eff_obj(), scan_obj(), probs_obj()$map), file)
    }
  )
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      file.path(paste0("scan_", win_par$chr_id, ".pdf")) },
    content = function(file) {
      shiny::req(win_par$chr_id, allele_info())
      effs <- shiny::req(eff_obj())
      scans <- shiny::req(scan_obj())
      win <- shiny::req(input$scan_window)
      map <- shiny::req(probs_obj())$map
      grDevices::pdf(file, width=9,height=9)
      print(ggplot2::autoplot(scans, map,
                 lodcolumn = seq_along(names(effs)),
                 chr = win_par$chr_id,
                 xlim = win))
      for(pheno in names(effs)) {
        plot_eff(pheno, effs, map, scans, win,
                 addlod = TRUE, allele_info())
      }
      grDevices::dev.off()
    }
  )
  output$blups <- shiny::renderUI({
    shiny::checkboxInput(ns("blups"), "BLUPs?")
  })
  output$radio <- shiny::renderUI({
    shiny::radioButtons(ns("button"), "",
                 c("LOD","Effects","LOD & Effects","Summary"),
                 input$button)
  })
})
}
shinyScanCoefUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::strong("Genome Scans"),
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("radio"))),
      shiny::column(6, shiny::uiOutput(ns("blups")))),
    shiny::uiOutput(ns("pheno_choice")),
    shiny::uiOutput(ns("win_choice")),
    shiny::fluidRow(
      shiny::column(6, shiny::downloadButton(ns("downloadData"), "CSV")),
      shiny::column(6, shiny::downloadButton(ns("downloadPlot"), "Plots"))))
}
shinyScanCoefOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("LOD")),
    shiny::uiOutput(ns("Effects")),
    shiny::uiOutput(ns("Summary")))
}

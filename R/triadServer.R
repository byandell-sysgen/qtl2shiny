#' Shiny triad plot module
#'
#' Shiny module for triad plots, with interfaces \code{triadUI} and  \code{triadOutput}.
#'
#' @param id identifier for shiny reactive
#' @param med_par,patterns,geno_max,peak_mar,med_ls,mediate_obj,phe_mx,cov_df,K_chr,probs_obj,chr_id,sdp reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom ggplot2 autoplot
#' @importFrom qtl2mediate mediation_triad_qtl2
#' @importFrom shiny column downloadButton downloadHandler fluidRow isTruthy
#'             moduleServer NS plotOutput reactive renderPlot renderUI req
#'             selectInput setProgress tagList uiOutput withProgress
#' @importFrom utils write.csv
#' @importFrom grDevices dev.off pdf
#
triadServer <- function(id, med_par, patterns, geno_max, peak_mar, med_ls,
                       mediate_obj, phe_mx, cov_df, K_chr, probs_obj, chr_id, sdp) {
  shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns

  ## Select triad for plots.
  output$triad_input <- shiny::renderUI({
    triad <- shiny::req(mediate_obj())$best$triad
    choices <- levels(triad)
    choices <- choices[choices %in% unique(triad)]
    shiny::selectInput(ns("triad"), NULL,
                       choices = choices, input$triad)
  })
  medID <- reactive({
    ifelse("symbol" %in% names(shiny::req(mediate_obj())$best), "symbol", "longname")
  })
  ## Select mediator for plots.
  output$med_name_input <- shiny::renderUI({
    shiny::req(mediate_obj(), input$triad, medID())
    choices <- dplyr::filter(mediate_obj()$best, .data$triad == input$triad)[[medID()]]
    shiny::selectInput(ns("med_name"), NULL,
                       choices = choices, input$med_name)
  })
  
  
  scat_dat <- reactive({
    shiny::req(phe_mx(), med_ls(), cov_df(), probs_obj(), chr_id(),
               input$med_name, med_par$pos_Mbp, sdp())
    qtl2mediate::mediation_triad_qtl2(
      target = phe_mx(),
      mediator = med_ls()[[1]][, input$med_name, drop = FALSE],
      annotation = med_ls()[[2]],
      covar_tar = cov_df(),
      covar_med = med_ls()$covar,
      genoprobs = probs_obj()$probs,
      map = probs_obj()$map,
      chr = chr_id(),
      pos = med_par$pos_Mbp,
      kinship = K_chr()[[1]],
      sdp = sdp())
  })
  
  ## Select plot format.
  output$med_plot_input <- shiny::renderUI({
    shiny::selectInput(ns("med_plot"), NULL,
                       choices = c("by_mediator", 
                                   "by_target", 
                                   "driver_offset", 
                                   "driver"),
                       input$med_plot)
  })

  ## Triad plot
  output$scatPlot <- shiny::renderPlot({
    if(!shiny::isTruthy(patterns())) {
      return(plot_null("first run\nAllele Patterns"))
    }
    if(!shiny::isTruthy(scat_dat())) {
      plot_null("too much\nmissing data\nin mediators\nreduce window width")
    } else {
      shiny::req(input$med_plot, input$med_name, phe_mx())
      shiny::withProgress(message = 'Triad Plot ...', value = 0, {
        shiny::setProgress(1)
        ggplot2::autoplot(scat_dat(), type = input$med_plot,
             dname = peak_mar(),
             mname = input$med_name,
             tname = colnames(phe_mx()),
             fitlines = "sdp",
             centerline = NULL)
      })
    }
  })

  ## Downloads.
  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      file.path(paste0("scatter.csv")) },
    content = function(file) {
      shiny::req(mediate_obj())
      utils::write.csv(mediate_obj()$best, file)
    }
  )
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      file.path(paste0("scatter.pdf")) },
    content = function(file) {
      shiny::req(phe_mx(), scat_dat(), input$med_plot)
      grDevices::pdf(file, width=9,height=9)
      for(types in c("by_mediator", 
                     "by_target", 
                     "driver_offset", 
                     "driver")) {
        print(ggplot2::autoplot(scat_dat(), type = types,
                   dname = peak_mar(),
                   mname = input$med_name,
                   tname = colnames(phe_mx()),
                   centerline = NULL))
      }
      grDevices::dev.off()
    })
  med_ls
})
}
#' @export
#' @rdname triadServer
triadUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("triad_input")),
    shiny::uiOutput(ns("med_name_input")),
    shiny::uiOutput(ns("med_plot_input")),
    shiny::fluidRow(
      shiny::column(6, shiny::downloadButton(ns("downloadData"), "CSV")),
      shiny::column(6, shiny::downloadButton(ns("downloadPlot"), "Plots"))))
}
#' @export
#' @rdname triadServer
triadOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("scatPlot"))
}

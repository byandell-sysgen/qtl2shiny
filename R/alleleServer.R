#' Shiny allele coefficient analysis and plot module
#'
#' Shiny module for scan1 coefficient plots, with interfaces \code{alleleUI} and  \code{alleleOutput}.
#'
#' @param id identifier for shiny reactive
#' @param win_par,phe_mx,cov_df,probs_obj,K_chr,analyses_df,patterns,scan_pat,project_info,snp_action reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny column downloadButton downloadHandler fluidRow moduleServer
#'             NS plotOutput reactive renderPlot renderUI req setProgress
#'             sliderInput tagList uiOutput updateSliderInput withProgress
#' @importFrom ggplot2 autoplot ggtitle
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom utils write.csv
#' @importFrom grDevices dev.off pdf
#' @importFrom rlang .data
alleleServer <- function(id, win_par, phe_mx, cov_df, probs_obj, K_chr,
                        analyses_df, patterns, scan_pat, project_info, snp_action) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Scan Window slider
    output$pos_Mbp <- shiny::renderUI({
      shiny::req(project_info())
      chr_id <- shiny::req(win_par$chr_id)
      map <- shiny::req(probs_obj())$map[[chr_id]]
      rng <- round(2 * range(map)) / 2
      if(is.null(selected <- input$pos_Mbp))
        selected <- req(win_par$peak_Mbp)
      shiny::sliderInput(ns("pos_Mbp"), NULL, rng[1], rng[2],
                         selected, step=.1)
    })
    ## Reset pos_Mbp if chromosome changes.
    observeEvent(win_par$chr_id, {
      map <- shiny::req(probs_obj()$map)
      chr <- shiny::req(win_par$chr_id)
      rng <- round(2 * range(map[[chr]])) / 2
      shiny::updateSliderInput(session, "pos_Mbp", NULL, 
                               req(win_par$peak_Mbp), 
                               rng[1], rng[2], step=.1)
    })
    
    ## Coefficient Effects.
    allele_obj <- shiny::reactive({
      shiny::req(snp_action(), project_info())
      shiny::req(phe_mx(), probs_obj(), K_chr(), cov_df())
      blups <- attr(scan_pat(), "blups")
      shiny::withProgress(message = 'Effect scans ...', value = 0, {
        shiny::setProgress(1)
        allele_scan(phe_mx(), cov_df(), probs_obj(), K_chr(),
                    patterns(), scan_pat(), blups)
      })
    })
    output$allele1Plot <- shiny::renderPlot({
      shiny::req(allele_obj(), input$pos_Mbp)
      shiny::withProgress(message = 'Allele plots ...', value = 0, {
        shiny::setProgress(1)
        p <- ggplot2::autoplot(allele_obj(), pos = input$pos_Mbp)
        if(is.null(p)) {
          plot_null()
        } else {
          p + ggplot2::ggtitle(colnames(phe_mx()))
        }
      })
    })
    output$allele1Sum <- DT::renderDataTable({
      shiny::req(allele_obj(), input$pos_Mbp)
      shiny::withProgress(message = 'Effect summary ...', value = 0, {
        shiny::setProgress(1)
        summary(allele_obj(), pos = input$pos_Mbp)
      })
    })
    
    ## Downloads.
    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        file.path(paste0("allele1_", win_par$chr_id, "_", win_par$peak_Mbp, ".csv")) },
      content = function(file) {
        shiny::req(allele_obj())
        out <- tidyr::pivot_wider(
          dplyr::select(
            dplyr::mutate(allele_obj(),
                          allele = paste(.data$source, .data$allele, sep = ".")),
            -.data$probe, -.data$source),
          names_from = "allele", values_from = "effect")
        utils::write.csv(out, file)
      }
    )
    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        file.path(paste0("allele1_", win_par$chr_id, "_", win_par$peak_Mbp, ".pdf")) },
      content = function(file) {
        shiny::req(allele_obj(), input$pos_Mbp)
        grDevices::pdf(file, width=9,height=9)
        print(ggplot2::autoplot(
          allele_obj(), pos = input$pos_Mbp) +
            ggplot2::ggtitle(colnames(phe_mx())))
        grDevices::dev.off()
      }
    )
  })
}
#' @export
#' @rdname alleleServer
alleleUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("pos_Mbp")),
    shiny::fluidRow(
      shiny::column(6, shiny::downloadButton(ns("downloadData"), "CSV")),
      shiny::column(6, shiny::downloadButton(ns("downloadPlot"), "Plots"))))
}
#' @export
#' @rdname alleleServer
alleleOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns("allele1Plot")),
    DT::dataTableOutput(ns("allele1Sum")))
}

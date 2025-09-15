#' Shiny SNP summary module
#'
#' Shiny module for SNP summary, with interfaces \code{shinySNPInput}, \code{shinySNPUI} and  \code{shinySNPOutput}.
#'
#' @param id identifier for shiny reactive
#' @param snp_list,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny downloadButton downloadHandler moduleServer NS reactive
#'             renderUI req selectInput setProgress tagList uiOutput
#'             withProgress
#' @importFrom ggplot2 autoplot
#' @importFrom utils write.csv
#' @importFrom rlang .data
#' 
snpSumApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test SNP Sum",
    bslib::nav_panel(
      title = "Hotspots",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            projectUI("project_df"),            # project
            hotspotPanelInput("hotspot_list")), # class, subject_model, pheno_names, hotspot
          bslib::card(
            hotspotPanelUI("hotspot_list")),    # window_Mbp, radio, win_par, chr_ct, minLOD
          width = 400),
        hotspotPanelOutput("hotspot_list"))
    ),
    bslib::nav_panel(
      title = "snpSum",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            snpListInput("snp_list")),    # scan_window, minLOD, pheno_name
          bslib::card(
            dipParUI("dip_par")),         # allele_names
          width = 400),
        bslib::card(snpSumOutput("snp_sum"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    dip_par <- dipParServer("dip_par", hotspot_list)
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    snpSumServer("snp_sum", snp_list, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname snpSumApp
snpSumServer <- function(id, snp_list, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    best_snps <- shiny::reactive({
      shiny::req(snp_list$top_snps_tbl())
      summary(snp_list$top_snps_tbl(),"best")
    })
    best_href <- shiny::reactive({
      best <- shiny::req(best_snps())
      ensembl_gene(best, project_df(), TRUE)
    })
    best_http <- shiny::reactive({
      best <- shiny::req(best_snps())
      ensembl_gene(best, project_df())
    })
    
    output$top_snps_tbl <- DT::renderDataTable({
      shiny::req(snp_list$top_snps_tbl())
      shiny::withProgress(message = "Top SNP Range ...", value = 0,
                          {
                            shiny::setProgress(1)
                            summary(snp_list$top_snps_tbl())
                          })
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
    output$top_snps_best <- DT::renderDataTable({
      shiny::withProgress(message = "Top SNP Best ...", value = 0,
                          {
                            shiny::setProgress(1)
                            shiny::req(best_href())
                          })
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
    output$top_indels <- DT::renderDataTable({
      shiny::withProgress(message = "Top InDels ...", value = 0, {
        shiny::setProgress(1)
        # This might change from .data$type to .data$variant_type someday
        dplyr::filter(shiny::req(best_href()), .data$type != "SNP")
      })
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
    output$top_snps_peak <- DT::renderDataTable({
      shiny::req(snp_list$top_snps_tbl())
      shiny::withProgress(message = "Top SNP Peaks ...", value = 0,
                          {
                            shiny::setProgress(1)
                            summary(snp_list$top_snps_tbl(),"peak")
                          })
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
  })
}
#' @export
#' @rdname snpSumApp
snpSumOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("sum_tab"),
    bslib::nav_panel("best", bslib::card(
      DT::dataTableOutput(ns("top_snps_best")))),
    bslib::nav_panel("indels", bslib::card(
      DT::dataTableOutput(ns("top_indels")))),
    bslib::nav_panel("peaks", bslib::card(
      DT::dataTableOutput(ns("top_snps_peak")))),
    bslib::nav_panel("range", bslib::card(
      DT::dataTableOutput(ns("top_snps_tbl")))))
}

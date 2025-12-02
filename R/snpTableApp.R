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
#' @importFrom shiny moduleServer NS reactive renderUI req selectInput
#'             setProgress tagList uiOutput withProgress
#' @importFrom ggplot2 autoplot
#' @importFrom utils write.csv
#' @importFrom rlang .data
#' @importFrom bslib card layout_sidebar navset_tab nav_panel page_navbar sidebar
snpTableApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test SNP Sum",
    bslib::nav_panel(
      title = "Hotspots",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            projectUI("project_df"),       # project
            hotspotInput("hotspot_list")), # class, subject_model, pheno_names, hotspot
          bslib::card(
            hotspotUI("hotspot_list")),    # window_Mbp, radio, win_par, chr_ct, minLOD
          width = 400),
        hotspotOutput("hotspot_list"))
    ),
    bslib::nav_panel(
      title = "snpTable",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            snpListInput("snp_list")),     # scan_window, minLOD, pheno_name
          bslib::card(
            dipParUI("dip_par")),          # allele_names
          width = 400),
        bslib::card(snpTableOutput("snp_table"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotServer("hotspot_list", project_df)
    dip_par <- dipParServer("dip_par", hotspot_list)
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    snpTableServer("snp_table", snp_list, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname snpTableApp
snpTableServer <- function(id, snp_list, project_df) {
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
    snp_sum_table <- shiny::reactive({
      top_snps_tbl <- shiny::req(snp_list$top_snps_tbl())
      msg <- switch(shiny::req(input$sum_tab),
        best   = "SNP Best",
        indels = "InDels",
        peaks  = "SNP Peaks",
        range  = "SNP Range")
      
      shiny::withProgress(message = paste("Top", msg, "..."), value = 0, {
        shiny::setProgress(1)
        switch(shiny::req(input$sum_tab),
          best   = shiny::req(best_href()),
          indels = dplyr::filter(shiny::req(best_href()), .data$type != "SNP"),
          peaks  = summary(top_snps_tbl, "peak"),
          range  = summary(top_snps_tbl))
      })
    })
    
    output$range_table <- DT::renderDataTable(
      shiny::req(snp_sum_table()),
      escape = FALSE, options = list(scrollX = TRUE, pageLength = 5))
    output$best_table <- DT::renderDataTable(
      shiny::req(snp_sum_table()),
      escape = FALSE, options = list(scrollX = TRUE, pageLength = 5))
    output$indels_table <- DT::renderDataTable(
      shiny::req(snp_sum_table()),
      escape = FALSE, options = list(scrollX = TRUE, pageLength = 5))
    output$peaks_table <- DT::renderDataTable(
      shiny::req(snp_sum_table()),
      escape = FALSE, options = list(scrollX = TRUE, pageLength = 5))
    
    # Return.
    snp_sum_table
  })
}
#' @export
#' @rdname snpTableApp
snpTableOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("sum_tab"),
    bslib::nav_panel("best", bslib::card(
      DT::dataTableOutput(ns("best_table")))),
    bslib::nav_panel("indels", bslib::card(
      DT::dataTableOutput(ns("indels_table")))),
    bslib::nav_panel("peaks", bslib::card(
      DT::dataTableOutput(ns("peaks_table")))),
    bslib::nav_panel("range", bslib::card(
      DT::dataTableOutput(ns("range_table")))))
}

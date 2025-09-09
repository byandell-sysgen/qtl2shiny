#' Shiny SNP Features in SNP Region module
#'
#' Shiny module for scan1 analysis and plots, with interfaces \code{snpFeatureInput}, \code{snpFeatureUI} and  \code{snpFeatureOutput}.
#'
#' @param id identifier for shiny reactive
#' @param snp_list reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom ggplot2 autoplot
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny column fluidRow moduleServer
#'             NS plotOutput reactive renderPlot renderUI req selectInput
#'             setProgress tagList uiOutput withProgress
#' @importFrom utils write.csv
#' @importFrom grDevices dev.off pdf   
#' @importFrom bslib card layout_sidebar navset_tab nav_hide nav_panel
#'             nav_select nav_show page_navbar sidebar
snpFeatureApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test SNP Feature",
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
      title = "snpFeature",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          snpListInput2("snp_list"),            # minLOD
          snpListUI("snp_list"), # pheno_name
          snpListInput("snp_list")), # scan_window
        bslib::card(snpFeatureOutput("top_feature"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    snpFeatureServer("top_feature", snp_list)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname snpFeatureApp
snpFeatureServer <- function(id, snp_list) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    top_feature <- shiny::reactive({
      shiny::req(snp_list$top_snps_tbl(), snp_list$snp_scan_obj(),
                 snp_list$snpinfo(), snp_list$gene_exon_tbl())
      shiny::withProgress(message = 'Merging gene info ...', value = 0, {
        shiny::setProgress(1)
        topSNPs(snp_list$top_snps_tbl(), snp_list$snpinfo(),
                snp_list$snp_scan_obj(), snp_list$gene_exon_tbl(),
                snp_list$snp_par$pheno_name)
      })
    })
    
    output$feature_cons_table <- DT::renderDataTable({
      tops <- shiny::req(top_feature(), "SNP type")
      summary(tops)
    }, options = list(scrollX = TRUE, paging = FALSE, searching=FALSE))
    output$feature_pattern_table <- DT::renderDataTable({
      summary(top_feature(), "pattern")
    }, options = list(scrollX = TRUE, paging = FALSE, searching=FALSE))
    phename <- shiny::reactive({dimnames(snp_list$snp_scan_obj())[[2]]})
    output$feature_pattern_plot <- shiny::renderPlot({
      shiny::req(top_feature(), snp_list$snp_par$pheno_name)
      ggplot2::autoplot(top_feature(), snp_list$snp_par$pheno_name, "consequence")
    })
    output$feature_cons_plot <- shiny::renderPlot({
      shiny::req(top_feature(), snp_list$snp_par$pheno_name)
      ggplot2::autoplot(top_feature(), snp_list$snp_par$pheno_name, "pattern")
    })
  })
}
#' @export
#' @rdname snpFeatureApp
snpFeatureOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("fea_tab"),
    bslib::nav_panel("Pattern",     shiny::tagList(
      shiny::plotOutput(ns("feature_pattern_plot")),
      DT::dataTableOutput(ns("feature_pattern_table")))),
    bslib::nav_panel("Consequence", shiny::tagList(
      shiny::plotOutput(ns("feature_cons_plot")),
      DT::dataTableOutput(ns("feature_cons_table")))))
}

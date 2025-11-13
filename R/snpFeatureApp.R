#' Shiny SNP Features in SNP Region module
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
            projectUI("project_df"),       # project
            hotspotInput("hotspot_list")), # class, subject_model, pheno_names, hotspot
          bslib::card(
            hotspotUI("hotspot_list")),    # window_Mbp, radio, win_par, chr_ct, minLOD
          width = 400),
        hotspotOutput("hotspot_list"))
    ),
    bslib::nav_panel(
      title = "snpFeature",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          snpListInput("snp_list")),       # scan_window, minLOD, pheno_name
        bslib::card(snpFeatureOutput("top_feature"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotServer("hotspot_list", project_df)
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
    
    # Patterns.
    feature_pattern_table <- shiny::reactive({
      summary(shiny::req(top_feature()), "pattern")
    })
    feature_pattern_plot <- shiny::reactive({
      shiny::req(top_feature(), snp_list$snp_par$pheno_name)
      ggplot2::autoplot(top_feature(), snp_list$snp_par$pheno_name, "consequence")
    })
    output$feature_pattern_table <- DT::renderDataTable(
      shiny::req(feature_pattern_table()),
      options = list(scrollX = TRUE, paging = FALSE, searching=FALSE))
    output$feature_pattern_plot <- shiny::renderPlot({
      print(shiny::req(feature_pattern_plot()))
    })

    # Consequences.
    feature_cons_table <- shiny::reactive({
      tops <- shiny::req(top_feature(), "SNP type")
      summary(tops)
    })
    feature_cons_plot <- shiny::reactive({
      shiny::req(top_feature(), snp_list$snp_par$pheno_name)
      ggplot2::autoplot(top_feature(), snp_list$snp_par$pheno_name, "pattern")
    })
    output$feature_cons_table <- DT::renderDataTable(
      shiny::req(feature_cons_table()),
      options = list(scrollX = TRUE, paging = FALSE, searching=FALSE))
    output$feature_cons_plot <- shiny::renderPlot({
      print(shiny::req(feature_cons_plot()))
    })
    
    # Download.
    download_Plot <- shiny::reactive({
      switch(shiny::req(input$fea_tab),
        Pattern     = shiny::req(feature_pattern_plot()),
        Consequence = shiny::req(feature_cons_plot()))
    })
    download_Table <- shiny::reactive({
      switch(shiny::req(input$fea_tab),
        Pattern     = shiny::req(feature_pattern_table()),
        Consequence = shiny::req(feature_cons_table()))
    })
    download_Filename <- shiny::reactive({
      shiny::req(input$fea_tab)
    })
    download_list <- shiny::reactiveValues(
      Plot = download_Plot,
      Table = download_Table,
      Filename = download_Filename
    )
    
    # Return.
    download_list
  })
}
#' @export
#' @rdname snpFeatureApp
snpFeatureOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("fea_tab"),
    bslib::nav_panel("By Pattern",     value = "Pattern",     shiny::tagList(
      shiny::plotOutput(ns("feature_pattern_plot")),
      DT::dataTableOutput(ns("feature_pattern_table")))),
    bslib::nav_panel("By Consequence", value = "Consequence", shiny::tagList(
      shiny::plotOutput(ns("feature_cons_plot")),
      DT::dataTableOutput(ns("feature_cons_table")))))
}

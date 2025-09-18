#' Shiny SNP Pattern App
#'
#' @param id identifier for shiny reactive
#' @param snp_list,allele_info reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @export
#' 
#' @importFrom dplyr across distinct mutate where
#' @importFrom qtl2pattern sdp_to_pattern
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny column fluidRow moduleServer
#'             NS plotOutput reactive renderPlot renderUI req
#'             selectInput setProgress strong tagList uiOutput
#'             withProgress
#' @importFrom rlang .data
#' @importFrom bslib card layout_sidebar navset_tab nav_hide nav_panel
#'             nav_select nav_show page_navbar sidebar
snpPatternApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test SNP Pattern",
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
      title = "snpPattern",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            dipParInput("dip_par")),      # snp_action
          bslib::card(
            snpListInput("snp_list")),    # scan_window, minLOD, pheno_name
          bslib::card(
            dipParUI("dip_par")),         # allele_names
          width = 400),
        bslib::card(snpPatternOutput("snp_pattern"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    dip_par <- dipParServer("dip_par", hotspot_list)
    snp_action <- shiny::reactive({dip_par$snp_action})
    snp_list <- snpListServer("snp_list", hotspot_list, project_df, snp_action)
    snpPatternServer("snp_pattern", snp_list, hotspot_list$allele_info)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname snpPatternApp
snpPatternServer <- function(id, snp_list, allele_info) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Shiny Module
    snpFeatureServer("top_feature", snp_list)
    
    sum_top_pat <- shiny::reactive({
      summary(shiny::req(snp_list$top_snps_tbl()))
    })
    
    output$snp_pattern_table <- DT::renderDataTable({
      dplyr::mutate(sum_top_pat(),
                    dplyr::across(dplyr::where(is.numeric),
                                  signif, digits = 4))
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
    
    dropHilit <- shiny::reactive({
      max(0,
          max(unclass(shiny::req(snp_list$snp_scan_obj()))) - 
            shiny::req(snp_list$snp_par$minLOD))
    })
    chr_id <- shiny::reactive(snp_list$win_par()$chr_id)
    peak_Mbp <- shiny::reactive(snp_list$win_par()$peak_Mbp)
    ready <- shiny::reactive({
      shiny::isTruthy(snp_list$snp_par$pheno_name) |
        shiny::isTruthy(snp_list$snp_scan_obj()) |
        shiny::isTruthy(snp_list$snp_par$scan_window) |
        shiny::isTruthy(snp_list$snp_action()) |
        shiny::isTruthy(snp_list$snpinfo()) |
        all(shiny::isTruthy(chr_id()))
    })
    output$snp_pattern_plot <- shiny::renderPlot({
      if(!ready()) return(plot_null())
      shiny::withProgress(message = 'SNP pattern plots ...', value = 0, {
        shiny::setProgress(1)
        top_pat_plot(snp_list$snp_par$pheno_name, 
                     snp_list$snp_scan_obj(), 
                     chr_id(),
                     snp_list$snpinfo(),
                     snp_list$snp_par$scan_window,
                     drop_hilit = dropHilit(),
                     snp_action = snp_list$snp_action())
      })
    })
    
    output$snp_pattern_plotly <- plotly::renderPlotly({
      if(!ready()) return(plot_null())
      shiny::withProgress(message = 'SNP pattern plots ...', value = 0, {
        shiny::setProgress(1)
        top_pat_plot(snp_list$snp_par$pheno_name, 
                     snp_list$snp_scan_obj(), 
                     chr_id(),
                     snp_list$snpinfo(),
                     snp_list$snp_par$scan_window,
                     drop_hilit = dropHilit(),
                     snp_action = snp_list$snp_action(),
                     lines = FALSE, cex = 2)
      })
    })
    
    ## SNP Pheno patterns
    output$snp_phe_pat <- shiny::renderPlot({
      if(!ready()) return(plot_null())
      shiny::withProgress(message = 'SNP Pheno patterns ...', value = 0, {
        shiny::setProgress(1)
        top_pat_plot(snp_list$pheno_names(), 
                     snp_list$snp_scan_obj(), 
                     chr_id(),
                     snp_list$snpinfo(),
                     snp_list$snp_par$scan_window,
                     drop_hilit = dropHilit(),
                     facet = "pheno", 
                     snp_action = snp_list$snp_action())
      })
    })
    
    haplos <- reactive({
      shiny::req(allele_info())$code
    })
    output$pattern <- shiny::renderUI({
      shiny::req(snp_list$snp_action())
      top_pat <- shiny::req(snp_list$top_snps_tbl())
      choices <- qtl2pattern::sdp_to_pattern(
        dplyr::distinct(top_pat, .data$sdp)$sdp,
        haplos())
      if(!is.null(selected <- input$pattern)) {
        if(!selected %in% choices)
          selected <- NULL
      }
      shiny::selectInput(ns("pattern"), NULL,
                         choices = choices,
                         selected = selected)
    })
    ## SNP Pattern phenos
    output$snp_pat_phe <- shiny::renderPlot({
      if(!ready()) return(plot_null())
      #     shiny::req(input$pattern)
      top_pat <- shiny::req(snp_list$top_snps_tbl())
      patterns <- qtl2pattern::sdp_to_pattern(top_pat$sdp, haplos())
      shiny::withProgress(message = 'SNP Pattern phenos ...', value = 0, {
        shiny::setProgress(1)
        top_pat_plot(snp_list$pheno_names(), 
                     snp_list$snp_scan_obj(), 
                     chr_id(),
                     snp_list$snpinfo(), 
                     snp_list$snp_par$scan_window,
                     drop_hilit = dropHilit(),
                     facet = "pattern", 
                     snp_action = snp_list$snp_action())
      })
    })
    
    output$title <- shiny::renderUI({
      if(snp_action() == "basic")
        shiny::strong("SNP Plots")
    })
    
    # shiny::observeEvent(snp_list$pheno_names(), {
    #   browser()
    #   if(length(snp_list$pheno_names()) == 1) {
    #     shiny::tagList(
    #       bslib::nav_select(ns("pat_tab"), "All Phenos", session),
    #       bslib::nav_hide(ns("pat_tab"), "Top SNPs", session),
    #       bslib::nav_hide(ns("pat_tab"), "By Pheno", session))
    #   } else {
    #     shiny::tagList(
    #       bslib::nav_select(ns("pat_tab"), "Top SNPs", session),
    #       bslib::nav_show(ns("pat_tab"), "Top SNPs", session),
    #       bslib::nav_show(ns("pat_tab"), "By Pheno", session))
    #   }
    # })

    # Return.
    input
  })
}
#' @export
#' @rdname snpPatternApp
snpPatternOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("pat_tab"),
    bslib::nav_panel("All Phenos",   shiny::plotOutput(ns("snp_phe_pat"))),
    bslib::nav_panel("All Patterns", shiny::plotOutput(ns("snp_pat_phe"))),
    bslib::nav_panel("Summary",      DT::dataTableOutput(ns("snp_pattern_table"))),
    bslib::nav_panel("Top SNPs",     snpFeatureOutput(ns("top_feature"))),
    bslib::nav_panel("By Pheno",     shiny::plotOutput(ns("snp_pattern_plot"))),
    bslib::nav_panel("Interactive",  plotly::plotlyOutput(ns("snp_pattern_plotly"))))
}

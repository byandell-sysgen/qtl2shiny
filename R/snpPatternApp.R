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
#' @importFrom shiny column downloadButton downloadHandler fluidRow moduleServer
#'             NS plotOutput radioButtons reactive renderPlot renderUI req
#'             selectInput setProgress strong tagList uiOutput
#'             updateRadioButtons withProgress
#' @importFrom rlang .data
#' @importFrom bslib card layout_sidebar nav_panel page_navbar sidebar
snpPatternApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test SNP Pattern Setup",
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
          hapParInput("hap_par"),                 # sex_type
          snpPatternInput("snp_pattern"),   # button_input pat_input
          snpListInput2("snp_list"),            # minLOD
          snpListUI("snp_list"), # pheno_name
          snpListInput("snp_list")), # scan_window
        bslib::card(snpPatternOutput("snp_pattern"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    hap_par <- hapParServer("hap_par")
    snp_list <- snpListServer("snp_list", hotspot_list, hap_par, project_df)
    snpPatternServer("snp_pattern", snp_list, allele_info)
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
    chr_id <- shiny::reactive(snp_list$win_par()$chr_id[1])
    peak_Mbp <- shiny::reactive(snp_list$win_par()$peak_Mbp[1])
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
    
    output$pat_input <- shiny::renderUI({
      switch(shiny::req(input$button),
             #           "All Patterns" = shiny::uiOutput(ns("pattern")),
             "Top SNPs"     = snpFeatureInput(ns("top_feature"))) # by_choice
    })
    output$pat_output <- shiny::renderUI({
      switch(shiny::req(input$button),
             "Top SNPs"     = snpFeatureOutput(ns("top_feature")),
             "By Pheno"     = shiny::plotOutput(ns("snp_pattern_plot")),
             "All Phenos"   = shiny::plotOutput(ns("snp_phe_pat")),
             "All Patterns" = shiny::plotOutput(ns("snp_pat_phe")),
             "Interactive"  = plotly::plotlyOutput(ns("snp_pattern_plotly")))
    })
    output$title <- shiny::renderUI({
      if(snp_action() == "basic")
        shiny::strong("SNP Plots")
    })
    
    ## Downloads
    output$download_csv_plot <- shiny::renderUI({
      switch(shiny::req(input$button),
        "Top SNPs" = snpFeatureUI(ns("top_feature")),
        shiny::fluidRow(
          shiny::column(6, shiny::downloadButton(ns("downloadData"), "CSV")),
          shiny::column(6, shiny::downloadButton(ns("downloadPlot"), "Plots"))))
    })
    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        file.path(paste0(paste0("pattern", chr_id(),
                                peak_Mbp(), snp_action(),
                                sep = "_"),
                         ".csv")) },
      content = function(file) {
        utils::write.csv(sum_top_pat(), file)
      }
    )
    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        file.path(paste0(paste0("pattern", chr_id(),
                                peak_Mbp(), snp_action(),
                                sep = "_"),
                         ".pdf")) },
      content = function(file) {
        scans <- shiny::req(snp_list$snp_scan_obj())
        snp_w <- shiny::req(snp_list$snp_par$scan_window)
        phenos <- shiny::req(snp_list$snp_list$pheno_names())
        grDevices::pdf(file, width = 9)
        ## Plots over all phenotypes
        print(top_pat_plot(phenos, 
                           scans, 
                           chr_id(),
                           snp_list$snpinfo(), 
                           snp_w,
                           drop_hilit = dropHilit(),
                           facet = "pheno", 
                           snp_action = snp_list$snp_action()))
        
        print(top_pat_plot(snp_list$pheno_names(), 
                           snp_list$snp_scan_obj(), 
                           chr_id(),
                           snp_list$snpinfo(), 
                           snp_list$snp_par$scan_window,
                           drop_hilit = dropHilit(),
                           facet = "pattern", 
                           snp_action = snp_list$snp_action()))
        
        ## Plots by phenotype.
        for(pheno in phenos) {
          print(top_pat_plot(pheno, 
                             scans, 
                             chr_id(),
                             snp_list$snpinfo(), 
                             snp_w, 
                             drop_hilit = dropHilit(),
                             snp_action = snp_list$snp_action()))
        }
        grDevices::dev.off()
      }
    )
    output$button_input <- shiny::renderUI({
      button_val <- c("All Phenos","All Patterns",
                      "By Pheno",
                      "Top SNPs","Interactive")
      if(length(snp_list$pheno_names()) == 1) {
        button_val <- button_val[-(1:2)]
      }
      if(!is.null(selected <- input$button)) {
        if(!(selected %in% button_val))
          selected <- button_val[1]
      }
      shiny::radioButtons(ns("button"), "",
                          button_val, selected)
    })
    ## Update Radio Button if 1 or >1 Phenotype Names.
    shiny::observeEvent(snp_list$pheno_names(), {
      button_val <- c("All Phenos","All Patterns",
                      "By Pheno",
                      "Top SNPs")
      if(length(snp_list$pheno_names()) == 1) {
        button_val <- button_val[-(1:2)]
      }
      selected <- input$button
      if(!is.null(selected)) {
        if(!(selected %in% button_val))
          selected <- button_val[1]
        shiny::updateRadioButtons(session, "button", 
                                  selected = selected,
                                  choices = button_val)
      }
    })
    
    input
  })
}
#' @export
#' @rdname snpPatternApp
snpPatternInput <- function(id) { # button_input pat_input
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("button_input")), # button
    shiny::uiOutput(ns("pat_input"))     # by_choice
  )
}
#' @export
#' @rdname snpPatternApp
snpPatternUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("download_csv_plot"))
}
#' @export
#' @rdname snpPatternApp
snpPatternOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("pat_output")),
    DT::dataTableOutput(ns("snp_pattern_table")))
}

#' Shiny top SNP analysis and plot module
#'
#' Shiny module for top SNP analysis and plots, with interfaces \code{snpPatternInput}, \code{snpPatternUI} and  \code{snpPatternOutput}.
#'
#' @param id identifier for shiny reactive
#' @param snp_par,chr_pos,pheno_names,snp_scan_obj,snpinfo,top_snps_tbl,gene_exon_tbl,allele_info,snp_action reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @export
#' 
#' @importFrom dplyr distinct
#' @importFrom qtl2pattern sdp_to_pattern
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny column downloadButton downloadHandler fluidRow moduleServer
#'             NS plotOutput radioButtons reactive renderPlot renderUI req
#'             selectInput setProgress strong tagList uiOutput
#'             updateRadioButtons withProgress
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom utils write.csv
#' @importFrom grDevices dev.off pdf
#' @importFrom rlang .data
snpPatternServer <- function(id, snp_par, chr_pos, pheno_names, snp_scan_obj,
                            snpinfo, top_snps_tbl, gene_exon_tbl, allele_info, 
                            snp_action = shiny::reactive({"basic"})) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Shiny Module
    snpFeatureServer("top_feature", snp_par, chr_pos, snp_scan_obj, snpinfo,
                    top_snps_tbl, gene_exon_tbl, snp_action)
    
    sum_top_pat <- shiny::reactive({
      summary(shiny::req(top_snps_tbl()))
    })
    
    chr_id <- reactive({
      stringr::str_split(shiny::req(chr_pos()), "_")[[1]][1]
    })
    
    output$snp_pattern_table <- DT::renderDataTable({
      sum_top_pat()
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
    
    dropHilit <- reactive({
      max(0,
          max(unclass(shiny::req(snp_scan_obj()))) - 
            shiny::req(snp_par$minLOD))
    })
    
    output$snp_pattern_plot <- shiny::renderPlot({
      if(is.null(snp_par$pheno_name) | is.null(snp_scan_obj()) |
         is.null(snp_par$scan_window) | is.null(snp_action()) |
         is.null(snpinfo()) | is.null(chr_id()))
        return(plot_null())
      shiny::withProgress(message = 'SNP pattern plots ...', value = 0, {
        shiny::setProgress(1)
        top_pat_plot(snp_par$pheno_name, 
                     snp_scan_obj(), 
                     chr_id(),
                     snpinfo(),
                     snp_par$scan_window,
                     drop_hilit = dropHilit(),
                     snp_action = snp_action())
      })
    })
    
    output$snp_pattern_plotly <- plotly::renderPlotly({
      if(is.null(snp_par$pheno_name) | is.null(snp_scan_obj()) |
         is.null(snp_par$scan_window) | is.null(snp_action()) |
         is.null(snpinfo()) | is.null(chr_id()))
        return(plot_null())
      shiny::withProgress(message = 'SNP pattern plots ...', value = 0, {
        shiny::setProgress(1)
        top_pat_plot(snp_par$pheno_name, 
                     snp_scan_obj(), 
                     chr_id(),
                     snpinfo(),
                     snp_par$scan_window,
                     drop_hilit = dropHilit(),
                     snp_action = snp_action(),
                     lines = FALSE, cex = 2)
      })
    })
    
    ## SNP Pheno patterns
    output$snp_phe_pat <- shiny::renderPlot({
      if(is.null(pheno_names()) | is.null(snp_scan_obj()) |
         is.null(snp_par$scan_window) | is.null(snp_action()))
        return(plot_null())
      shiny::withProgress(message = 'SNP Pheno patterns ...', value = 0, {
        shiny::setProgress(1)
        top_pat_plot(pheno_names(), 
                     snp_scan_obj(), 
                     chr_id(),
                     snpinfo(),
                     snp_par$scan_window,
                     drop_hilit = dropHilit(),
                     facet = "pheno", 
                     snp_action = snp_action())
      })
    })
    
    haplos <- reactive({
      shiny::req(allele_info())$code
    })
    output$pattern <- shiny::renderUI({
      shiny::req(snp_action())
      top_pat <- shiny::req(top_snps_tbl())
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
      if(is.null(pheno_names()) | is.null(snp_scan_obj()) |
         is.null(snp_par$scan_window) | is.null(snp_action()))
        return(plot_null())
      #     shiny::req(input$pattern)
      top_pat <- shiny::req(top_snps_tbl())
      patterns <- qtl2pattern::sdp_to_pattern(top_pat$sdp, haplos())
      shiny::withProgress(message = 'SNP Pattern phenos ...', value = 0, {
        shiny::setProgress(1)
        top_pat_plot(pheno_names(), 
                     snp_scan_obj(), 
                     chr_id(),
                     snpinfo(), 
                     snp_par$scan_window,
                     drop_hilit = dropHilit(),
                     facet = "pattern", 
                     snp_action = snp_action())
      })
    })
    
    output$pat_input <- shiny::renderUI({
      switch(shiny::req(input$button),
             #           "All Patterns" = shiny::uiOutput(ns("pattern")),
             "Top SNPs"     = snpFeatureInput(ns("top_feature")))
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
        file.path(paste0("pattern_", chr_pos(), "_", snp_action(), ".csv")) },
      content = function(file) {
        utils::write.csv(sum_top_pat(), file)
      }
    )
    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        file.path(paste0("pattern_", chr_pos(), "_", snp_action(), ".pdf")) },
      content = function(file) {
        scans <- shiny::req(snp_scan_obj())
        snp_w <- shiny::req(snp_par$scan_window)
        phenos <- shiny::req(pheno_names())
        grDevices::pdf(file, width = 9)
        ## Plots over all phenotypes
        print(top_pat_plot(phenos, 
                           scans, 
                           chr_id(),
                           snpinfo(), 
                           snp_w,
                           drop_hilit = dropHilit(),
                           facet = "pheno", 
                           snp_action = snp_action()))
        
        print(top_pat_plot(pheno_names(), 
                           snp_scan_obj(), 
                           chr_id(),
                           snpinfo(), 
                           snp_par$scan_window,
                           drop_hilit = dropHilit(),
                           facet = "pattern", 
                           snp_action = snp_action()))
        
        ## Plots by phenotype.
        for(pheno in phenos) {
          print(top_pat_plot(pheno, 
                             scans, 
                             chr_id(),
                             snpinfo(), 
                             snp_w, 
                             drop_hilit = dropHilit(),
                             snp_action = snp_action()))
        }
        grDevices::dev.off()
      }
    )
    output$button_input <- shiny::renderUI({
      button_val <- c("All Phenos","All Patterns",
                      "By Pheno",
                      "Top SNPs","Interactive")
      if(length(pheno_names()) == 1) {
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
    shiny::observeEvent(pheno_names(), {
      button_val <- c("All Phenos","All Patterns",
                      "By Pheno",
                      "Top SNPs")
      if(length(pheno_names()) == 1) {
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
#' @rdname snpPatternServer
snpPatternInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("button_input")),
    shiny::uiOutput(ns("pat_input"))
  )
}
#' @export
#' @rdname snpPatternServer
snpPatternUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("download_csv_plot"))
}
#' @export
#' @rdname snpPatternServer
snpPatternOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("pat_output")),
    DT::dataTableOutput(ns("snp_pattern_table")))
}

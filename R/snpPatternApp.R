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
snpPatternApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test SNP Pattern",
    sidebar = bslib::sidebar(
      projectUI("project"),            # project
      setParInput("set_par"),          # class
      setupInput("setup"),             # 
      setupUI("setup"),                # <various>
      hapParUI("hap_par"),             # button
      hapParInput("hap_par"),          # sex_type
      snpListInput("snp_list"),        # scan_window
      snpListInput2("snp_list"),       # minLOD
      snpListUI("snp_list"),           # pheno_name
      snpPatternInput("snp_pattern")), # button_input pat_input
    bslib::card(snpPatternUI("snp_pattern")),
    bslib::card(snpPatternOutput("snp_pattern"))
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    
    pmap_obj <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "pmap")
    })

    # set_list returns pheno_names(), win_par.
    set_list <- setupServer("setup", set_par, peak_df, pmap_obj, project_df)
    
    pheno_mx <- shiny::reactive({
      shiny::req(project_df(), set_par$class)
      pheno_names <- shiny::req(set_list$pheno_names())
      read_project(project_df(), "pheno", class = set_par$class,
                   columns = pheno_names)
    })
    covar_df <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "covar")
    })
    K_chr <- shiny::reactive({
      shiny::req(project_df())
      chr_id <- shiny::req(set_list$win_par$chr_id)
      read_project(project_df(), "kinship")[chr_id]
    })
    ## Allele names.
    allele_info <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "allele_info")
    })
    
    hap_par <- hapParServer("hap_par")
    # `snp_list` returns many objects in list.
    snp_list <- snpListServer("snp_list", hap_par, set_list$win_par,
                              peak_df, pheno_mx, covar_df, K_chr, project_df)
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
    
    chr_id <- reactive({
      stringr::str_split(shiny::req(snp_list$chr_pos()), "_")[[1]][1]
    })
    
    output$snp_pattern_table <- DT::renderDataTable({
      sum_top_pat()
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
    
    dropHilit <- reactive({
      max(0,
          max(unclass(shiny::req(snp_list$snp_scan_obj()))) - 
            shiny::req(snp_list$snp_par$minLOD))
    })
    
    output$snp_pattern_plot <- shiny::renderPlot({
      if(is.null(snp_list$snp_par$pheno_name) | is.null(snp_list$snp_scan_obj()) |
         is.null(snp_list$snp_par$scan_window) | is.null(snp_list$snp_action()) |
         is.null(snp_list$snpinfo()) | is.null(chr_id()))
        return(plot_null())
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
      if(is.null(snp_list$snp_par$pheno_name) | is.null(snp_list$snp_scan_obj()) |
         is.null(snp_list$snp_par$scan_window) | is.null(snp_list$snp_action()) |
         is.null(snp_list$snpinfo()) | is.null(chr_id()))
        return(plot_null())
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
      if(is.null(snp_list$pheno_names()) | is.null(snp_scan_obj()) |
         is.null(snp_par$scan_window) | is.null(snp_action()))
        return(plot_null())
      shiny::withProgress(message = 'SNP Pheno patterns ...', value = 0, {
        shiny::setProgress(1)
        top_pat_plot(snp_list$pheno_names(), 
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
      if(is.null(snp_list$pheno_names()) | is.null(snp_list$snp_scan_obj()) |
         is.null(snp_list$snp_par$scan_window) | is.null(snp_list$snp_action()))
        return(plot_null())
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
        file.path(paste0("pattern_", snp_list$chr_pos(), "_", snp_action(), ".csv")) },
      content = function(file) {
        utils::write.csv(sum_top_pat(), file)
      }
    )
    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        file.path(paste0("pattern_", snp_list$chr_pos(), "_", snp_action(), ".pdf")) },
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
    shiny::uiOutput(ns("button_input")),
    shiny::uiOutput(ns("pat_input"))
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

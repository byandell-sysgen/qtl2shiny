#' Shiny Pattern module
#'
#' Shiny module for SNP pattern plots, with interfaces \code{patternUI} and  \code{patternOutput}.
#'
#' @param id identifier for shiny reactive
#' @param job_par,chr_pos,win_par,phe_mx,cov_df,pairprobs_obj,K_chr,analyses_df,patterns,project_info,allele_info,snp_action reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' 
#' @importFrom qtl2pattern sdp_to_pattern
#' @importFrom dplyr filter
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny checkboxInput column downloadButton downloadHandler
#'             fluidRow moduleServer NS observeEvent plotOutput radioButtons
#'             reactive renderPlot renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom grDevices dev.off pdf
#' @importFrom utils write.csv
#' @importFrom rlang .data
patternServer <- function(id, job_par, chr_pos, win_par,
                         phe_mx, cov_df, pairprobs_obj, K_chr, analyses_df,
                         patterns, project_info, allele_info, 
                         snp_action = shiny::reactive({NULL})) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    phe1_mx <- reactive({
      phe_mx()[, shiny::req(input$pheno_name), drop = FALSE]
    })
    shinyAllele("alleles", win_par,  phe1_mx, cov_df, pairprobs_obj, K_chr,
                analyses_df, patterns, scan_pat, project_info, snp_action)
    
    ## Select phenotype for plots.
    output$pheno_name <- shiny::renderUI({
      shiny::selectInput(ns("pheno_name"), NULL,
                         choices = colnames(shiny::req(phe_mx())),
                         selected = input$pheno_name)
    })
    ## Select pattern for plots.
    pats <- shiny::reactive({
      shiny::req(input$pheno_name, patterns())
      pull_patterns(patterns(), colnames(shiny::req(phe_mx())))
    })
    haplos <- reactive({
      shiny::req(allele_info())$code
    })
    pattern_choices <- shiny::reactive({
      qtl2pattern::sdp_to_pattern(pats()$sdp, haplos())
    })
    output$pattern <- shiny::renderUI({
      shiny::req(pattern_choices(), snp_action())
      choices <- pattern_choices()
      if(!length(choices)) {
        choices <- input$pattern
      }
      shiny::selectInput(ns("pattern"), NULL,
                         choices = choices,
                         selected = input$pattern)
    })
    shiny::observeEvent(patterns(), update_patterns())
    shiny::observeEvent(input$pheno_name, update_patterns())
    update_patterns <- function() {
      shiny::req(snp_action(), input$pheno_name, patterns())
      pats <- dplyr::filter(patterns(), .data$pheno == input$pheno_name)
      if(nrow(pats)) {
        choices <- qtl2pattern::sdp_to_pattern(pats$sdp, haplos())
      } else {
        choices <- input$pattern
      }
      if(!is.null(selected <- input$pattern)) {
        if(!(selected %in% choices))
          selected <- choices[1]
      }
      shiny::updateSelectInput(session, "pattern", NULL,
                               choices, selected)
    }
    
    scan_pat <- shiny::reactive({
      req(snp_action())
      pheno_in <- shiny::req(input$pheno_name)
      shiny::req(phe_mx(), cov_df(), pairprobs_obj(), K_chr(),
                 pats(), analyses_df(), job_par$sex_type)
      withProgress(message = 'Scan Patterns ...', value = 0, {
        setProgress(1)
        scan1_pattern(pheno_in, phe_mx(), cov_df(), 
                      pairprobs_obj(), K_chr(), analyses_df(),
                      pats(), job_par$sex_type, input$blups)
      })
    })
    
    output$scan_pat_lod <- shiny::renderPlot({
      if(is.null(scan_pat()))
        return(plot_null())
      shiny::req(scan_pat(), pattern_choices(), input$pheno_name, pairprobs_obj())
      withProgress(message = 'Pattern LODs ...', value = 0, {
        setProgress(1)
        scan_pat_type(scan_pat(), pairprobs_obj()$map, "lod", pattern_choices(), 
                      input$pheno_name, haplos())
      })
    })
    output$scan_pat_coef <- shiny::renderPlot({
      if(is.null(scan_pat()))
        return(plot_null())
      shiny::req(scan_pat(), pattern_choices(), input$pheno_name, pairprobs_obj())
      withProgress(message = 'Pattern Effects ...', value = 0, {
        setProgress(1)
        scan_pat_type(scan_pat(), pairprobs_obj()$map, "coef", pattern_choices(), 
                      input$pheno_name, haplos())
      })
    })
    output$scanSummary <- DT::renderDataTable({
      shiny::req(scan_pat())
      withProgress(message = 'Pattern summary ...', value = 0, {
        setProgress(1)
        summary(scan_pat(), pairprobs_obj()$map)
      })
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
    
    output$eff_lodPlot <- shiny::renderPlot({
      if(is.null(scan_pat()))
        return(plot_null())
      shiny::req(scan_pat(), pattern_choices(), input$pheno_name, pairprobs_obj())
      withProgress(message = 'Pattern Effects & LOD ...', value = 0, {
        setProgress(1)
        scan_pat_type(scan_pat(), pairprobs_obj()$map, "coef_and_lod", pattern_choices(), 
                      input$pheno_name, haplos())
      })
    })
    
    output$pattern_choice <- shiny::renderUI({
      switch(shiny::req(input$button),
             LOD =,
             "LOD & Effects" =,
             Effects = shiny::uiOutput(ns("pattern")))
    })
    
    output$LOD <- shiny::renderUI({
      switch(shiny::req(input$button),
             LOD             = shiny::plotOutput(ns("scan_pat_lod")))
    })
    output$Effects <- shiny::renderUI({
      switch(shiny::req(input$button),
             Effects         = shiny::plotOutput(ns("scan_pat_coef")))
    })
    output$Both <- shiny::renderUI({
      switch(shiny::req(input$button),
             "LOD & Effects" = shiny::plotOutput(ns("eff_lodPlot")))
    })
    output$Means <- shiny::renderUI({
      switch(shiny::req(input$button),
             "Allele Means"  = shinyAlleleOutput(ns("alleles")))
    })
    output$Summary <- shiny::renderUI({
      switch(shiny::req(input$button),
             Summary = DT::dataTableOutput(ns("scanSummary")))
    })
    
    ## Downloads
    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        pheno_in <- shiny::req(input$pheno_name)
        file.path(paste0(pheno_in, "_", snp_action(), "_effects_", chr_pos(), ".csv"))
      },
      content = function(file) {
        scan_in <- shiny::req(scan_pat())
        shiny::req(pairprobs_obj())
        utils::write.csv(summary(scan_in, pairprobs_obj()$map), file)
      }
    )
    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        shiny::req(input$pheno_name)
        file.path(paste0("pattern", "_", snp_action(), "_scan_", chr_pos(), ".pdf"))
      },
      content = function(file) {
        shiny::req(scan_pat(), pattern_choices(), job_par$sex_type)
        scan_in <- shiny::req(scan_pat())
        top_panel_prop <- 0.65
        grDevices::pdf(file, width = 9, height = 9)
        for(pheno_in in colnames(phe_mx())) {
          pats <- dplyr::filter(patterns(), .data$pheno == pheno_in)
          pat_choices <- qtl2pattern::sdp_to_pattern(pats$sdp, haplos())
          
          scan_now <- scan1_pattern(pheno_in, phe_mx(), cov_df(), 
                                    pairprobs_obj(), K_chr(), analyses_df(),
                                    pats, job_par$sex_type, input$blups)
          
          scan_pat_type(scan_now, pairprobs_obj()$map, "coef_and_lod", pat_choices,
                        pheno_in, haplos())
        }
        grDevices::dev.off()
      }
    )
    output$radio <- shiny::renderUI({
      shiny::radioButtons(ns("button"), "",
                          c("LOD","Effects","LOD & Effects","Allele Means","Summary"),
                          input$button)
    })
    output$blups <- shiny::renderUI({
      shiny::checkboxInput(ns("blups"), "BLUPs?")
    })
    output$select <- shiny::renderUI({
      switch(shiny::req(input$button),
             "Allele Means"  = shinyAlleleUI(ns("alleles")),
             uiOutput(ns("patterndown")))
      
    })
    output$patterndown <- shiny::renderUI({
      shiny::fluidRow(
        shiny::column(6, shiny::downloadButton(ns("downloadData"), "CSV")),
        shiny::column(6, shiny::downloadButton(ns("downloadPlot"), "Plots")))
    })
  })
}
#' @export
#' @rdname patternServer
patternUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("radio"))),
      shiny::column(6, shiny::uiOutput(ns("blups")))),
    shiny::uiOutput(ns("pheno_name")),
    shiny::uiOutput(ns("select")))
}
#' @export
#' @rdname patternServer
patternOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("LOD")),
    shiny::uiOutput(ns("Effects")),
    shiny::uiOutput(ns("Both")),
    shiny::uiOutput(ns("Means")),
    shiny::uiOutput(ns("Summary"))
  )
}

#' Shiny coefficient analysis and plot module
#'
#' Shiny module for scan1 coefficient plots, with interfaces \code{mediateUI} and  \code{mediateOutput}.
#'
#' @param id identifier for shiny reactive
#' @param job_par,win_par,patterns,phe_mx,cov_df,probs_obj,K_chr,analyses_df,pmap_obj,covar,analyses_tbl,peak_df,project_info,allele_info reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom qtl2 find_marker
#' @importFrom qtl2mediate comediator_region comediator_type expr_region
#'             mediation_test_qtl2
#' @importFrom ggplot2 autoplot geom_point
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny checkboxInput column downloadButton downloadHandler
#'             fluidRow isolate isTruthy moduleServer NS observeEvent plotOutput
#'             radioButtons reactive renderPlot renderUI req selectInput
#'             setProgress sliderInput strong tagList uiOutput updateSelectInput
#'             updateSliderInput withProgress
#' @importFrom plotly renderPlotly plotlyOutput
#' @importFrom dplyr filter
#' @importFrom utils write.csv
#' @importFrom grDevices dev.off pdf
#' @importFrom rlang .data
#' 
mediateServer <- function(id,
  job_par, win_par, patterns, phe_mx, cov_df, probs_obj, K_chr,
  analyses_df, pmap_obj, covar, analyses_tbl, peak_df,
  project_info, allele_info) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    chr_id <- reactive({
      shiny::req(win_par$chr_id)
    })
    scan_window <- reactive({
      shiny::req(win_par)
      win_par$peak_Mbp + c(-1,1) * win_par$window_Mbp
    })
    ## Expression data
    expr_ls <- reactive({
      shiny::req(win_par)
      qtl2mediate::expr_region(chr_id(), scan_window(), covar(),
                               shiny::req(pmap_obj()), 
                               drivers = shiny::req(input$qtls),
                               query_mrna = query_mrna())
    })
    query_mrna <- reactive({
      read_query_rds(project_info(), "query_mrna.rds")
    })
    pheno_data <- reactive({
      pheno_read(project_info(), analyses_tbl())
    })
    
    ## Comediator data
    comed_ls <- reactive({
      shiny::req(input$pheno_name, win_par, project_info())
      qtl2mediate::comediator_region(
        input$pheno_name, chr_id(), scan_window(), covar(), analyses_tbl(),
        peak_df(), shiny::req(input$qtls), shiny::req(pmap_obj()), pheno_data())
    })
    med_ls <- reactive({
      out <- switch(shiny::req(input$med_type, input$pheno_name),
        expression = expr_ls(),
        phenotype = qtl2mediate::comediator_type(
          comed_ls(), shiny::req(peak_df()), input$pheno_name,
          shiny::isTruthy(input$other)))
      out
    })
    
    # Get genotype matrix and map at 
    peak_mar <- reactive({
      qtl2::find_marker(probs_obj()$map, chr_id(), input$pos_Mbp)
    })
    geno_max <- reactive({
      shiny::req(input$pos_Mbp, probs_obj())
      subset(probs_obj()$probs, chr = chr_id(), mar = peak_mar())[[1]][,,1]
    })
    
    # Select pattern 
    sdps <- shiny::reactive({
      shiny::req(patterns(), input$pheno_name)
      unique(dplyr::filter(patterns(), .data$pheno == input$pheno_name)$sdp)
    })
    haplos <- reactive({
      shiny::req(allele_info())$code
    })
    choices_pattern <- reactive({
      shiny::req(sdps(), haplos())
      qtl2pattern::sdp_to_pattern(sdps(), haplos())
    })
    shiny::observeEvent(shiny::req(input$checkplot, choices_pattern()), {
      choices <- choices_pattern()
      selected <- input$pattern
      if(is.null(selected)) {
        selected <- choices[1]
      }
      if(!(selected %in% choices)) {
        selected <- choices[1]
      }
      shiny::updateSelectInput(session, "pattern",
                               choices = choices, selected = selected)
    })
    sdp <- reactive({
      shiny::req(input$pattern)
      choices <- choices_pattern()
      sdps()[match(input$pattern, choices, nomatch = 1)]
    })
    
    ## Triad Plots
    triadServer("triad", input, patterns, geno_max, peak_mar, med_ls,
                mediate_signif, phe1_mx, cov_df, K_chr, probs_obj, chr_id, sdp)
    
    ## Mediate1
    probs_chr <- reactive({
      probs_obj()$probs[[chr_id()]]
    })
    mediate_obj <- shiny::reactive({
      shiny::req(phe1_mx(), probs_obj(), K_chr(), cov_df(), geno_max(), 
                 input$pos_Mbp, input$med_type, med_ls())
      shiny::withProgress(message = "Mediation Scan ...", value = 0, {
        shiny::setProgress(1)
        qtl2mediate::mediation_test_qtl2(
          target = phe1_mx(),
          mediator = med_ls()[[1]],
          annotation = med_ls()[[2]],
          covar_tar = cov_df(),
          covar_med = med_ls()$covar,
          genoprobs = probs_obj()$probs,
          map = probs_obj()$map,
          chr = chr_id(),
          pos = input$pos_Mbp,
          kinship = K_chr())
      })
    })
    mediate_signif <- shiny::reactive({
      out <- shiny::req(mediate_obj())
      out$best <- dplyr::filter(out$best, .data$pvalue <= 0.1)
      class(out) <- class(mediate_obj())
      out
    })
    
    phe1_mx <- reactive({
      shiny::req(phe_mx())
      phename <- shiny::req(input$pheno_name)
      if(phename %in% colnames(phe_mx())) {
        phe_mx()[, phename, drop = FALSE]
      } else {
        NULL
      }
    })
    ## Select phenotype for plots.
    output$pheno_name_input <- shiny::renderUI({
      shiny::req(phe_mx())
      shiny::selectInput(ns("pheno_name"), NULL,
                         choices = colnames(phe_mx()),
                         selected = input$pheno_name)
    })
    ## Select plot format.
    output$med_plot_input <- shiny::renderUI({
      shiny::selectInput(ns("med_plot"), NULL,
                         choices = c("Position by LR", 
                                     "Position by P-value", 
                                     "P-value by LR",
                                     "Allele Effects",
                                     "Mediator Effects"),
                         selected = input$med_plot)
    })
    ## Select type of mediation.
    output$med_type_input <- shiny::renderUI({
      shiny::selectInput(ns("med_type"), NULL,
                         choices = c("phenotype","expression"),
                         selected = input$med_type)
    })
    
    med_plot_type <- reactive({
      switch(shiny::req(input$med_plot),
             "Position by LR" = "pos_LR",
             "Position by P-value" = "pos_pvalue",
             "P-value by LR" = "pvalue_LR",
             "Allele Effects" = "alleles",
             "Mediator Effects" = "mediator")
    })
    ## Mediate1 plot
    output$medPlot <- shiny::renderPlot({
      if(!shiny::isTruthy(med_ls()) || !shiny::isTruthy(mediate_obj())) {
        plot_null("too much\nmissing data\nin mediators\nreduce window width")
      } else {
        shiny::req(med_plot_type(), mediate_obj())
        shiny::withProgress(message = 'Mediation Plot ...', value = 0, {
          shiny::setProgress(1)
          ggplot2::autoplot(
            mediate_obj(), med_plot_type(),
            local_only = input$local, 
            significant = input$signif) +
            ggplot2::geom_point(size = 4)
        })
      }
    })
    ## Mediate1 plotly
    output$medPlotly <- plotly::renderPlotly({
      shiny::req(mediate_obj())
      shiny::withProgress(message = 'Mediation Plotly ...', value = 0, {
        shiny::setProgress(1)
        ggplot2::autoplot(
          mediate_signif(), med_plot_type(),
          local_only = input$local, 
          significant = TRUE)
      })
    })
    
    output$medSummary <- DT::renderDataTable({
      shiny::req(mediate_obj())$best
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
    
    # Scan Window slider
    output$pos_Mbp_input <- shiny::renderUI({
      map <- shiny::req(probs_obj())$map[[chr_id()]]
      rng <- round(2 * range(map)) / 2
      if(is.null(selected <- input$pos_Mbp))
        selected <- req(win_par$peak_Mbp)
      shiny::sliderInput(ns("pos_Mbp"), NULL, rng[1], rng[2],
                         selected, step=.1)
    })
    ## Reset pos_Mbp if chromosome changes.
    observeEvent(chr_id(), {
      map <- shiny::req(probs_obj()$map)
      rng <- round(2 * range(map[[chr_id()]])) / 2
      shiny::updateSliderInput(session, "pos_Mbp", NULL, 
                               req(win_par$peak_Mbp), 
                               rng[1], rng[2], step=.1)
    })
    
    output$out_choice <- shiny::renderUI({
      switch(shiny::req(input$button),
             Static      = shiny::plotOutput(ns("medPlot")),
             Interactive = plotly::plotlyOutput(ns("medPlotly")))
    })
    output$qtls_input <- shiny::renderUI({
      if(is.null(selected <- input$qtls))
        selected <- 2
      shiny::radioButtons(ns("qtls"), "",
                          c("1 QTL" = 1, "2 QTLs" = 2),
                          selected, inline = TRUE)
    })
    output$radio <- shiny::renderUI({
      shiny::radioButtons(ns("button"), "",
                          c("Static","Interactive"),
                          "Static")
    })
    output$checkplot_input <- shiny::renderUI({
      shiny::checkboxInput(ns("checkplot"), "Triad Plot", input$checkplot)
    })
    output$local_other <- shiny::renderUI({
      switch(shiny::req(input$med_type),
        expression = shiny::checkboxInput(ns("local"), "Local?", input$local),
        phenotype  = shiny::checkboxInput(ns("other"), "Other types?",
                                          input$other))
    })
    output$signif_input <- shiny::renderUI({
      if(shiny::isTruthy(input$signif)) {
        value <- input$signif
      } else {
        value <- TRUE
      }
      shiny::checkboxInput(ns("signif"), "Significant?", value)
    })
    
    ## Downloads.
    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        file.path(paste0("mediate_", chr_id(), "_", win_par$peak_Mbp, ".csv"))
      },
      content = function(file) {
        shiny::req(mediate_obj())
        utils::write.csv(mediate_obj()$best, file)
      }
    )
    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        file.path(paste0("mediate_", chr_id(), "_", win_par$peak_Mbp, ".pdf"))
      },
      content = function(file) {
        shiny::req(phe_mx(), geno_max(), K_chr(), cov_df(),
                   input$pos_Mbp, input$med_type)
        grDevices::pdf(file, width=9,height=9)
        for(pheno in colnames(phe_mx())) {
          med <- qtl2mediate::mediation_test_qtl2(
            target = phe_mx()[, pheno, drop = FALSE],
            mediator = med_ls()[[1]],
            annotation = med_ls()[[2]],
            covar_tar = cov_df(),
            covar_med = med_ls()$covar,
            genoprobs = probs_obj()$probs,
            map = probs_obj()$map,
            chr = chr_id(),
            pos = input$pos_Mbp,
            kinship = K_chr())
          
          print(ggplot2::autoplot(
            med, "pos_LR",
            local_only = input$local, 
            significant = input$signif))
          print(ggplot2::autoplot(
            med, "pos_pvalue",
            local_only = input$local, 
            significant = TRUE))
          print(ggplot2::autoplot(
            med, "pvalue_LR",
            local_only = input$local, 
            significant = TRUE))
          print(ggplot2::autoplot(
            med, "mediator",
            local_only = input$local, 
            significant = TRUE))
        }
        grDevices::dev.off()
      })
    output$mediation <- renderUI({
      shiny::tagList(
        shiny::uiOutput(ns("qtls_input")),
        shiny::uiOutput(ns("radio")),
        shiny::uiOutput(ns("pheno_name_input")),
        shiny::uiOutput(ns("med_type_input")),
        shiny::fluidRow(
          shiny::column(6, shiny::uiOutput(ns("signif_input"))),
          shiny::column(6, shiny::uiOutput(ns("local_other")))),
        shiny::uiOutput(ns("med_plot_input")),
        shiny::uiOutput(ns("pos_Mbp_input")),
        shiny::fluidRow(
          shiny::column(6, shiny::downloadButton(ns("downloadData"), "CSV")),
          shiny::column(6, shiny::downloadButton(ns("downloadPlot"), "Plots"))))
    })
    output$medUI <- shiny::renderUI({
      switch(1 + shiny::isTruthy(input$checkplot),
             {
               shiny::uiOutput(ns("mediation"))
             },
             {
               shiny::tagList(
                 shiny::selectInput(ns("pattern"), NULL,
                                    choices_pattern(),
                                    shiny::isolate(input$pattern)),
                 triadUI(ns("triad")))
             })
    })
    output$medOutput <- shiny::renderUI({
      if(shiny::isTruthy(input$checkplot))
        triadOutput(ns("triad"))
      else
        shiny::tagList(
          shiny::uiOutput(ns("out_choice")),
          DT::dataTableOutput(ns("medSummary")))
    })
  })
}
#' @export
#' @rdname mediateServer
mediateUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::strong("Mediation"),
    shiny::uiOutput(ns("checkplot_input")),
    shiny::uiOutput(ns("medUI")))
}
#' @export
#' @rdname mediateServer
mediateOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("medOutput"))
}

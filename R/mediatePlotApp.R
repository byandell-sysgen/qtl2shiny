#' Shiny Mediate Plot App
#'
#' @param id identifier for shiny reactive
#' @param hotspot_list,probs_obj,patterns,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom qtl2 find_marker
#' @importFrom qtl2mediate expr_region mediation_test_qtl2
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
#' @importFrom bslib card layout_sidebar navset_tab nav_panel page_navbar sidebar
mediatePlotApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Mediate Plot",
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
      title = "Mediate Plot",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          snpListInput("snp_list"),           # scan_window
          snpListInput2("snp_list"),          # minLOD
          snpListUI("snp_list"),              # pheno_name
          mediatePlotInput("mediate plot")),  # checkplot, pattern
        bslib::card(mediatePlotOutput("mediate plot"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    probs_obj <- probsServer("probs", hotspot_list$win_par, project_df)
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    mediatePlotServer("mediate plot", hotspot_list, snp_list, probs_obj, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname mediatePlotApp
mediatePlotServer <- function(id, hotspot_list, snp_list, probs_obj, project_df) {
  #** need to check; also eliminate `analyses_df`.
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    patterns <- shiny::reactive(snp_list$patterns)
    win_par <- shiny::isolate(hotspot_list$win_par)
    chr_id <- shiny::reactive({
      shiny::req(win_par()$chr_id[1])
    })
    peak_Mbp <- shiny::reactive({
      shiny::req(win_par()$peak_Mbp[1])
    })
    window_Mbp <- shiny::reactive({
      shiny::req(win_par()$window_Mbp)
    })
    scan_window <- shiny::reactive({
      shiny::req(win_par())
      peak_Mbp() + c(-1,1) * window_Mbp()
    })
    ## Expression data
    expr_ls <- shiny::reactive({
      shiny::req(win_par())
      shiny::req(input$qtls, chr_id(), scan_window(), 
                 hotspot_list$covar_df(), hotspot_list$pmap_obj(), query_mrna())
      qtl2mediate::expr_region(chr_id(), scan_window(), hotspot_list$covar_df(),
                               hotspot_list$pmap_obj(), 
                               drivers = input$qtls,
                               query_mrna = query_mrna())
    })
    query_mrna <- shiny::reactive({
      read_query_rds(project_df(), "query_mrna.rds")
    })
    pheno_data <- shiny::reactive({
      read_pheno(shiny::req(project_df()), shiny::req(hotspot_list$set_par$class))
    })
    
    ## Comediator data
    comed_ls <- shiny::reactive({
      shiny::req(input$pheno_name, input$qtls, chr_id(), scan_window(), 
                 hotspot_list$covar_df(), hotspot_list$peak_df(),
                 hotspot_list$pmap_obj(), pheno_data())
      comediator_region(input$pheno_name, chr_id(), scan_window(),
        hotspot_list$covar_df(), hotspot_list$peak_df(), input$qtls,
        hotspot_list$pmap_obj(), pheno_data())
    })
    med_ls <- shiny::reactive({
      out <- switch(shiny::req(input$med_type),
        expression = expr_ls(),
        phenotype = comediator_type(
          shiny::req(comed_ls()), shiny::req(hotspot_list$peak_df()),
          shiny::req(input$pheno_name), shiny::isTruthy(input$other)))
      out
    })
    
    # Select pattern 
    sdps <- shiny::reactive({
      shiny::req(patterns(), input$pheno_name)
      unique(dplyr::filter(patterns(), .data$pheno == input$pheno_name)$sdp)
    })
    haplos <- shiny::reactive({
      shiny::req(hotspot_list$allele_info())$code
    })
    choices_pattern <- shiny::reactive({
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
    sdp <- shiny::reactive({
      shiny::req(input$pattern)
      choices <- choices_pattern()
      sdps()[match(input$pattern, choices, nomatch = 1)]
    })
    
    ## Triad Plots
    triadServer("triad", input, patterns, med_ls,
                mediate_signif, phe1_mx, hotspot_list$covar_df, hotspot_list$kinship_list, probs_obj, chr_id, sdp)
    
    ## Mediate1
    probs_chr <- shiny::reactive({
      probs_obj()$probs[[chr_id()]]
    })
    mediate_obj <- shiny::reactive({
      shiny::req(phe1_mx(), probs_obj(), hotspot_list$kinship_list(), hotspot_list$covar_df(), geno_max(), 
                 input$pos_Mbp, input$med_type, med_ls())
      shiny::withProgress(message = "Mediation Scan ...", value = 0, {
        shiny::setProgress(1)
        qtl2mediate::mediation_test_qtl2(
          target = phe1_mx(),
          mediator = med_ls()[[1]],
          annotation = med_ls()[[2]],
          covar_tar = hotspot_list$covar_df(),
          covar_med = med_ls()$covar,
          genoprobs = probs_obj()$probs,
          map = probs_obj()$map,
          chr = chr_id(),
          pos = input$pos_Mbp,
          kinship = hotspot_list$kinship_list())
      })
    })
    mediate_signif <- shiny::reactive({
      out <- shiny::req(mediate_obj())
      out$best <- 
        # Rename `qtl_pos` as `pos` for `intermediate::ggplot_mediation_test`.
        dplyr::rename(
          # Filter out n.s. entries.
          dplyr::filter(out$best, .data$pvalue <= 0.1),
          pos = "qtl_pos")
      class(out) <- class(mediate_obj())
      out
    })
    
    phe1_mx <- shiny::reactive({
      shiny::req(hotspot_list$pheno_mx())
      phename <- shiny::req(input$pheno_name)
      if(phename %in% colnames(hotspot_list$pheno_mx())) {
        hotspot_list$pheno_mx()[, phename, drop = FALSE]
      } else {
        NULL
      }
    })
    ## Select phenotype for plots.
    output$pheno_name_input <- shiny::renderUI({
      shiny::req(hotspot_list$pheno_mx())
      shiny::selectInput(ns("pheno_name"), NULL,
                         choices = colnames(hotspot_list$pheno_mx()),
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
    
    med_plot_type <- shiny::reactive({
      switch(shiny::req(input$med_plot),
             "Position by LR" = "pos_LR",
             "Position by P-value" = "pos_pvalue",
             "P-value by LR" = "pvalue_LR",
             "Allele Effects" = "alleles",
             "Mediator Effects" = "mediator")
    })
    ## Mediate1 plot
    output$mediate_plot <- shiny::renderPlot({
      if(!shiny::isTruthy(med_ls()) || !shiny::isTruthy(mediate_signif())) {
        plot_null("too much\nmissing data\nin mediators\nreduce window width")
      } else {
        shiny::req(med_plot_type(), mediate_signif())
        shiny::withProgress(message = 'Mediation Plot ...', value = 0, {
          shiny::setProgress(1)
          ggplot2::autoplot(
            mediate_signif(), med_plot_type(),
            local_only = input$local, 
            significant = input$signif) +
            ggplot2::geom_point(size = 4)
        })
      }
    })
    ## Mediate1 plotly
    output$mediate_plotly <- plotly::renderPlotly({
      shiny::req(mediate_signif())
      shiny::withProgress(message = 'Mediation Plotly ...', value = 0, {
        shiny::setProgress(1)
        ggplot2::autoplot(
          mediate_signif(), med_plot_type(),
          local_only = input$local, 
          significant = TRUE)
      })
    })
    
    output$mediate_table <- DT::renderDataTable({
      shiny::req(mediate_obj())$best
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
    
    # Scan Window slider
    output$pos_Mbp_input <- shiny::renderUI({
      map <- shiny::req(probs_obj())$map[[chr_id()]]
      rng <- round(2 * range(map)) / 2
      if(is.null(selected <- input$pos_Mbp))
        selected <- req(peak_Mbp())
      shiny::sliderInput(ns("pos_Mbp"), NULL, rng[1], rng[2],
                         selected, step=.1)
    })
    ## Reset pos_Mbp if chromosome changes.
    observeEvent(chr_id(), {
      map <- shiny::req(probs_obj()$map)
      rng <- round(2 * range(map[[chr_id()]])) / 2
      shiny::updateSliderInput(session, "pos_Mbp", NULL, 
                               req(peak_Mbp()), 
                               rng[1], rng[2], step=.1)
    })
    
    output$out_choice <- shiny::renderUI({
      switch(shiny::req(input$button),
             Static      = shiny::plotOutput(ns("mediate_plot")),
             Interactive = plotly::plotlyOutput(ns("mediate_plotly")))
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
    
    output$mediation <- shiny::renderUI({
      shiny::tagList(
        shiny::uiOutput(ns("qtls_input")),
        shiny::uiOutput(ns("radio")),
        shiny::uiOutput(ns("pheno_name_input")),
        shiny::uiOutput(ns("med_type_input")),
        shiny::fluidRow(
          shiny::column(6, shiny::uiOutput(ns("signif_input"))),
          shiny::column(6, shiny::uiOutput(ns("local_other")))),
        shiny::uiOutput(ns("med_plot_input")),
        shiny::uiOutput(ns("pos_Mbp_input")))
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
      else {
        bslib::navset_tab(
          id = ns("med_tab"),
          bslib::nav_panel("Plot", bslib::card(
            shiny::uiOutput(ns("out_choice")))),
          bslib::nav_panel("Summary", bslib::card(
            DT::dataTableOutput(ns("mediate_table")))))
      }
    })
  })
}
#' @export
#' @rdname mediatePlotApp
mediatePlotInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::strong("Mediation"),
    shiny::uiOutput(ns("checkplot_input")), # checkplot
    shiny::uiOutput(ns("medUI")))           # pattern
}
#' @export
#' @rdname mediatePlotApp
mediatePlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("medOutput"))
}

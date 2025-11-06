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
#' @importFrom shiny checkboxInput 
#'             isolate isTruthy moduleServer NS observeEvent plotOutput
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
            projectUI("project_df"),        # project
            hotspotInput("hotspot_list")),  # class, subject_model, pheno_names, hotspot
          bslib::card(
            hotspotUI("hotspot_list")),     # window_Mbp, radio, win_par, chr_ct, minLOD
          width = 400),
        hotspotOutput("hotspot_list"))
    ),
    bslib::nav_panel(
      title = "Mediate Plot",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          mediatePlotInput("mediate_plot"), # static, signif, local, med_plot
          mediateDataInput("mediate_list"), # qtls, pos_Mbp
          snpListInput("snp_list")),        # scan_window, minLOD, pheno_name
        bslib::card(mediatePlotOutput("mediate_plot"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project", projects_df)
    hotspot_list <- hotspotServer("hotspot_list", project_df)
    probs_obj <- probsServer("probs", hotspot_list$win_par, project_df)
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    mediate_list <- mediateDataServer("mediate_list", hotspot_list, snp_list,
                                      probs_obj, project_df)
    mediatePlotServer("mediate_plot", hotspot_list, mediate_list, probs_obj, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname mediatePlotApp
mediatePlotServer <- function(id, hotspot_list, mediate_list, probs_obj, project_df) {
  #** need to check; also eliminate `analyses_df`.
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    mediate_signif <- shiny::reactive({
      out <- shiny::req(mediate_list$mediate_obj())
      out$best <- 
        # Rename `qtl_pos` as `pos` for `intermediate::ggplot_mediation_test`.
        dplyr::rename(
          # Filter out n.s. entries.
          dplyr::filter(out$best, .data$pvalue <= 0.1),
          pos = "qtl_pos")
      class(out) <- class(mediate_list$mediate_obj())
      out
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
    # Translate `med_plot` into internal option.
    # ** Plots not correct.
    med_plot_type <- shiny::reactive({
      switch(shiny::req(input$med_plot),
             "Position by LR" = "pos_LR",
             "Position by P-value" = "pos_pvalue",
             "P-value by LR" = "pvalue_LR",
             "Allele Effects" = "alleles",
             "Mediator Effects" = "mediator")
    })
    ## Mediate1 plot
    output$mediate_plot_static <- shiny::renderPlot({
      if(!shiny::isTruthy(mediate_list$med_ls()) || !shiny::isTruthy(mediate_signif())) {
        plot_null("too much\nmissing data\nin mediators\nreduce window width")
      } else {
        shiny::req(med_plot_type(), mediate_signif())
        local <- shiny::isTruthy(input$local)
        signif <- shiny::isTruthy(input$signif)
        shiny::withProgress(message = 'Mediation Plot ...', value = 0, {
          shiny::setProgress(1)
          ggplot2::autoplot(
            mediate_signif(), med_plot_type(),
            local_only = local, 
            significant = signif) +
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
    
    output$static_input <- shiny::renderUI({
      shiny::radioButtons(ns("static"), "", c("Static","Interactive"), "Static")
    })
    istrue <- function(input_value)
      ifelse(shiny::isTruthy(input_value), input_value, TRUE)
    output$local_other <- shiny::renderUI({
      switch(shiny::req(input$med_type),
        expression = shiny::checkboxInput(ns("local"), "Local?",
                                          istrue(input$local)),
        phenotype  = shiny::checkboxInput(ns("other"), "Other types?",
                                          istrue(input$other)))
    })
    output$signif_input <- shiny::renderUI({
      shiny::checkboxInput(ns("signif"), "Significant?", istrue(input$signif))
    })
    
    output$mediate_input <- shiny::renderUI({
      shiny::tagList(
        shiny::uiOutput(ns("static_input")),
        shiny::fluidRow(
          shiny::column(6, shiny::uiOutput(ns("signif_input"))),
          shiny::column(6, shiny::uiOutput(ns("local_other")))),
        shiny::uiOutput(ns("med_plot_input")))
    })
    output$mediate_plot <- shiny::renderUI({
      switch(shiny::req(input$static),
             Static      = shiny::plotOutput(ns("mediate_plot_static")),
             Interactive = plotly::plotlyOutput(ns("mediate_plotly")))
    })
  })
}
#' @export
#' @rdname mediatePlotApp
mediatePlotInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("mediate_input"))    # static, signif, local, med_plot
}
#' @export
#' @rdname mediatePlotApp
mediatePlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("mediate_plot"))     # mediate_plot
}

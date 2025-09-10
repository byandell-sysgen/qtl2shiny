#' Shiny coefficient analysis and plot module
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
medListApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Mediate List",
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
      title = "Mediate List",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          medListInput("mediate_list"),            # qtls, pos_Mbp
          snpListInput("snp_list"),                # scan_window
          snpListInput2("snp_list"),               # minLOD
          snpListUI("snp_list")),                  # pheno_name
        bslib::card(medListOutput("mediate_list"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    probs_obj <- probsServer("probs", hotspot_list$win_par, project_df)
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    medListServer("mediate_list", hotspot_list, snp_list, probs_obj, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname medListApp
medListServer <- function(id, hotspot_list, snp_list, probs_obj, project_df) {
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
    peak_mar <- shiny::reactive({
      qtl2::find_marker(probs_obj()$map, chr_id(), input$pos_Mbp)
    })
    geno_max <- shiny::reactive({
      shiny::req(input$pos_Mbp, probs_obj())
      subset(probs_obj()$probs, chr = chr_id(), mar = peak_mar())[[1]][,,1]
    })
    
    ## Mediate1
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

    phe1_mx <- shiny::reactive({
      shiny::req(hotspot_list$pheno_mx())
      phename <- shiny::req(input$pheno_name)
      if(phename %in% colnames(hotspot_list$pheno_mx())) {
        hotspot_list$pheno_mx()[, phename, drop = FALSE]
      } else {
        NULL
      }
    })

    # Position of Mediation
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
    output$qtls_input <- shiny::renderUI({
      if(is.null(selected <- input$qtls))
        selected <- 2
      shiny::radioButtons(ns("qtls"), "",
                          c("1 QTL" = 1, "2 QTLs" = 2),
                          selected, inline = TRUE)
    })
    ## Select phenotype for plots.
    output$pheno_name_input <- shiny::renderUI({
      shiny::req(hotspot_list$pheno_mx())
      shiny::selectInput(ns("pheno_name"), NULL,
                         choices = colnames(hotspot_list$pheno_mx()),
                         selected = input$pheno_name)
    })
    
    output$medList_output <- shiny::renderUI({
      shiny::tagList(
        shiny::renderText(paste("qtls:", input$qtls)),
        shiny::renderText(paste("pheno_name:", input$pheno_name)),
        shiny::renderText(paste("pos_Mbp:", input$pos_Mbp))
      )
    })

    # Returns.
    shiny::reactiveValues(
      med_par = input,
      patterns = patterns,
      med_ls = med_ls,
      mediate_obj = mediate_obj,
      sdps = sdps,
      peak_mar = peak_mar,
      phe1_mx = phe1_mx)
  })
}
#' @export
#' @rdname medListApp
medListInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("pheno_name_input")), # pheno_name
    shiny::uiOutput(ns("qtls_input")),       # qtls
    shiny::uiOutput(ns("pos_Mbp_input")))    # pos_Mbp
}
#' @export
#' @rdname medListApp
medListOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("medList_output"))
}

#' Shiny Allele App
#'
#' @param id identifier for shiny reactive
#' @param hotspot_list,pattern_list,pairprobs_obj,patterns,project_df,snp_action reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny isTruthy moduleServer NS plotOutput reactive renderPlot renderUI req
#'             shiny::isTruthy(patterns()) setProgress sliderInput uiOutput
#'             updateSliderInput withProgress
#' @importFrom ggplot2 autoplot ggtitle
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom bslib card layout_sidebar navset_tab nav_panel page_navbar sidebar
genoPanelApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Geno Panel",
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
      title = "Genotypes",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            patternInput("pattern_list"),   # button, blups, pheno_name
            patternUI("pattern_list")),     # pattern
          bslib::card(
            dipParInput("dip_par")),        # snp_action
          bslib::card(
            snpListInput("snp_list")),      # scan_window, minLOD, pheno_name
          bslib::card(
            dipParUI("dip_par")),           # allele_names
          width = 400),
        bslib::card(genoPanelInput("geno_panel"), min_height = "100px"), # pos_Mbp
        bslib::card(genoPanelOutput("geno_panel"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    dip_par <- dipParServer("dip_par", hotspot_list)
    snp_action <- shiny::reactive({dip_par$snp_action})
    snp_list <- snpListServer("snp_list", hotspot_list, project_df, snp_action)
    pairprobs_obj <-
      pairProbsServer("pairprobs", hotspot_list$win_par, project_df)
    pattern_list <- patternServer("pattern_list", hotspot_list, dip_par,
      pairprobs_obj, snp_list$patterns, snp_action, project_df)
    genoPanelServer("geno_panel", hotspot_list, pattern_list, snp_list, pairprobs_obj,
               project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname genoPanelApp
genoPanelServer <- function(id, hotspot_list, pattern_list, snp_list, pairprobs_obj,
                       project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    win_par <- shiny::isolate(hotspot_list$win_par)
    patterns <- shiny::isolate(snp_list$patterns)
    snp_action <- shiny::isolate(snp_list$snp_action)
    chr_id <- shiny::reactive(shiny::req(win_par())$chr_id)
    peak_Mbp <- shiny::reactive(shiny::req(win_par())$peak_Mbp)
    
    geno_list <- 
      genoServer("geno_list", hotspot_list, snp_list, pairprobs_obj, project_df)
    gen_par <- shiny::isolate(geno_list$gen_par)
    

    ## Coefficient Effects.
    allele_obj <- shiny::reactive({
      shiny::req(snp_action(), project_df(), pairprobs_obj(),
                 hotspot_list$kinship_list(), hotspot_list$covar_df(),
                 hotspot_list$peak_df())
      pheno_name <- shiny::req(pattern_list$pat_par$pheno_name)
      pheno_mx <- shiny::req(hotspot_list$pheno_mx())[, pheno_name, drop = FALSE]
      blups <- attr(pattern_list$scan_pat(), "blups")
      shiny::withProgress(message = 'Effect scans ...', value = 0, {
        shiny::setProgress(1)
        allele_scan(pheno_mx, hotspot_list$covar_df(),
                    pairprobs_obj(), hotspot_list$kinship_list(),
                    hotspot_list$peak_df(), patterns(), pattern_list$scan_pat(),
                    blups)
      })
    })
    allele_plot <- shiny::reactive({
      if(!shiny::isTruthy(patterns()) || !shiny::isTruthy(allele_obj()))
        return(plot_null("Visit Patterns Panel First"))
      shiny::req(allele_obj(), gen_par$pos_Mbp)
      shiny::withProgress(message = 'Allele plots ...', value = 0, {
        shiny::setProgress(1)
        p <- ggplot2::autoplot(allele_obj(), pos = gen_par$pos_Mbp)
        if(is.null(p)) {
          plot_null("Visit Patterns Panel First")
        } else {
          p + ggplot2::ggtitle(colnames(hotspot_list$pheno_mx()))
        }
      })
    })
    output$allele_plot_output <- shiny::renderPlot(
      print(shiny::req(allele_plot())))
    
    allele_table <- shiny::reactive({
      shiny::req(allele_obj(), gen_par$pos_Mbp)
      shiny::withProgress(message = 'Effect summary ...', value = 0, {
        shiny::setProgress(1)
        allele_summary(allele_obj(), pos = gen_par$pos_Mbp)
      })
    })
    output$allele_table_output <- DT::renderDataTable(
      shiny::req(allele_table()),
      escape = FALSE, options = list(scrollX = TRUE, pageLength = 5))
    
    # Return. Modify to pick up either table using input$gen_tab
    download_table <- shiny::reactive({
      switch(shiny::req(input$gen_tab),
        Genotypes = gen_list$table(),
        Effects = allele_table())
    })
    shiny::reactiveValues(
      filename = "geno",
      plot  = allele_plot,
      table = download_table)
  })
}
#' @export
#' @rdname genoPanelApp
genoPanelInput <- function(id) {
  ns <- shiny::NS(id)
  genoInput(ns("geno_list"))    # pos_Mbp
}
#' @export
#' @rdname genoPanelApp
genoPanelOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = "gen_tab",
    bslib::nav_panel("Effects",   shiny::plotOutput(ns("allele_plot_output"))),
    bslib::nav_panel("Genotypes", genoOutput(ns("geno_list"))),
    bslib::nav_panel("Summary",   DT::dataTableOutput(ns("allele_table_output"))))
}

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
#' @importFrom shiny isTruthy moduleServer NS plotOutput reactive renderPlot
#'             renderUI req setProgress sliderInput uiOutput
#'             updateSliderInput withProgress
#' @importFrom ggplot2 autoplot ggtitle
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom bslib card layout_sidebar navset_tab nav_panel page_navbar sidebar
genoEffectApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Geno Panel",
    bslib::nav_panel(
      title = "Hotspots",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            projectUI("project_df"),          # project
            hotspotInput("hotspot_list")),    # class, subject_model, pheno_names, hotspot
          bslib::card(
            hotspotUI("hotspot_list")),       # window_Mbp, radio, win_par, chr_ct, minLOD
          width = 400),
        hotspotOutput("hotspot_list"))
    ),
    bslib::nav_panel(
      title = "Genotypes",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            patternInput("pattern_list"),     # button, blups, pheno_name
            patternUI("pattern_list")),       # pattern
          bslib::card(
            dipParInput("dip_par")),          # snp_action
          bslib::card(
            snpListInput("snp_list")),        # scan_window, minLOD, pheno_name
          bslib::card(
            dipParUI("dip_par")),             # allele_names
          width = 400),
        bslib::card(genoInput("geno_list"),
                    min_height = "100px"),    # pos_Mbp
        bslib::navset_tab(
          id = "pat_tab",
          bslib::nav_panel("Plot",
            genoEffectOutput("geno_effect")), # effect_plot
          bslib::nav_panel("Summary",
            genoEffectUI("geno_effect")))     # effect_table
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotServer("hotspot_list", project_df)
    dip_par <- dipParServer("dip_par", hotspot_list)
    snp_action <- shiny::reactive({dip_par$snp_action})
    snp_list <- snpListServer("snp_list", hotspot_list, project_df, snp_action)
    pairprobs_obj <-
      pairProbsServer("pairprobs", hotspot_list$win_par, project_df)
    pattern_list <- patternServer("pattern_list", hotspot_list, dip_par,
      pairprobs_obj, snp_list$patterns, snp_action, project_df)
    geno_list <- 
      genoServer("geno_list", hotspot_list, snp_list, pairprobs_obj, project_df)
    genoEffectServer("geno_effect", hotspot_list, pattern_list, snp_list,
                     geno_list, pairprobs_obj, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname genoEffectApp
genoEffectServer <- function(id, hotspot_list, pattern_list, snp_list,
                             geno_list, pairprobs_obj, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    win_par <- shiny::isolate(hotspot_list$win_par)
    patterns <- shiny::isolate(snp_list$patterns)
    snp_action <- shiny::isolate(snp_list$snp_action)
    chr_id <- shiny::reactive(shiny::req(win_par())$chr_id)
    peak_Mbp <- shiny::reactive(shiny::req(win_par())$peak_Mbp)
    gen_par <- shiny::isolate(geno_list$gen_par)

    ## Coefficient Effects.
    effect_obj <- shiny::reactive({
      shiny::req(snp_action(), project_df(), pairprobs_obj(),
                 hotspot_list$kinship_list(), hotspot_list$covar_df(),
                 hotspot_list$peak_df())
      pheno_name <- shiny::req(pattern_list$pat_par$pheno_name)
      pheno_mx <- shiny::req(hotspot_list$pheno_mx())[, pheno_name, drop = FALSE]
      blups <- attr(pattern_list$scan_pat(), "blups")
      shiny::withProgress(message = 'Effect scans ...', value = 0, {
        shiny::setProgress(1)
        effect_scan(pheno_mx, hotspot_list$covar_df(),
                    pairprobs_obj(), hotspot_list$kinship_list(),
                    hotspot_list$peak_df(), patterns(), pattern_list$scan_pat(),
                    blups)
      })
    })
    effect_plot <- shiny::reactive({
      if(!shiny::isTruthy(patterns()) || !shiny::isTruthy(effect_obj()))
        return(plot_null("Visit Patterns Panel First"))
      shiny::req(effect_obj(), gen_par$pos_Mbp)
      shiny::withProgress(message = 'Allele plots ...', value = 0, {
        shiny::setProgress(1)
        p <- ggplot2::autoplot(effect_obj(), pos = gen_par$pos_Mbp)
        if(is.null(p)) {
          plot_null("Visit Patterns Panel First")
        } else {
          p + ggplot2::ggtitle(colnames(hotspot_list$pheno_mx()))
        }
      })
    })
    output$effect_plot_output <- shiny::renderPlot(
      print(shiny::req(effect_plot())))
    
    effect_table <- shiny::reactive({
      shiny::req(effect_obj(), gen_par$pos_Mbp)
      shiny::withProgress(message = 'Effect summary ...', value = 0, {
        shiny::setProgress(1)
        effect_summary(effect_obj(), pos = gen_par$pos_Mbp)
      })
    })
    output$effect_table_output <- DT::renderDataTable(
      shiny::req(effect_table()),
      escape = FALSE, options = list(scrollX = TRUE, pageLength = 5))
    
    # Return.
    shiny::reactiveValues(
      table = effect_table,
      plot = effect_plot)
  })
}
#' @export
#' @rdname genoEffectApp
genoEffectUI <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("effect_table_output")) # effect_table
}
#' @export
#' @rdname genoEffectApp
genoEffectOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("effect_plot_output"))    # effect_plot
}

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
#' @importFrom shiny column downloadButton downloadHandler fluidRow moduleServer
#'             NS plotOutput reactive renderPlot renderUI req setProgress
#'             sliderInput tagList uiOutput updateSliderInput withProgress
#' @importFrom ggplot2 autoplot ggtitle
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom utils write.csv
#' @importFrom grDevices dev.off pdf
#' @importFrom rlang .data
genoApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Geno",
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
        bslib::card(genoInput("geno"), min_height = "100px"), # pos_Mbp
        bslib::card(genoOutput("geno"))
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
    genoServer("geno", hotspot_list, pattern_list, snp_list, pairprobs_obj,
               project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname genoApp
genoServer <- function(id, hotspot_list, pattern_list, snp_list, pairprobs_obj,
                       project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    win_par <- shiny::isolate(hotspot_list$win_par)
    patterns <- shiny::isolate(snp_list$patterns)
    snp_action <- shiny::isolate(snp_list$snp_action)
    chr_id <- shiny::reactive(shiny::req(win_par())$chr_id[1])
    peak_Mbp <- shiny::reactive(shiny::req(win_par())$peak_Mbp[1])
    
    # Scan Window slider
    output$pos_Mbp_input <- shiny::renderUI({
      shiny::req(project_df())
      chr <- shiny::req(chr_id())
      map <- shiny::req(pairprobs_obj())$map[[chr]]
      rng <- round(2 * range(map)) / 2
      if(is.null(value <- input$pos_Mbp))
        value <- req(peak_Mbp())
      if(value < rng[1] | value > rng[2]) value <- mean(rng)
      shiny::sliderInput(ns("pos_Mbp"), NULL, rng[1], rng[2],
                         value, step=.1)
    })
    ## Reset pos_Mbp if chromosome changes.
    observeEvent(shiny::req(win_par()), {
      map <- shiny::req(pairprobs_obj()$map)
      chr <- shiny::req(chr_id())
      rng <- round(2 * range(map[[chr]])) / 2
      value <- shiny::req(peak_Mbp())
      if(value < rng[1] | value > rng[2]) value <- mean(rng)
      shiny::updateSliderInput(session, "pos_Mbp", NULL, 
                               value, 
                               rng[1], rng[2], step=.1)
    })
    
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
    output$allele_plot <- shiny::renderPlot({
      shiny::req(allele_obj(), input$pos_Mbp)
      shiny::withProgress(message = 'Allele plots ...', value = 0, {
        shiny::setProgress(1)
        p <- ggplot2::autoplot(allele_obj(), pos = input$pos_Mbp)
        if(is.null(p)) {
          plot_null()
        } else {
          p + ggplot2::ggtitle(colnames(hotspot_list$pheno_mx()))
        }
      })
    })
    output$allele_table <- DT::renderDataTable({
      shiny::req(allele_obj(), input$pos_Mbp)
      shiny::withProgress(message = 'Effect summary ...', value = 0, {
        shiny::setProgress(1)
        allele_summary(allele_obj(), pos = input$pos_Mbp)
      })
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
  })
}
#' @export
#' @rdname genoApp
genoInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("pos_Mbp_input"))    # pos_Mbp
}
#' @export
#' @rdname genoApp
genoOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = "all_tab",
    bslib::nav_panel("Plot",    shiny::plotOutput(ns("allele_plot"))),
    bslib::nav_panel("Summary", DT::dataTableOutput(ns("allele_table"))))
}

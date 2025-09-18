#' Shiny Allele App
#'
#' @param id identifier for shiny reactive
#' @param hotspot_list,pairprobs_obj,patterns,project_df,snp_action reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny moduleServer NS plotOutput reactive renderPlot renderUI req
#'             setProgress sliderInput uiOutput updateSliderInput withProgress
#' @importFrom ggplot2 autoplot ggtitle
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom bslib card layout_sidebar navset_tab nav_panel page_navbar sidebar
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
    genoServer("geno", hotspot_list, snp_list, pairprobs_obj, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname genoApp
genoServer <- function(id, hotspot_list, snp_list, pairprobs_obj, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    win_par <- shiny::isolate(hotspot_list$win_par)
    patterns <- shiny::isolate(snp_list$patterns)
    snp_action <- shiny::isolate(snp_list$snp_action)
    chr_id <- shiny::reactive(shiny::req(win_par())$chr_id)
    peak_Mbp <- shiny::reactive(shiny::req(win_par())$peak_Mbp)

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
    
    ## Genotypes
    geno_table <- shiny::reactive({
      shiny::req(patterns(), pairprobs_obj(), input$pos_Mbp)
      shiny::withProgress(message = 'Genotypes ...', value = 0, {
        shiny::setProgress(1)
        pair_geno_table(pairprobs_obj(), patterns(), input$pos_Mbp)
      })
    })
    output$geno_table_output <- DT::renderDataTable(
      shiny::req(geno_table()),
      escape = FALSE, options = list(scrollX = TRUE, pageLength = 5))
    
    download <- shiny::reactiveValues(
      filename = "geno",
      #plot  = geno_plot,
      table = geno_table)
    
    # Return.
    shiny::reactiveValues(
      gen_par = input,
      download = download)
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
    #bslib::nav_panel("Plot",    shiny::plotOutput(ns("geno_plot_output"))),
    bslib::nav_panel("Summary", DT::dataTableOutput(ns("geno_table_output"))))
}

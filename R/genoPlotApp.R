#' Shiny Genotype Data App
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
genoPlotApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Geno Data",
    bslib::nav_panel(
      title = "Hotspots",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            projectUI("project_df"),       # project
            hotspotInput("hotspot_list")), # class, subject_model, pheno_names, hotspot
          bslib::card(
            hotspotUI("hotspot_list")),    # window_Mbp, radio, win_par, chr_ct, minLOD
          width = 400),
        hotspotOutput("hotspot_list"))
    ),
    bslib::nav_panel(
      title = "Genotypes",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            dipParInput("dip_par")),       # snp_action
          bslib::card(
            snpListInput("snp_list")),     # scan_window, minLOD, pheno_name
          bslib::card(
            dipParUI("dip_par")),          # allele_names
          width = 400),
        bslib::card(
          genoDataInput("geno_data"),
          min_height = "100px"),           # pos_Mbp
        genoPlotOutput("geno_plot")        # geno_plot
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
    geno_list <- genoDataServer("geno_data", hotspot_list, snp_list,
                                pairprobs_obj, project_df)
    genoPlotServer("geno_plot", hotspot_list, geno_list)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname genoPlotApp
genoPlotServer <- function(id, hotspot_list, geno_list) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pheno_mx <- shiny::isolate(hotspot_list$pheno_mx)
    geno_table <- shiny::isolate(geno_list$Table)
    
    geno_plot <- shiny::reactive({
      shiny::req(geno_table())
      pheno_df <- as.data.frame(shiny::req(pheno_mx()), make.names = FALSE)
      pheno_names <- names(pheno_df)
      pheno_df <- tibble::rownames_to_column(pheno_df, var = "subject")
      geno_df <- as.data.frame(shiny::req(geno_table()), make.names = FALSE)
      geno_names <- names(geno_df)
      geno_df <- tibble::rownames_to_column(geno_df, var = "subject")
      dat <- dplyr::left_join(pheno_df, geno_df, by = "subject")
      yname <- ifelse(length(pheno_names) > 1, pheno_names[2], geno_names[2])
      p <- ggplot2::ggplot(dat) +
        ggplot2::aes(x = .data[[pheno_names[1]]], y = .data[[yname]],
                     col = .data[[geno_names[2]]],
                     label = .data[[geno_names[1]]]) +
        ggrepel::geom_text_repel(max.overlaps = Inf, min.segment.length = Inf,
                                 size = 3, fontface = 2)
      if(length(pheno_names) > 1) {
        p <- p + ggplot2::geom_smooth(se = FALSE, method = "lm",
                                      linewidth = 2, linetype = 2)
      }
      p
    })
    output$geno_plot <- shiny::renderPlot({
      print(shiny::req(geno_plot()))
    })
    
    # Return.
    geno_plot
  })
}
#' @export
#' @rdname genoPlotApp
genoPlotInput <- function(id) {
  ns <- shiny::NS(id)
}
#' @export
#' @rdname genoPlotApp
genoPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("geno_plot"))
}

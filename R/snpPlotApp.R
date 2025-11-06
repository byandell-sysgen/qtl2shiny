#' Shiny SNP plot module
#'
#' @param id identifier for shiny reactive
#' @param snp_list reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom shiny isTruthy moduleServer NS
#'             plotOutput reactive renderPlot req setProgress withProgress
#' @importFrom bslib card layout_sidebar nav_panel page_navbar sidebar
snpPlotApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test SNP Plot",
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
      title = "snpPlot",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            dipParInput("dip_par")),       # snp_action
          bslib::card(
            snpListInput("snp_list")),     # scan_window, minLOD, pheno_name
          bslib::card(
            dipParUI("dip_par")),          # allele_names
          width = 400),
        bslib::card(snpPlotOutput("snp_plot"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotServer("hotspot_list", project_df)
    dip_par <- dipParServer("dip_par", hotspot_list)
    snp_action <- shiny::reactive({dip_par$snp_action})
    snp_list <- snpListServer("snp_list", hotspot_list, project_df, snp_action)
    snpPlotServer("snp_plot", snp_list)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname snpPlotApp
snpPlotServer <- function(id, snp_list) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    snp_plot <- shiny::reactive({
      if(!shiny::isTruthy(snp_list$snp_par$scan_window) ||
         !shiny::isTruthy(snp_list$pheno_names()))
        return(plot_null("need to select\nRegion & Phenotype"))
      
      if(is.null(snp_list$snp_scan_obj()) |
         is.null(snp_list$snp_par$scan_window) |
         is.null(snp_list$snp_action()) |
         is.null(snp_list$snpinfo()) |
         is.null(snp_list$snp_par$minLOD))
        return(plot_null())
      shiny::withProgress(message = 'SNP plots ...', value = 0, {
        shiny::setProgress(1)
        top_snp_asso(snp_list$snp_scan_obj(), snp_list$snpinfo(),
                     snp_list$snp_par$scan_window, snp_list$snp_action(), 
                     minLOD = snp_list$snp_par$minLOD)
      })
    })
    output$snp_plot_output <- shiny::renderPlot({
      print(shiny::req(snp_plot()))
    })
    # Return.
    snp_plot
  })
}
#' @export
#' @rdname snpPlotApp
snpPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("snp_plot_output"))
}

#' Shiny Pattern Panel App
#'
#' @param id identifier for shiny reactive
#' @param dip_par,hotspot_list,snp_list,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom shiny mainPanel moduleServer NS radioButtons reactive renderText
#'             renderUI req selectInput sidebarPanel strong tagList textOutput
#'             uiOutput
#' @importFrom bslib card layout_sidebar navbar_options navset_tab nav_panel
#'             page_navbar sidebar
patternPanelApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test patternPanel",
    navbar_options = bslib::navbar_options(bg = "#2D89C8", theme = "dark"),
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
      title = "Pattern Panel",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            patternPanelInput("pattern_panel"), # <various>
            bslib::card(
              dipParInput("dip_par")),      # snp_action
            bslib::card(
              snpListInput("snp_list")),    # scan_window, minLOD, pheno_name
            bslib::card(
              dipParUI("dip_par")),         # allele_names
            width = 400),
          bslib::card(dipParUI("dip_par")),   # allele_names
        ),
        bslib::card(patternPanelOutput("pattern_panel"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    dip_par <- dipParServer("dip_par", hotspot_list)
    snp_action <- shiny::reactive({dip_par$snp_action})
    snp_list <- snpListServer("snp_list", hotspot_list, project_df, snp_action)
    patternPanelServer("pattern_panel", dip_par, hotspot_list, snp_list, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname patternPanelApp
patternPanelServer <- function(id, dip_par, hotspot_list, snp_list, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## SDP Patterns
    snpPatternServer("snp_pattern", snp_list, hotspot_list$allele_info)
    pairprobs_obj <-
      pairProbsServer("pairprobs", hotspot_list$win_par, project_df)
    pattern_list <- patternServer("pattern_list", hotspot_list, dip_par,
      pairprobs_obj, snp_list$patterns, snp_list$snp_action, project_df)
    patternPlotServer("pattern_plot", pattern_list, pairprobs_obj)
  })
}
#' @export
#' @rdname patternPanelApp
patternPanelInput <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    patternInput(ns("pattern_list")),  # button, blups, pheno_name
    patternUI(ns("pattern_list")))     # pattern
}
#' @export
#' @rdname patternPanelApp
patternPanelOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("dip_tab"),
    bslib::nav_panel("SNP Pattern Scan",
      bslib::card(snpPatternOutput(ns("snp_pattern")))),
    bslib::nav_panel("SDP Pattern Scans", bslib::navset_tab(
      bslib::nav_panel("Plot",
        bslib::card(patternPlotOutput(ns("pattern_plot")))),
      bslib::nav_panel("Summary",
        bslib::card(patternOutput(ns("pattern_list")))))))
}
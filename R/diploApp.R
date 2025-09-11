#' Shiny Diplotype module
#'
#' Shiny diplotype SNP/Gene action analysis, with interface \code{diploUI}.
#' 
#' @param id identifier for shiny reactive
#' @param win_par,phe_mx,cov_df,K_chr,analyses_df,project_df,allele_info reactive arguments
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
diploApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Diplo",
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
      title = "Diplo",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(diploUI("diplo")), # <various>
        bslib::card(diploOutput("diplo"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    diploServer("diplo", hotspot_list, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname diploApp
diploServer <- function(id, hotspot_list, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    dip_par <- dipParServer("dip_par")
    ## Probs object for allele pair diplotypes.
    pairprobs_obj <-
      pairProbsServer("pairprobs", hotspot_list$win_par, project_df)
    snp_action <- shiny::reactive({dip_par$snp_action})
    ## SNP Association
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    pattern_list <- patternServer("pattern_list", hotspot_list, dip_par,
      pairprobs_obj, snp_list$patterns, snp_list$snp_action, project_df)
    patternPlotServer("pattern_plot", pattern_list, pairprobs_obj)
    alleleServer("allele", hotspot_list, pattern_list, pairprobs_obj,
      patterns, project_df, snp_action)

    output$allele_names <- shiny::renderText({
      allele_info <- shiny::req(hotspot_list$allele_info())
      paste(allele_info$code, allele_info$shortname, sep = "=", collapse = ", ")
    })
    
    output$tabset_input <- shiny::renderUI({
      switch(shiny::req(input$dip_tab),
        "SDP Scans" = bslib::card(
          patternInput(ns("pattern_list")), # button, blups, pheno_name
          patternUI(ns("pattern_list"))),   # pattern
        "Allele Pattern" = bslib::card(
          alleleInput(ns("allele"))))       # pos_Mbp
    })
    output$pattern_output <- shiny::renderUI({
      
    })
  })
}
#' @export
#' @rdname diploApp
diploUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("project")),
    shiny::strong("SNP/Gene Action"),
    shiny::uiOutput(ns("tabset_input")),   # <various--see above>
    dipParUI(ns("dip_par")),               # snp_action
    snpListInput(ns("snp_list")),          # scan_window, minLOD, pheno_name
    shiny::textOutput(ns("allele_names"))) # allele_names
}
#' @export
#' @rdname diploApp
diploOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("dip_tab"),
    bslib::nav_panel("SDP Scans", bslib::navset_tab(
      bslib::nav_panel("Plot",
        bslib::card(patternPlotOutput(ns("pattern_plot")))),
      bslib::nav_panel("Summary",
        bslib::card(patternOutput(ns("pattern_list")))))),
    bslib::nav_panel("Allele Pattern", 
      bslib::card(alleleOutput(ns("allele")))))
}
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
diploApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Diplo",
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
    patterns <-
      snpSetupServer("snp_setup", hotspot_list, dip_par, project_df, snp_action)
    pattern_list <- patternServer("pattern_list", hotspot_list, dip_par,
      pairprobs_obj, patterns, snp_action, project_df)
    patternPlotServer("pattern_plot", pattern_list, pairprobs_obj)
    alleleServer("allele", hotspot_list, pattern_list, pairprobs_obj,
      patterns, project_df, snp_action)

    output$allele_names <- shiny::renderText({
      allele_info <- shiny::req(hotspot_list$allele_info())
      paste(allele_info$code, allele_info$shortname, sep = "=", collapse = ", ")
    })
    
    output$dip_input <- shiny::renderUI({
      switch(shiny::req(dip_par$button),
             "Allele Pattern"  = alleleUI(ns("allele")))
    })
    output$dip_output <- shiny::renderUI({
      switch(shiny::req(dip_par$button),
             "Genome Scans"    = patternPlotOutput(ns("pattern_plot")),
             "Summary"         = patternOutput(ns("pattern_list")),
             "SNP Association" = snpSetupOutput(ns("snp_setup")),
             "Allele Pattern"  = alleleOutput(ns("allele")))
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
    dipParInput(ns("dip_par")),            # sex_type
    dipParUI(ns("dip_par")),               # button, snp_action
    snpSetupInput(ns("snp_setup")),        # <various>
    patternInput(ns("pattern_list")),      # button, blups, pheno_name
    patternUI(ns("pattern_list")),         # pattern
    shiny::uiOutput(ns("dip_input")),
    shiny::textOutput(ns("allele_names")))
}
#' @export
#' @rdname diploApp
diploOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("dip_output"))
}
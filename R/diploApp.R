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
    title =  "Test Haplo",
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
      title = "Haplo",
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
    
    patternServer("dip_pat", hotspot_list, dip_par, pairprobs_obj, patterns,
                  snp_action, project_df)
    
    output$allele_names <- shiny::renderText({
      allele_info <- shiny::req(hotspot_list$allele_info())
      paste(allele_info$code, allele_info$shortname, sep = "=", collapse = ", ")
    })
    
    output$dip_input <- shiny::renderUI({
      switch(shiny::req(dip_par$button),
             "Genome Scans"    = patternUI(ns("dip_pat")),
             "SNP Association" =,
             "Allele Pattern"  = snpSetupInput(ns("snp_setup")))
    })
    output$dip_output <- shiny::renderUI({
      switch(shiny::req(dip_par$button),
             "Genome Scans"    = patternOutput(ns("dip_pat")),
             "SNP Association" = ,
             "Allele Pattern"  = snpSetupOutput(ns("snp_setup")))
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
    dipParInput(ns("dip_par")),
    dipParUI(ns("dip_par")),
    shiny::uiOutput(ns("dip_input")),
    shiny::textOutput(ns("allele_names")))
}
#' @export
#' @rdname diploApp
diploOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("dip_output"))
}
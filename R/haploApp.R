#' Shiny Haplotype Analysis App
#'
#' Shiny module for analysis based on haplotype alleles, with interface \code{haploUI}.
#'
#' @param id identifier for shiny reactive
#' @param hotspot_list,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom shiny mainPanel moduleServer NS radioButtons renderText renderUI
#'             req sidebarPanel strong tagList textOutput uiOutput
#' @importFrom bslib card layout_sidebar nav_panel page_navbar sidebar
haploApp <- function() {
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
        sidebar = bslib::sidebar(haploUI("haplo")), # <various>
        bslib::card(haploOutput("haplo"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    haploServer("haplo", hotspot_list, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname haploApp
haploServer <- function(id, hotspot_list, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    hap_par <- hapParServer("hap_par")
    ## Genotype Probabilities.
    probs_obj <- probsServer("probs", hotspot_list$win_par, project_df)
    ## Genome Scan.
    scanServer("scan", hotspot_list, hap_par, probs_obj, project_df)
    ## SNP Association and Allele Patterns
    patterns <- snpSetupServer("snp_setup", hotspot_list, hap_par, project_df)
    ## Mediation
    #mediateServer("mediate", hotspot_list, probs_obj, patterns, project_df)
    
    output$allele_names <- shiny::renderText({
      allele_info <- shiny::req(hotspot_list$allele_info())
      paste(allele_info$code, allele_info$shortname, sep = "=", collapse = ", ")
    })
    
    output$hap_input <- shiny::renderUI({
      switch(shiny::req(hap_par$button),
             "Genome Scans"    = scanUI(ns("scan")),
             "SNP Association" =,
             "Allele Pattern"  = snpSetupInput(ns("snp_setup")),
             #"Mediation"       = mediateUI(ns("mediate"))
             )
    })
    output$hap_output <- shiny::renderUI({
      switch(shiny::req(hap_par$button),
             "Genome Scans"    = scanOutput(ns("scan")),
             "SNP Association" = ,
             "Allele Pattern"  = snpSetupOutput(ns("snp_setup")),
             #"Mediation"       = mediateOutput(ns("mediate"))
             )
    })
    output$project <- shiny::renderUI({
      shiny::strong(shiny::req(paste("Project:",
                                     project_df()$project,
                                     "\n")))
    })
  })
}
#' @export
#' @rdname haploApp
haploUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("project")),
    shiny::strong("SNP/Gene Additive"),
    hapParUI(ns("hap_par")),               # button
    hapParInput(ns("hap_par")),            # sex_type
    shiny::uiOutput(ns("hap_input")),
    shiny::textOutput(ns("allele_names")))
}
#' @export
#' @rdname haploApp
haploOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("hap_output"))
}  
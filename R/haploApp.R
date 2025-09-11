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
#' @importFrom bslib card layout_sidebar navbar_options navset_tab nav_panel
#'             page_navbar sidebar
haploApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Haplo",
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
      title = "Haplo",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          haploUI("haplo"),                  # <various>
          haploInput("haplo")),                    # <various>
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
    
    ## Genotype Probabilities.
    probs_obj <- probsServer("probs", hotspot_list$win_par, project_df)
    ## Genome Scan.
    scanServer("scan", hotspot_list, probs_obj, project_df)
    ## SNP List and Patterns
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    ## SNP Association
    snpGeneServer("snp_gene", snp_list, project_df)
    ## Pattern Scans
    snpPatternServer("snp_pattern", snp_list, hotspot_list$allele_info)
    ## Mediation
    mediate_list <-
      mediateServer("mediate_list", hotspot_list, snp_list, probs_obj, project_df)
    mediatePlotServer("mediate_plot", hotspot_list, mediate_list, probs_obj, project_df)
    triadServer("triad", hotspot_list, snp_list, mediate_list, probs_obj)
    
    output$allele_names <- shiny::renderText({
      allele_info <- shiny::req(hotspot_list$allele_info())
      paste(allele_info$code, allele_info$shortname, sep = "=", collapse = ", ")
    })
    
    output$project <- shiny::renderUI({
      shiny::strong(shiny::req(paste("Project:",
                                     project_df()$project,
                                     "\n")))
    })
    output$tabset_input <- shiny::renderUI({
      switch(shiny::req(input$hap_tab),
        "Genome Scans"    = shiny::tagList(
          scanInput(ns("scan")),                          # blups, pheno_name
          scanUI(ns("scan"))),                            # scan_window
        "Mediation"       = bslib::card(
          switch(input$med_tab,
            Plot = mediatePlotInput(ns("mediate_plot")),  # static, signif, local, med_plot
            Triad = triadInput(ns("triad"))),             # triad, med_name, triad_plot
          mediateInput(ns("mediate_list"))),              # qtls, pos_Mbp
        "SNP Association" = snpGeneInput(ns("snp_gene"))) # SNP, gene_name
    })
    output$mediate_panel <- shiny::renderUI({
      bslib::navset_tab(
        id = ns("med_tab"),
        bslib::nav_panel("Plot",    mediatePlotOutput(ns("mediate_plot"))),
        bslib::nav_panel("Triad",   triadOutput(ns("triad"))),
        bslib::nav_panel("Summary", mediateOutput(ns("mediate_list"))))
    })
  })
}
#' @export
#' @rdname haploApp
haploInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    snpListInput(ns("snp_list")),          # scan_window
    snpListInput2(ns("snp_list")),         # minLOD
    snpListUI(ns("snp_list")),             # pheno_name
    shiny::textOutput(ns("allele_names")))
}
#' @export
#' @rdname haploApp
haploUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("project")),
    shiny::strong("SNP/Gene Additive"),
    shiny::uiOutput(ns("tabset_input")))
}
#' @export
#' @rdname haploApp
haploOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("hap_tab"),
    bslib::nav_panel("Genome Scans",    scanOutput(ns("scan"))),
    bslib::nav_panel("SNP Association", snpGeneOutput(ns("snp_gene"))),
    bslib::nav_panel("Pattern Scan",    snpPatternOutput(ns("snp_pattern"))),
    bslib::nav_panel("Mediation",       shiny::uiOutput(ns("mediate_panel"))))
}  
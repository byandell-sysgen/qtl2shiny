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
#' @importFrom bslib page_sidebar sidebar
haploApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Haplo",
    sidebar = bslib::sidebar(
      projectUI("project_df"),           # project
      setParInput("set_par"),            # class, subject_model
      hotspotPanelInput("hotspot_list"), # pheno_names, chr_pos
      hotspotPanelUI("hotspot_list")),   # radio, local, win_par, chr_ct, minLOD
    shiny::uiOutput("pheno_names"),
    haploUI("haplo")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    hotspot_list <- 
      hotspotPanelServer("hotspot_list", set_par, peak_df, pmap_obj, project_df)

    haploServer("haplo", hotspot_list, project_df)
    
    output$pheno_names <- shiny::renderUI({
      paste("pheno_names: ", paste(hotspot_list$pheno_names(), collapse = ", "))
    })
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
    
    ## SNP Association
    patterns <- snpSetupServer("snp_setup", hotspot_list, hap_par, project_df)
    
    ## Mediation
    mediateServer("mediate", hotspot_list, probs_obj, patterns, project_df)
    
    output$allele_names <- shiny::renderText({
      shiny::req(hotspot_list$allele_info())
      paste(hotspot_list$allele_info()$code, hotspot_list$allele_info()$shortname, sep = "=", collapse = ", ")
    })
    
    output$hap_input <- shiny::renderUI({
      switch(shiny::req(input$button),
             "Genome Scans"    = scanUI(ns("scan")),
             "SNP Association" =,
             "Allele Pattern"  = snpSetupInput(ns("snp_setup")),
             "Mediation"       = mediateUI(ns("mediate")))
    })
    output$hap_output <- shiny::renderUI({
      switch(shiny::req(input$button),
             "Genome Scans"    = scanOutput(ns("scan")),
             "SNP Association" = ,
             "Allele Pattern"  = snpSetupOutput(ns("snp_setup")),
             "Mediation"       = mediateOutput(ns("mediate")))
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
    shiny::sidebarPanel(
      shiny::uiOutput(ns("project")),
      shiny::strong("SNP/Gene Additive"),
      hapParUI(ns("hap_par")),   # button
      hapParInput(ns("hap_par")), # sex_type
      shiny::uiOutput(ns("hap_input")),
      shiny::textOutput(ns("allele_names"))),
    shiny::mainPanel(
      shiny::uiOutput(ns("hap_output")))
  )
}

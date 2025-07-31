#' Shiny Haplotype Analysis App
#'
#' Shiny module for analysis based on haplotype alleles, with interface \code{haploUI}.
#'
#' @param id identifier for shiny reactive
#' @param win_par,pmap_obj,phe_mx,cov_df,K_chr,analyses_df,covar,analyses_tbl,peak_df,project_df,allele_info reactive arguments
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
      setupInput("setup"),
      projectUI("project"),
      setupUI("setup")),
    haploUI("haplo")
  )
  server <- function(input, output, session) {
    peak_df <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "peaks")
    })
    pmap_obj <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "pmap")
    })
    analyses_df <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "analyses")
    })
    covar <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "covar")
    })
    ## Phenotypes and Covariates for Haplotype Analyses.
    phe_mx <- shiny::reactive({
      analyses <- analyses_df() 
      if(is.null(analyses)) return(NULL)
      shiny::req(project_df())
      pheno_read(project_df(), analyses)
    })
    cov_df <- shiny::reactive({
      analyses <- analyses_df() 
      if(is.null(analyses)) return(NULL)
      qtl2mediate::get_covar(covar(), analyses_df())
    })
    ## Allele names.
    allele_info <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "allele_info")
    })
    
    project_df <- projectServer("project", projects_df)
    set_par <- setupServer("setup", peak_df, pmap_obj, analyses_df, covar,
                           project_df)
    haploServer("haplo", set_par$win_par, pmap_obj, phe_mx, cov_df, K_chr,
      analyses_df, covar, analyses_tbl, peak_df, project_df, allele_info)
    
    output$set_par <- shiny::renderUI({
      paste("pheno_names: ", paste(set_par$pheno_names(), collapse = ", "))
    })
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname haploApp
haploServer <- function(id, win_par, pmap_obj, 
                       phe_mx, cov_df, K_chr, analyses_df, 
                       covar, analyses_tbl, peak_df,
                       project_df, allele_info) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Genotype Probabilities.
    probs_obj <- probsServer("probs", win_par, project_df)
    
    ## Genome Scan.
    scanServer("hap_scan", input, win_par, phe_mx, cov_df, probs_obj, K_chr,
                  analyses_df, project_df, allele_info)
    
    ## SNP Association
    patterns <- snpSetupServer("snp_setup", input, win_par, phe_mx, cov_df, K_chr,
                              analyses_df, project_df, allele_info)
    
    ## Mediation
    mediateServer("mediate", input, win_par, patterns, phe_mx, cov_df, probs_obj, K_chr,
                 analyses_df, pmap_obj, covar, analyses_tbl, peak_df, project_df, allele_info)
    
    output$allele_names <- shiny::renderText({
      shiny::req(allele_info())
      paste(allele_info()$code, allele_info()$shortname, sep = "=", collapse = ", ")
    })
    
    output$hap_input <- shiny::renderUI({
      switch(shiny::req(input$button),
             "Genome Scans"    = scanUI(ns("hap_scan")),
             "SNP Association" =,
             "Allele Pattern"  = snpSetupUI(ns("snp_setup")),
             "Mediation"       = mediateUI(ns("mediate")))
    })
    output$hap_output <- shiny::renderUI({
      switch(shiny::req(input$button),
             "Genome Scans"    = scanOutput(ns("hap_scan")),
             "SNP Association" = ,
             "Allele Pattern"  = snpSetupOutput(ns("snp_setup")),
             "Mediation"       = mediateOutput(ns("mediate")))
    })
    output$button_input <- shiny::renderUI({
      shiny::radioButtons(ns("button"), "",
                          c("Genome Scans","SNP Association","Allele Pattern","Mediation"),
                          input$button)
    })
    output$sex_type_input <- shiny::renderUI({
      choices <- c("A","I","F","M","all")
      if(ncol(shiny::req(phe_mx())) > 1 & shiny::req(input$button) != "Mediation") {
        choices <- choices[1:4]
      }
      shiny::radioButtons(ns("sex_type"), "Sex:",
                          choices,
                          input$sex_type, inline = TRUE)
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
      shiny::uiOutput(ns("button_input")),
      shiny::uiOutput(ns("sex_type_input")),
      shiny::uiOutput(ns("hap_input")),
      shiny::textOutput(ns("allele_names"))),
    shiny::mainPanel(
      shiny::uiOutput(ns("hap_output")))
  )
}

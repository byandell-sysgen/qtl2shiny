#' Shiny Haplotype Analysis App
#'
#' Shiny module for analysis based on haplotype alleles, with interface \code{haploUI}.
#'
#' @param id identifier for shiny reactive
#' @param win_par,pmap_obj,pheno_mx,covar_df,K_chr,analyses_df,covar,analyses_tbl,peak_df,project_df,allele_info reactive arguments
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
      projectUI("project"),
      setParInput("set_par"),
      setupInput("setup"),
      setupUI("setup")),
    shiny::uiOutput("pheno_names"),
    haploUI("haplo")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    
    pmap_obj <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "pmap")
    })

    set_list <- setupServer("setup", set_par, peak_df, pmap_obj, project_df)
    
    ## Phenotypes and Covariates for Haplotype Analyses.
    pheno_mx <- shiny::reactive({
      shiny::req(project_df(), set_par$class)
      read_project(project_df(), "pheno", class = set_par$class,
                   columns = set_list$pheno_names)
    })
    covar_df <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "covar")
    })
    ## Kinship and Alleles.
    K_chr <- shiny::reactive({
      shiny::req(project_df(), set_list$win_par$chr_id)
      read_project(project_df(), "kinship")[set_list$win_par$chr_id]
    })
    allele_info <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "allele_info")
    })

    haploServer("haplo", set_list$win_par, pmap_obj, pheno_mx, covar_df, K_chr,
      peak_df, project_df, allele_info)
    
    output$pheno_names <- shiny::renderUI({
      paste("pheno_names: ", paste(set_list$pheno_names(), collapse = ", "))
    })
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname haploApp
haploServer <- function(id, win_par, pmap_obj, pheno_mx, covar_df, K_chr, 
                        peak_df, project_df, allele_info) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    hap_par <- hapParServer("hap_par")
    ## Genotype Probabilities.
    probs_obj <- probsServer("probs", win_par, project_df)
    
    ## Genome Scan.
    scanServer("scan", hap_par, win_par, peak_df, pheno_mx, covar_df,
               K_chr, probs_obj, allele_info, project_df)
    
    ## SNP Association
    patterns <- snpSetupServer("snp_setup", hap_par, win_par,
      peak_df, pheno_mx, covar_df, K_chr, allele_info, project_df)
    
    ## Mediation
    mediateServer("mediate", hap_par, win_par, patterns, pheno_mx, covar_df, probs_obj, K_chr,
                 analyses_df, pmap_obj, covar_df, analyses_tbl, peak_df, project_df, allele_info)
    
    output$allele_names <- shiny::renderText({
      shiny::req(allele_info())
      paste(allele_info()$code, allele_info()$shortname, sep = "=", collapse = ", ")
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

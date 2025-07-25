#' Shiny haplotype analysis module
#'
#' Shiny module for analysis based on haplotype alleles, with interface \code{haploUI}.
#'
#' @param id identifier for shiny reactive
#' @param win_par,pmap_obj,phe_mx,cov_df,K_chr,analyses_df,covar,analyses_tbl,peaks_tbl,project_info,allele_info reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom shiny mainPanel moduleServer NS radioButtons renderText renderUI
#'             req sidebarPanel strong tagList textOutput uiOutput
haploServer <- function(id, win_par, pmap_obj, 
                       phe_mx, cov_df, K_chr, analyses_df, 
                       covar, analyses_tbl, peaks_tbl,
                       project_info, allele_info) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Genotype Probabilities.
    probs_obj <- probsServer("probs", win_par, project_info)
    
    ## Genome Scan.
    scanServer("hap_scan", input, win_par, phe_mx, cov_df, probs_obj, K_chr,
                  analyses_df, project_info, allele_info)
    
    ## SNP Association
    patterns <- snpSetupServer("snp_setup", input, win_par, phe_mx, cov_df, K_chr,
                              analyses_df, project_info, allele_info)
    
    ## Mediation
    mediateServer("mediate", input, win_par, patterns, phe_mx, cov_df, probs_obj, K_chr,
                 analyses_df, pmap_obj, covar, analyses_tbl, peaks_tbl, project_info, allele_info)
    
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
                                     project_info()$project,
                                     "\n")))
    })
  })
}
#' @export
#' @rdname haploServer
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

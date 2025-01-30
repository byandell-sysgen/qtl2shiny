#' Shiny SNP and Allele analysis and plot module
#'
#' Shiny module to coordinate SNP and allele analyses and plots, with interfaces \code{shinySNPSetupUI} and  \code{shinySNPSetupOutput}.
#'
#' @param id identifier for shiny reactive
#' @param job_par,win_par,phe_mx,cov_df,K_chr,analyses_df,project_info,allele_info,snp_action reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom dplyr arrange desc filter mutate
#' @importFrom qtl2pattern gene_exon top_snps_pattern snpprob_collapse
#' @importFrom qtl2mediate scan1covar covar_matrix which_covar
#' @importFrom shiny moduleServer NS reactive req 
#'   selectInput sliderInput
#'   numericInput uiOutput
#'   renderUI
#'   tagList
#'   withProgress setProgress
#' @importFrom rlang .data
#' 
shinySNPSetup <- function(id, job_par, win_par, phe_mx, cov_df, K_chr, analyses_df,
                          project_info, allele_info,
                          snp_action = shiny::reactive({"basic"})) {
  shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns
  
  chr_pos <- shiny::reactive({
    make_chr_pos(shiny::req(win_par$chr_id), 
                 range = shiny::req(input$scan_window))
  })
  pheno_names <- shiny::reactive({
    shiny::req(project_info(), phe_mx())
    colnames(phe_mx())
  })
  
  ## Reactives
  ## SNP Probabilities.
  snpprobs_obj <- probsSNPServer("snp_probs", win_par, pheno_names, project_info)
  snpinfo <- reactive({
    shiny::req(project_info(), phe_mx())
    shiny::req(snpprobs_obj())$snpinfo
  })
  
  ## SNP Scan.
  snp_scan_obj <- shiny::reactive({
    shiny::req(snpprobs_obj(), phe_mx())
    shiny::req(K_chr(), cov_df(), job_par$sex_type)
    snpprobs <- snpprobs_obj()$snpprobs
    shiny::withProgress(message = "SNP Scan ...", value = 0, {
      shiny::setProgress(1)
      snpprobs_act <- 
        qtl2pattern::snpprob_collapse(snpprobs, snp_action())
      qtl2mediate::scan1covar(phe_mx(), cov_df(), snpprobs_act, K_chr(), analyses_df(),
                  sex_type = job_par$sex_type)
    })
  })
  
  # Minimum LOD for SNP top values.
  minLOD <- reactive({
    if(shiny::isTruthy(input$minLOD)) {
      input$minLOD
    } else {
      max(3, round(max(unclass(shiny::req(snp_scan_obj()))), 1) - 1.5)
    }
  })
  output$minLOD <- shiny::renderUI({
    value <- minLOD()
    shiny::numericInput(ns("minLOD"), "LOD threshold", value, min = 0, step = 0.5)
  })
  
  ## Top SNPs table.
  top_snps_tbl <- shiny::reactive({
    shiny::req(snp_action(), snpinfo())
    drop_hilit <- max(unclass(shiny::req(snp_scan_obj()))) - 
      minLOD() 
    shiny::withProgress(message = 'Get Top SNPs ...', value = 0, {
      shiny::setProgress(1)
      qtl2pattern::top_snps_pattern(snp_scan_obj(),
                                snpinfo(),
                                drop_hilit)
    })
  })
  ## Genes and Exons.
  gene_exon_tbl <- shiny::reactive({
    shiny::req(snp_action())
    shiny::withProgress(message = 'Gene Exon Calc ...', value = 0, {
      shiny::setProgress(1)
      tops <- shiny::req(top_snps_tbl())
      gene_exons(tops, project_info())
    })
  })
  
  ## Shiny Modules
  ## SNP Association
  ass_par <- shinySNPGene("snp_gene", input, chr_pos, pheno_names,
                          snp_scan_obj, snpinfo, top_snps_tbl, 
                          gene_exon_tbl, project_info, snp_action)
  ## Allele Patterns
  pat_par <- shinySNPPattern("snp_pattern", input, chr_pos, pheno_names,
                             snp_scan_obj, snpinfo, top_snps_tbl, 
                             gene_exon_tbl, allele_info, snp_action)

  # Scan Window slider
  output$scan_window <- shiny::renderUI({
    shiny::req(pheno_names())
    rng <- round(shiny::req(win_par$peak_Mbp) + 
                   c(-1,1) * shiny::req(win_par$window_Mbp), 
                 1)
    selected <- select_range(input$scan_window, rng)
    
    shiny::sliderInput(ns("scan_window"), NULL, rng[1], rng[2],
                selected, step=.1)
  })

  ## Select phenotype for plots.
  output$pheno_name <- shiny::renderUI({
    shiny::req(pheno_names())
    shiny::selectInput(ns("pheno_name"), NULL,
                choices = shiny::req(pheno_names()),
                selected = input$pheno_name)
  })
  
  ## Button Options.
  output$phe_choice <- shiny::renderUI({
    ## Show Phenotype Choice for Scan, Pattern, Top SNPs
    pheno_choice <- FALSE
    switch(shiny::req(job_par$button),
           "SNP Association" = {
             if(!is.null(ass_par$button)) {
               pheno_choice <- (ass_par$button %in% c("Genes","Exons"))
             }
           },
           "Allele Pattern" = {
             if(!is.null(pat_par$button)) {
               pheno_choice <- (pat_par$button %in% c("By Pheno","Top SNPs"))
             }
           })
    if(pheno_choice) {
      shiny::uiOutput(ns("pheno_name"))
    }
  })
  output$win_choice <- shiny::renderUI({
    ## Show Window for Scan, Genes, By Pheno, Alls
    win_choice <- FALSE
    switch(shiny::req(job_par$button),
           "SNP Association" = {
             if(!is.null(ass_par$button)) {
               win_choice <- (ass_par$button %in% c("Scan","Genes"))
             }
           },
           "Allele Pattern" = {
             if(!is.null(pat_par$button)) {
               win_choice <- 
                 (pat_par$button %in%
                    c("By Pheno","All Phenos","All Patterns"))
             }
           })
    if(win_choice) {
      shiny::uiOutput(ns("scan_window"))
    }
  })

  ## UI Logic
  output$title <- shiny::renderUI({
    if(snp_action() == "basic")
      strong(shiny::req(job_par$button))
  })
  output$snp_input <- shiny::renderUI({
    switch(shiny::req(job_par$button),
           "SNP Association" = shinySNPGeneInput(ns("snp_gene")),
           "Allele Pattern"  = shinySNPPatternInput(ns("snp_pattern")))
  })
  output$snp_output <- shiny::renderUI({
    switch(shiny::req(job_par$button),
           "SNP Association" = shinySNPGeneOutput(ns("snp_gene")),
           "Allele Pattern"  = shinySNPPatternOutput(ns("snp_pattern")))
  })

  ## Downloads
  output$download_csv_plot <- shiny::renderUI({
    switch(shiny::req(job_par$button),
           "SNP Association" = shinySNPGeneUI(ns("snp_gene")),
           "Allele Pattern"  = shinySNPPatternUI(ns("snp_pattern")))
  })
  
  ## Return patterns
  shiny::reactive({ # patterns
    if(shiny::isTruthy(snp_action()) && shiny::isTruthy(top_snps_tbl())) {
      dplyr::arrange(
        dplyr::mutate(
          dplyr::filter(
            summary(top_snps_tbl()), 
            .data$max_lod >= 3), 
          contrast = snp_action()), 
        dplyr::desc(.data$max_lod))
    } else {
      NULL
    }
  })
})
}

shinySNPSetupUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("title")),
    shiny::uiOutput(ns("snp_input")),
    shiny::uiOutput(ns("minLOD")),
    shiny::uiOutput(ns("phe_choice")),
    shiny::uiOutput(ns("win_choice")),
    shiny::uiOutput(ns("download_csv_plot")))
}
shinySNPSetupOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("snp_output"))
}

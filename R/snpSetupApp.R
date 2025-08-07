#' Shiny SNP and Allele analysis and plot module
#'
#' Shiny module to coordinate SNP and allele analyses and plots, with interfaces \code{snpSetupUI} and  \code{snpSetupOutput}.
#'
#' @param id identifier for shiny reactive
#' @param job_par,win_par,phe_mx,cov_df,K_chr,analyses_df,project_df,allele_info,snp_action reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom dplyr arrange desc filter mutate
#' @importFrom qtl2pattern gene_exon top_snps_pattern snpprob_collapse
#' @importFrom qtl2mediate scan1covar covar_matrix which_covar
#' @importFrom shiny isTruthy moduleServer NS numericInput reactive renderUI req
#'             selectInput setProgress sliderInput tagList uiOutput withProgress
#' @importFrom rlang .data
snpSetupApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Scan",
    sidebar = bslib::sidebar(
      projectUI("project"),
      projectUI("project"),
      setParInput("set_par"),
      setupInput("setup"),
      setupUI("setup"),
      shiny::uiOutput("sex_type_input"),
      scanUI("scan")),
    scanOutput("scan")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project", projects_df)
    set_par <- setParServer("set_par", project_df)
    
    pmap_obj <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "pmap")
    })
    peak_df <- shiny::reactive({
      shiny::req(project_df(), set_par$class)
      read_project(project_df(), "peaks", class = set_par$class)
    })
    
    # set_list returns pheno_names(), win_par.
    set_list <- setupServer("setup", set_par, peak_df, pmap_obj, project_df)
    
    pheno_mx <- shiny::reactive({
      shiny::req(project_df(), set_par$class)
      pheno_names <- shiny::req(set_list$pheno_names())
      read_project(project_df(), "pheno", class = set_par$class,
                   columns = pheno_names)
    })
    covar_df <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "covar")
    })
    K_chr <- shiny::reactive({
      shiny::req(project_df())
      chr_id <- shiny::req(set_list$win_par$chr_id)
      read_project(project_df(), "kinship")[chr_id]
    })
    ## Allele names.
    allele_info <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "allele_info")
    })
    output$sex_type_input <- shiny::renderUI({
      choices <- c("A","I","F","M")
      shiny::radioButtons("sex_type", "Sex:",
                          choices,
                          input$sex_type, inline = TRUE)
    })
    
    probs_obj <- probsServer("probs", set_list$win_par, project_df)
    scanServer("scan", input, set_list$win_par, peak_df, pheno_mx, covar_df,
               K_chr, probs_obj, allele_info, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname snpSetupApp
snpSetupServer <- function(id,
  job_par, win_par, phe_mx, cov_df, K_chr, analyses_df,
  project_df, allele_info,
  snp_action = shiny::reactive({"basic"})) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    chr_pos <- shiny::reactive({
      make_chr_pos(shiny::req(win_par$chr_id), 
                   range = shiny::req(input$scan_window))
    })
    pheno_names <- shiny::reactive({
      shiny::req(project_df(), phe_mx())
      colnames(phe_mx())
    })
    
    ## Reactives
    ## SNP Probabilities.
    snpprobs_obj <- snpProbsServer("snp_probs", win_par, pheno_names,
                                   project_df)
    snpinfo <- reactive({
      shiny::req(project_df(), phe_mx())
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
        qtl2mediate::scan1covar(phe_mx(), cov_df(), snpprobs_act, K_chr(),
                                analyses_df(), sex_type = job_par$sex_type)
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
    output$minLOD_input <- shiny::renderUI({
      value <- minLOD()
      shiny::numericInput(ns("minLOD"), "LOD threshold", value,
                          min = 0, step = 0.5)
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
        gene_exons(tops, project_df())
      })
    })
    
    ## Shiny Modules
    ## SNP Association
    ass_par <- snpGeneServer("snp_gene", input, chr_pos, pheno_names,
                            snp_scan_obj, snpinfo, top_snps_tbl, 
                            gene_exon_tbl, project_df, snp_action)
    ## Allele Patterns
    pat_par <- snpPatternServer("snp_pattern", input, chr_pos, pheno_names,
                               snp_scan_obj, snpinfo, top_snps_tbl, 
                               gene_exon_tbl, allele_info, snp_action)
    
    # Scan Window slider
    output$scan_window_input <- shiny::renderUI({
      shiny::req(pheno_names())
      rng <- round(shiny::req(win_par$peak_Mbp) + 
                     c(-1,1) * shiny::req(win_par$window_Mbp), 
                   1)
      selected <- select_range(input$scan_window, rng)
      
      shiny::sliderInput(ns("scan_window"), NULL, rng[1], rng[2],
                         selected, step=.1)
    })
    
    ## Select phenotype for plots.
    output$pheno_name_input <- shiny::renderUI({
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
        shiny::uiOutput(ns("pheno_name_input"))
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
        shiny::uiOutput(ns("scan_window_input"))
      }
    })
    
    ## UI Logic
    output$title <- shiny::renderUI({
      if(snp_action() == "basic")
        strong(shiny::req(job_par$button))
    })
    output$snp_input <- shiny::renderUI({
      switch(shiny::req(job_par$button),
             "SNP Association" = snpGeneInput(ns("snp_gene")),
             "Allele Pattern"  = snpPatternInput(ns("snp_pattern")))
    })
    output$snp_output <- shiny::renderUI({
      switch(shiny::req(job_par$button),
             "SNP Association" = snpGeneOutput(ns("snp_gene")),
             "Allele Pattern"  = snpPatternOutput(ns("snp_pattern")))
    })
    
    ## Downloads
    output$download_csv_plot <- shiny::renderUI({
      switch(shiny::req(job_par$button),
             "SNP Association" = snpGeneUI(ns("snp_gene")),
             "Allele Pattern"  = snpPatternUI(ns("snp_pattern")))
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
#' @export
#' @rdname snpSetupApp
snpSetupUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("title")),
    shiny::uiOutput(ns("snp_input")),
    shiny::uiOutput(ns("minLOD_input")),
    shiny::uiOutput(ns("phe_choice")),
    shiny::uiOutput(ns("win_choice")),
    shiny::uiOutput(ns("download_csv_plot")))
}
#' @export
#' @rdname snpSetupApp
snpSetupOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("snp_output"))
}

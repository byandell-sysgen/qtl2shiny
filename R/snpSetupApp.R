#' Shiny SNP and Allele analysis and plot App
#'
#' This app is deprecated.
#' Shiny module to coordinate SNP and allele analyses and plots.
#'
#' @param id identifier for shiny reactive
#' @param hotspot_list,hap_par,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom dplyr arrange desc filter mutate
#' @importFrom qtl2pattern gene_exon top_snps_pattern snpprob_collapse
#' @importFrom qtl2mediate scan1covar covar_matrix which_covar
#' @importFrom shiny isolate isTruthy moduleServer NS numericInput reactive
#'             renderUI req selectInput setProgress tagList uiOutput
#'             withProgress
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom rlang .data
#' @importFrom bslib card layout_sidebar nav_panel page_navbar sidebar
snpSetupApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test SNP Setup",
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
      title = "snpSetup",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          hapParUI("hap_par"),                    # button
          hapParInput("hap_par"),                 # sex_type
          snpSetupInput("snp_setup")),            # <various>
        bslib::card(snpSetupOutput("snp_setup"))
#        bslib::card(snpSetupUI("snp_setup"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    hap_par <- hapParServer("hap_par")
    patterns <- snpSetupServer("snp_setup", hotspot_list, hap_par, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname snpSetupApp
snpSetupServer <- function(id, hotspot_list, hap_par, project_df,
                           snp_action = shiny::reactive({"basic"})) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Shiny Modules
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    ## SNP Association
    ass_par <- snpGeneServer("snp_gene", snp_list, project_df)
    ## Allele Patterns
    pat_par <- snpPatternServer("snp_pattern", snp_list, hotspot_list$allele_info)
    
    ## Button Options.
    output$phe_choice <- shiny::renderUI({
      ## Show Phenotype Choice for Scan, Pattern, Top SNPs
      pheno_choice <- FALSE
      switch(shiny::req(hap_par$button),
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
        snpListUI(ns("snp_list")) # pheno_name
      }
    })
    output$win_choice <- shiny::renderUI({
      ## Show Window for Scan, Genes, By Pheno, Alls
      win_choice <- FALSE
      switch(shiny::req(hap_par$button),
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
        snpListInput(ns("snp_list")) # scan_window
      }
    })
    
    ## UI Logic
    output$title <- shiny::renderUI({
      if(shiny::req(snp_action()) == "basic")
        strong(shiny::req(hap_par$button))
    })
    output$snp_input <- shiny::renderUI({
      switch(shiny::req(hap_par$button),
             "SNP Association" = snpGeneInput(ns("snp_gene")),       # button, snp_check
             "Allele Pattern"  = snpPatternInput(ns("snp_pattern"))) # button, by_choice
    })
    output$snp_output <- shiny::renderUI({
      switch(shiny::req(hap_par$button),
             "SNP Association" = snpGeneOutput(ns("snp_gene")),
             "Allele Pattern"  = snpPatternOutput(ns("snp_pattern")))
    })

    output$patterns <- DT::renderDataTable({
      snp_list$patterns()
      }, options = list(scrollX = TRUE, paging = FALSE, searching=FALSE))
    
    ## Return
    shiny::isolate(snp_list$patterns)
  })
}
#' @export
#' @rdname snpSetupApp
snpSetupInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("title")),
    shiny::uiOutput(ns("snp_input")),         # button(2), snp_check, by_choice
    snpListInput2(ns("snp_list")),            # minLOD
    shiny::uiOutput(ns("phe_choice")),        # pheno_name
    shiny::uiOutput(ns("win_choice")))        # scan_window
}
#' @export
#' @rdname snpSetupApp
snpSetupUI <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("patterns"))
}
#' @export
#' @rdname snpSetupApp
snpSetupOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("snp_output"))
}

#' Shiny SNP List App
#'
#' Shiny module to create list of SNP objects.
#'
#' @param id identifier for shiny reactive
#' @param hotspot_list,hap_par,project_df,snp_action reactive arguments
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
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom rlang .data
#' @importFrom bslib card page_sidebar sidebar
snpListApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test SNP Setup",
    sidebar = bslib::sidebar(
      projectUI("project"),              # project
      hotspotPanelInput("hotspot_list"), # class, subject_model, pheno_names, hotspot
      hotspotPanelUI("hotspot_list"),    # window_Mbp, radio, win_par, chr_ct, minLOD
      snpListInput("snp_list"),
      snpListInput2("snp_list"),
      snpListUI("snp_list")),
    snpListOutput("snp_list")
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    hap_par <- hapParServer("hap_par")
    snp_list <- snpListServer("snp_list", hotspot_list, hap_par, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname snpListApp
snpListServer <- function(id, hotspot_list, hap_par, project_df,
                          snp_action = shiny::reactive({"basic"})) { 
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    hotspot <- shiny::reactive({
      decode_hotspot(shiny::req(hotspot_list$win_par$hotspot))
    })
    pheno_names <- shiny::reactive({
      shiny::req(project_df(), hotspot_list$pheno_mx())
      colnames(hotspot_list$pheno_mx())
    })
    ## SNP Probabilities.
    snpprobs_obj <- snpProbsServer("snp_probs", hotspot_list$win_par, pheno_names,
                                   project_df)
    snpinfo <- reactive({
      shiny::req(project_df(), hotspot_list$pheno_mx())
      shiny::req(snpprobs_obj())$snpinfo
    })
    
    ## SNP Scan.
    snp_scan_obj <- shiny::reactive({
      shiny::req(snpprobs_obj(), hotspot_list$pheno_mx(), hotspot_list$peak_df())
      shiny::req(hotspot_list$kinship_list(), hotspot_list$covar_df(), hap_par$sex_type)
      snpprobs <- snpprobs_obj()$snpprobs
      shiny::withProgress(message = "SNP Scan ...", value = 0, {
        shiny::setProgress(1)
        snpprobs_act <- 
          qtl2pattern::snpprob_collapse(snpprobs, snp_action())
        scan1covar(hotspot_list$pheno_mx(), hotspot_list$covar_df(), snpprobs_act, hotspot_list$kinship_list(), hotspot_list$peak_df())
      })
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
    
    ## SNP Parameters
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
    # Scan Window slider
    output$scan_window_input <- shiny::renderUI({
      shiny::req(pheno_names())
      rng <- round(shiny::req(hotspot_list$win_par$peak_Mbp) + 
                     c(-1,1) * shiny::req(hotspot_list$win_par$window_Mbp), 
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
    
    output$show_snp_par <- shiny::renderUI({
      shiny::tagList(
        shiny::renderText(paste("minLOD:", input$minLOD)),
        shiny::renderText(paste("scan_window:",
                                paste(input$scan_window, collapse = "-"))),
        shiny::renderText(paste("pheno_name:", input$pheno_name))
      )
    })
    
    ## Return `snp_list`.
    shiny::reactiveValues(
      snp_par = input,
      pheno_names = pheno_names,
      hotspot = hotspot,
      snp_scan_obj = snp_scan_obj,
      snpinfo = snpinfo,
      top_snps_tbl = top_snps_tbl,
      gene_exon_tbl = gene_exon_tbl,
      snp_action = snp_action)
  })
}
#' @export
#' @rdname snpListApp
snpListInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("scan_window_input")) # scan_window
}
#' @export
#' @rdname snpListApp
snpListInput2 <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("minLOD_input"))      # minLOD
}
#' @export
#' @rdname snpListApp
snpListUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("pheno_name_input"))  # pheno_name
}
#' @export
#' @rdname snpListApp
snpListOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("show_snp_par"))
}

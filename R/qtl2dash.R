#' Shiny main module for qtl2
#'
#' Shiny module for phenotype selection.
#'
#' @param id identifier for shiny reactive
#' @param projects_info reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom qtl2mediate get_covar
#' @importFrom gdata humanReadable
#' @importFrom dplyr distinct filter mutate one_of select 
#' @importFrom tidyr unite
#' @importFrom shiny moduleServer NS reactive req 
#'   radioButtons selectInput
#'   dataTableOutput textOutput uiOutput
#'   renderDataTable renderText renderUI
#'   observeEvent
#'   strong tagList
#' @importFrom rlang .data
#' 
mainServer <- function(id, projects_info) {
  shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns
  
  ## Data Setup
  project_info <- reactive({ 
    shiny::req(set_par()$project_info) 
  })
  pheno_type <- shiny::reactive({
    analtbl <- shiny::req(analyses_tbl())
    c("all", sort(unique(analtbl$pheno_type)))
  })
  peaks <- shiny::reactive({
    shiny::req(project_info())
    read_project(project_info(), "peaks")
  })
  analyses_tbl <- shiny::reactive({
    shiny::req(project_info())
    ## The analyses_tbl should only have one row per pheno.
    read_project(project_info(), "analyses")
  })
  covar <- shiny::reactive({
    shiny::req(project_info())
    read_project(project_info(), "covar")
  })
  
  pmap_obj <- shiny::reactive({
    shiny::req(project_info())
    read_project(project_info(), "pmap")
  })
  kinship <- shiny::reactive({
    shiny::req(project_info())
    read_project(project_info(), "kinship")
  })
  
  set_par <- shinySetup("setup", 
                               pheno_type, peaks, 
                               pmap_obj, analyses_tbl, 
                               cov_df, projects_info)
  
  ## Continue with Plots and Analysis.
  
  ## Phenotypes and Covariates.
  analyses_df <- shiny::reactive({
    phename <- shiny::req(set_par()$pheno_names)
    dplyr::filter(analyses_tbl(), .data$pheno %in% phename)
  })
  phe_mx <- shiny::reactive({
    shiny::req(analyses_df(), project_info())
    pheno_read(project_info(), analyses_df())
  })
  cov_df <- shiny::reactive({
    qtl2mediate::get_covar(covar(), analyses_df())
  })
  
  ## Set up shiny::reactives for scan1 module.
  K_chr <- shiny::reactive({
    kinship()[set_par()$win_par$chr_id]
  })
  
  ## Allele names.
  allele_info <- shiny::reactive({
    shiny::req(project_info())
    read_project(project_info(), "allele_info")
  })
  
  ## Haplotype Analysis.
  shinyHaplo("hap_scan", set_par()$win_par, pmap_obj, phe_mx, cov_df, K_chr,
             analyses_df, covar, analyses_tbl, peaks, project_info, allele_info)
  
  ## Diplotype Analysis.
  shinyDiplo("dip_scan", set_par()$win_par, phe_mx, cov_df, K_chr, analyses_df,
             project_info, allele_info)
})
}

#' @param id shiny identifier
#' @rdname mainServer
#' @export
mainInput <- function(id) {
  ns <- shiny::NS(id)
  shinySetupInput(ns("setup"))
}
#' @rdname mainServer
#' @export
mainUI <- function(id) {
  ns <- shiny::NS(id)
  shinySetupUI(ns("setup"))
}
#' @rdname mainServer
#' @export
mainOutput <- function(id) {
  ns <- shiny::NS(id)
  shinyHaploUI(ns("hap_scan"))
}
#' @rdname mainServer
#' @export
mainOutput2 <- function(id) {
  ns <- shiny::NS(id)
  shinyDiploUI(ns("dip_scan"))
}

#' Shiny dash module for qtl2
#'
#' Shiny module for phenotype selection.
#'
#' @param id shiny identifier
#' @param projects_info reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom qtl2mediate get_covar
#' @importFrom dplyr filter 
#' @importFrom shiny moduleServer NS reactive req 
#' @importFrom rlang .data
#' 
dashServer <- function(id, projects_info) {
  shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns
  
  ## Data Setup
  peak_df <- shiny::reactive({
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
  
  project_info <- projectServer("project", projects_info)
  set_par <- setupServer("setup", peak_df, pmap_obj, analyses_tbl, cov_df,
                         project_info)
  
  ## Continue with Plots and Analysis.
  
  ## Phenotypes and Covariates.
  analyses_df <- shiny::reactive({
    phename <- set_par$pheno_names()
    if(is.null(phename)) return(NULL)
    dplyr::filter(analyses_tbl(), .data$pheno %in% phename)
  })
  phe_mx <- shiny::reactive({
    analyses <- analyses_df() 
    if(is.null(analyses)) return(NULL)
    shiny::req(project_info())
    pheno_read(project_info(), analyses)
  })
  cov_df <- shiny::reactive({
    analyses <- analyses_df() 
    if(is.null(analyses)) return(NULL)
    qtl2mediate::get_covar(covar(), analyses_df())
  })
  
  ## Set up shiny::reactives for scan1 module.
  K_chr <- shiny::reactive({
    kinship()[set_par$win_par$chr_id]
  })
  
  ## Allele names.
  allele_info <- shiny::reactive({
    shiny::req(project_info())
    read_project(project_info(), "allele_info")
  })
  
  ## Haplotype Analysis.
  haploServer("hap_scan", set_par$win_par, pmap_obj, phe_mx, cov_df, K_chr,
             analyses_df, covar, analyses_tbl, peak_df, project_info, allele_info)
  
  ## Diplotype Analysis.
  diploServer("dip_scan", set_par$win_par, phe_mx, cov_df, K_chr, analyses_df,
             project_info, allele_info)
})
}

#' @rdname dashServer
#' @export
dashInput <- function(id) {
  ns <- shiny::NS(id)
  setupInput(ns("setup"))
}
#' @rdname dashServer
#' @export
dashUI <- function(id) {
  ns <- shiny::NS(id)
  setupUI(ns("setup"))
}
#' @rdname dashServer
#' @export
dashOutput <- function(id) {
  ns <- shiny::NS(id)
  haploUI(ns("hap_scan"))
}
#' @rdname dashServer
#' @export
dashOutput2 <- function(id) {
  ns <- shiny::NS(id)
  diploUI(ns("dip_scan"))
}

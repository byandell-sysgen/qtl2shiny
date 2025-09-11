#' Shiny dash module for qtl2
#'
#' Shiny module for phenotype selection.
#'
#' @param id shiny identifier
#' @param projects_df static data frame with project information
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
dashApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Haplo",
    navbar_options = bslib::navbar_options(bg = "#2D89C8", theme = "dark"),
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
    ## Currently just uses Haplo and Diplo Apps.
    ## Need to rethink dashServer
    bslib::nav_panel(
      title = "Haplo",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          haploUI("haplo"),                     # <various>
          haploInput("haplo")),                # <various>
        haploOutput("haplo")
      )
    ),
    bslib::nav_panel(
      title = "Diplo",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(diploUI("diplo")), # <various>
        bslib::card(diploOutput("diplo"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    haploServer("haplo", hotspot_list, project_df)
    diploServer("diplo", hotspot_list, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname dashApp
dashServer <- function(id, projects_df) {
  shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns
  
  ## Data Setup
  peak_df <- shiny::reactive({
    shiny::req(project_df())
    read_project(project_df(), "peaks")
  })
  analyses_tbl <- shiny::reactive({
    shiny::req(project_df())
    ## The analyses_tbl should only have one row per pheno.
    read_project(project_df(), "analyses")
  })
  covar <- shiny::reactive({
    shiny::req(project_df())
    read_project(project_df(), "covar")
  })
  
  pmap_obj <- shiny::reactive({
    shiny::req(project_df())
    read_project(project_df(), "pmap")
  })
  kinship <- shiny::reactive({
    shiny::req(project_df())
    read_project(project_df(), "kinship")
  })
  
  project_df <- projectServer("project", projects_df)
  hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
  
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
    shiny::req(project_df())
    pheno_read(project_df(), analyses)
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
    shiny::req(project_df())
    read_project(project_df(), "allele_info")
  })
  
  ## Haplotype Analysis.
  haploServer("hap_scan", set_par$win_par, pmap_obj, phe_mx, cov_df, K_chr,
             analyses_df, covar, analyses_tbl, peak_df, project_df, allele_info)
  
  ## Diplotype Analysis.
  diploServer("dip_scan", set_par$win_par, phe_mx, cov_df, K_chr, analyses_df,
             project_df, allele_info)
})
}

#' @rdname dashApp
#' @export
dashInput <- function(id) {
  ns <- shiny::NS(id)
  hotspotPanelInput(ns("hotspot_list"))
}
#' @rdname dashApp
#' @export
dashUI <- function(id) {
  ns <- shiny::NS(id)
  hotspotPanelUI(ns("hotspot_list"))
}
#' @rdname dashApp
#' @export
dashOutput <- function(id) {
  ns <- shiny::NS(id)
  haploUI(ns("hap_scan"))
}
#' @rdname dashApp
#' @export
dashOutput2 <- function(id) {
  ns <- shiny::NS(id)
  diploUI(ns("dip_scan"))
}

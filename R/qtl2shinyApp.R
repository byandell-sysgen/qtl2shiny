#' Shiny QTL2 Shiny App
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
qtl2shinyApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Whole App",
    navbar_options = bslib::navbar_options(bg = "red", theme = "dark"),
    bslib::nav_panel(
      title = "Hotspots and Phenotypes",
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
      title = "Allele and SNP Scans",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          scanPanelInput("scan_panel"),         # <various>
          snpListInput("scan_snp_list")),       # scan_window, minLOD, pheno_name
        scanPanelOutput("scan_panel")
      )
    ),
    bslib::nav_panel(
      title = "Mediation",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          mediatePanelInput("mediate_panel")), # <various>
        mediatePanelOutput("mediate_panel")
      )
    ),
    bslib::nav_panel(
      title = "Patterns",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            patternPanelInput("pattern_panel")), # <various>
          bslib::card(
            dipParInput("dip_par")),          # snp_action
          bslib::card(
            snpListInput("pattern_snp_list")), # scan_window, minLOD, pheno_name
          bslib::card(
            dipParUI("dip_par")),             # allele_names
          width = 400),
        bslib::card(patternPanelOutput("pattern_panel"))
      )
    ),
    bslib::nav_panel(
      title = "Genotypes",
      bslib::layout_sidebar(
        bslib::card(
          dipParUI("geno_dip_par")),                          # allele_names
        bslib::card(genoInput("geno"), min_height = "100px"), # pos_Mbp
        width = 400),
      bslib::card(genoOutput("geno"))
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    
    # Hotspots and Phenotypes Panel.
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    
    # Allele and SNP Scans Panel.
    scan_snp_list <- snpListServer("scan_snp_list", hotspot_list, project_df)
    probs_obj <- probsServer("probs", hotspot_list$win_par, project_df)
    scanPanelServer("scan_panel", hotspot_list, scan_snp_list, probs_obj,
                    project_df)

    # Mediation Panel.
    mediatePanelServer("mediate_panel", hotspot_list, scan_snp_list, probs_obj,
                       project_df)
    
    # Patterns Panel.
    dip_par <- dipParServer("dip_par", hotspot_list)
    snp_action <- shiny::reactive({dip_par$snp_action})
    pattern_snp_list <-
      snpListServer("pattern_snp_list", hotspot_list, project_df, snp_action)
    pairprobs_obj <-
      pairProbsServer("pairprobs", hotspot_list$win_par, project_df)
    pattern_list <-
      patternPanelServer("pattern_panel", dip_par, hotspot_list,
                         pattern_snp_list, pairprobs_obj, project_df)

    # Genotypes Panel.
    dipParServer("geno_dip_par", hotspot_list)
    genoServer("geno", hotspot_list, pattern_list, pattern_snp_list,
               pairprobs_obj, project_df)
  }
  shiny::shinyApp(ui, server)
}

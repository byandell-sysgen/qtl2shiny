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
#' @importFrom bslib card layout_sidebar nav_panel page_navbar sidebar
qtl2shinyApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- qtl2shinyUI("qtl2shiny")
  server <- function(input, output, session) {
    qtl2shinyServer("qtl2shiny", projects_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname qtl2shinyApp
qtl2shinyServer <- function(id, projects_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    project_df <- projectServer("project_df", projects_df)
    DL <- shiny::reactiveValues()
    
    # Hotspots and Phenotypes Panel.
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    DL$hotspot <- hotspot_list
    
    # Allele and SNP Scans Panel.
    scan_snp_list <- snpListServer("scan_snp_list", hotspot_list, project_df)
    probs_obj <- probsServer("probs", hotspot_list$win_par, project_df)
    DL$scan <-
      scanPanelServer("scan_panel", hotspot_list, scan_snp_list, probs_obj,
                      project_df)
    
    # Mediation Panel.
    DL$mediate <-
      mediatePanelServer("mediate_panel", hotspot_list, scan_snp_list,
                         probs_obj, project_df)
    
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
    DL$pattern <- pattern_list
    
    # Genotypes Panel.
    dipParServer("geno_dip_par", hotspot_list)
    DL$geno <-
      genoPanelServer("geno_panel", hotspot_list, pattern_list,
                      pattern_snp_list, pairprobs_obj, project_df)
    
    output$download <- shiny::renderUI({
      # Note: `input$panel` is of form `qtl2shiny-<panel>`.
      shiny::tagList(
        shiny::renderText(paste("input$panel:", shiny::req(input$panel))),
        shiny::renderText(paste("DL:", paste(names(DL), collapse = ", "))))
    })
    # Download Module
    #downloadServer("download", DL, input$panel)
  })    
}
#' @export
#' @rdname qtl2shinyApp
qtl2shinyUI <- function(id) {
  ns <- shiny::NS(id)

  bslib::page_navbar(
    title =  "QTL2 Shiny App",
    id = ns("panel"),
    navbar_options = bslib::navbar_options(bg = "red", theme = "dark"),
    sidebar = bslib::sidebar(
      bslib::card(
        projectUI(ns("project_df")),            # project
        hotspotPanelInput(ns("hotspot_list"))), # class, subject_model, pheno_names
      bslib::card(
        shiny::uiOutput(ns("download")))
    ),
    bslib::nav_panel(
      title = "Hotspots and Phenotypes",
      value = ns("hotspot"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            hotspotPanelUI(ns("hotspot_list"))),    # window_Mbp, radio, win_par, chr_ct, minLOD
          width = 400),
        hotspotPanelOutput(ns("hotspot_list")))
    ),
    ## Currently just uses Haplo and Diplo Apps.
    ## Need to rethink dashServer
    bslib::nav_panel(
      title = "Allele and SNP Scans",
      value = ns("scan"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          scanPanelInput(ns("scan_panel")),         # <various>
          snpListInput(ns("scan_snp_list"))),       # scan_window, minLOD, pheno_name
        scanPanelOutput(ns("scan_panel"))
      )
    ),
    bslib::nav_panel(
      title = "Mediation",
      value = ns("mediate"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          mediatePanelInput(ns("mediate_panel"))), # <various>
        mediatePanelOutput(ns("mediate_panel"))
      )
    ),
    bslib::nav_panel(
      title = "Patterns",
      value = ns("pattern"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            patternPanelInput(ns("pattern_panel"))), # <various>
          bslib::card(
            dipParInput(ns("dip_par"))),          # snp_action
          bslib::card(
            snpListInput(ns("pattern_snp_list"))), # scan_window, minLOD, pheno_name
          bslib::card(
            dipParUI(ns("dip_par"))),             # allele_names
          width = 400),
        bslib::card(patternPanelOutput(ns("pattern_panel")))
      )
    ),
    bslib::nav_panel(
      title = "Genotypes",
      value = ns("geno"),
      bslib::layout_sidebar(
        bslib::card(
          dipParUI(ns("geno_dip_par"))),                           # allele_names
        bslib::card(
          genoPanelInput(ns("geno_panel")), min_height = "100px"), # pos_Mbp
        width = 400),
      bslib::card(genoPanelOutput(ns("geno_panel")))
    )
  )
}


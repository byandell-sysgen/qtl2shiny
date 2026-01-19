#' Shiny QTL2 Shiny App
#'
#' @param id shiny identifier
#' @param projects_df static data frame with project information
#'
#' @author Brian S Yandell, \email{brian.yandell@wisc.edu}
#' @keywords utilities
#'
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom qtl2mediate get_covar
#' @importFrom dplyr filter
#' @importFrom shiny moduleServer NS reactive renderText req textOutput
#' @importFrom rlang .data
#' @importFrom bslib card layout_sidebar nav_panel page_navbar sidebar
#' @importFrom downr downloadServer downloadInput
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
    download_list <- shiny::reactiveValues()

    # Hotspots and Phenotypes Panel.
    hotspot_list <- hotspotServer(
      "hotspot_list", project_df,
      shiny::reactive({
        shiny::req(input$panel)
      })
    )
    download_list$hotspot <- hotspot_list

    # Allele and SNP Scans Panel.
    scan_snp_list <- snpListServer("scan_snp_list", hotspot_list, project_df)
    probs_obj <- probsServer("probs", hotspot_list$win_par, project_df)
    download_list$scan <-
      scanServer(
        "scan_panel", hotspot_list, scan_snp_list, probs_obj,
        project_df
      )

    # Mediation Panel.
    download_list$mediate <- mediateServer(
      "mediate_panel", hotspot_list,
      scan_snp_list, probs_obj, project_df
    )

    # Patterns Panel.
    dip_par <- dipParServer("dip_par", hotspot_list)
    snp_action <- shiny::reactive({
      dip_par$snp_action
    })
    pattern_snp_list <-
      snpListServer("pattern_snp_list", hotspot_list, project_df, snp_action)
    pairprobs_obj <-
      pairProbsServer("pairprobs", hotspot_list$win_par, project_df)
    pattern_list <-
      patternServer(
        "pattern_panel", dip_par, hotspot_list,
        pattern_snp_list, pairprobs_obj, project_df
      )
    download_list$pattern <- shiny::isolate(pattern_list$download_list)

    # Genotypes Panel.
    dipParServer("geno_dip_par", hotspot_list)
    download_list$geno <- genoServer(
      "geno_panel", hotspot_list, pattern_list,
      pattern_snp_list, pairprobs_obj, project_df
    )

    output$download_panel <- shiny::renderText(
      paste("input$panel:", shiny::req(input$panel))
    )
    output$download_names <- shiny::renderText(
      paste("DL:", paste(names(download_list), collapse = ", "))
    )
    # output$download <- shiny::renderUI({
    #   # Note: `input$panel` is of form `qtl2shiny-<panel>`.
    #   shiny::tagList(
    #     shiny::renderText(paste("input$panel:", shiny::req(input$panel))),
    #     shiny::renderText(paste("DL:", paste(names(download_list), collapse = ", "))))
    # })
    # Download
    download_list_panel <- shiny::reactive({
      # Note: `input$panel` is of form `qtl2shiny-<panel>`.
      input_panel <- stringr::str_remove(
        shiny::req(input$panel),
        "qtl2shiny-"
      )
      download_list[[input_panel]]
    })
    downr::downloadServer("download", download_list_panel)
  })
}
#' @export
#' @rdname qtl2shinyApp
qtl2shinyUI <- function(id) {
  ns <- shiny::NS(id)

  bslib::page_navbar(
    title = "QTL2 Shiny App",
    id = ns("panel"),
    navbar_options = bslib::navbar_options(bg = "red", theme = "dark"),
    sidebar = bslib::sidebar(
      bslib::card(
        projectUI(ns("project_df")), # project
        hotspotInput(ns("hotspot_list"))
      ), # class, subject_model, pheno_names
      bslib::card(
        shiny::textOutput(ns("download_panel")),
        shiny::textOutput(ns("download_names"))
      )
    ),
    header = downr::downloadInput(ns("download")),
    bslib::nav_panel(
      title = "Hotspots and Phenotypes",
      value = ns("hotspot"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(hotspotUI(ns("hotspot_list"))), # window_Mbp, radio, win_par, chr_ct, minLOD
          width = 400
        ),
        hotspotOutput(ns("hotspot_list"))
      )
    ),
    ## Currently just uses Haplo and Diplo Apps.
    ## Need to rethink dashServer
    bslib::nav_panel(
      title = "Allele and SNP Scans",
      value = ns("scan"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          scanInput(ns("scan_panel")), # <various>
          snpListInput(ns("scan_snp_list"))
        ), # scan_window, minLOD, pheno_name
        scanOutput(ns("scan_panel"))
      )
    ),
    bslib::nav_panel(
      title = "Patterns",
      value = ns("pattern"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(patternInput(ns("pattern_panel"))), # <various>
          bslib::card(dipParInput(ns("dip_par"))), # snp_action
          bslib::card(snpListInput(ns("pattern_snp_list"))), # scan_window, minLOD, pheno_name
          bslib::card(dipParUI(ns("dip_par"))), # allele_names
          width = 400
        ),
        bslib::card(patternOutput(ns("pattern_panel")))
      )
    ),
    bslib::nav_panel(
      title = "Genotypes",
      value = ns("geno"),
      bslib::layout_sidebar(
        bslib::card(
          dipParUI(ns("geno_dip_par"))
        ), # allele_names
        bslib::card(
          genoInput(ns("geno_panel")),
          min_height = "100px"
        ), # pos_Mbp
        width = 400
      ),
      bslib::card(genoOutput(ns("geno_panel")))
    ),
    bslib::nav_panel(
      title = "Mediation",
      value = ns("mediate"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          mediateInput(ns("mediate_panel"))
        ), # <various>
        mediateOutput(ns("mediate_panel"))
      )
    )
  )
}

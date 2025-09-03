#' Hotspot Parts App
#'
#' @export
hotspotPartsApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Hotspot Parts",
    sidebar = bslib::sidebar(
      bslib::card(
        projectUI("project_df"),            # project
        hotspotPanelInput("hotspot_list")), # class, subject_model, pheno_names, hotspot
      bslib::card(
        hotspotPanelUI("hotspot_list")),   # window_Mbp, radio, win_par, chr_ct, minLOD
      width = 400
    ),
    bslib::nav_panel(
      title = "HotspotPanel",
      hotspotPanelOutput("hotspot_list")
    ),
    bslib::nav_panel(
      title = "SetPar",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          setParInput("set_par"), # class, subject_model
          setParUI("set_par")),   # window_Mbp
        bslib::card(setParOutput("set_par")))
    ),
    bslib::nav_panel(
      title = "Peak",
      bslib::card(peakOutput("peak_df"))
    ),
    #** Not working.
    bslib::nav_panel(
      title = "Hotspot",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(hotspotInput("hotspot_df")), # chr_ct, minLOD
        bslib::card(hotspotOutput("hotspot_df")),             # hotspot_plot
        bslib::card(hotspotUI("hotspot_df")))                 # hotspot_table
    ),
    bslib::nav_panel(
      title = "WinPar",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(winParInput("win_par")), # hotspot
        bslib::card(winParOutput("win_par")))
    ),
    bslib::nav_panel(
      title = "PhenoNames",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          phenoNamesInput("pheno_names")), # pheno_names
        bslib::card(
          phenoNamesUI("pheno_names"),
          phenoNamesOutput("pheno_names")
        )
      )
    ),
    bslib::nav_panel(
      title = "Pheno",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(phenoUI("pheno_mx")), # raw_data
        bslib::card(phenoOutput("pheno_mx"))
      )
    ),
    bslib::nav_panel(
      title = "PhenoPlot",
      bslib::card(phenoPlotOutput("pheno_plot")),
      bslib::card(phenoPlotUI("pheno_plot"))
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    
    # Use result `hotspot_list` for all other servers.
    setParServer("set_par", project_df)
    peakServer("peak_df", hotspot_list$set_par, project_df)
    hotspotServer("hotspot_df", hotspot_list$set_par, hotspot_list$peak_df,
                  hotspot_list$pmap_obj, project_df)
    winParServer("win_par", hotspot_list$hotspot_df, project_df)
    phenoNamesServer("pheno_names", hotspot_list$set_par, hotspot_list$win_par,
                     hotspot_list$peak_df, project_df)
    phenoServer("pheno_mx", hotspot_list$set_par, hotspot_list$pheno_names,
                project_df)
    phenoPlotServer("pheno_plot", hotspot_list$pheno_names,
                    hotspot_list$pheno_mx, hotspot_list$covar_df)
  }
  shiny::shinyApp(ui, server)
}

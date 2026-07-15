#' Shiny Scatter App
#'
#' @param id identifier for shiny reactive
#' @param hotspot_list,pattern_list,snp_list,pairprobs_obj,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom shiny isTruthy moduleServer NS plotOutput reactive renderPlot renderUI req tagList uiOutput
#' @importFrom dplyr left_join
#' @importFrom bslib card layout_sidebar nav_panel page_navbar sidebar
#' @importFrom downr downloadServer downloadInput
scatterApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Scatter Panel",
    theme = qtl2shiny_theme(),
    navbar_options = bslib::navbar_options(bg = "#2D89C8", theme = "dark"),
    bslib::nav_panel(
      title = "Hotspots",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            projectUI("project_df"),          # project
            hotspotInput("hotspot_list")),    # class, subject_model, pheno_names, hotspot
          bslib::card(
            hotspotUI("hotspot_list")),       # window_Mbp, radio, win_par, chr_ct, minLOD
          width = 400),
        hotspotOutput("hotspot_list"))
    ),
    bslib::nav_panel(
      title = "Scatter Plot",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::card(
            patternDataInput("pattern_list"), # button, blups, pheno_name
            patternDataUI("pattern_list")),   # pattern
          bslib::card(
            dipParInput("dip_par")),          # snp_action
          bslib::card(
            snpListInput("snp_list")),        # scan_window, minLOD, pheno_name
          bslib::card(
            dipParUI("dip_par")),             # allele_names
          width = 400),
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            scatterInput("scatter_panel"),
            width = 300,
            position = "right"),
          downr::downloadInput("download"),
          bslib::card(
            scatterOutput("scatter_panel"),
            full_screen = TRUE
          )
        )
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotServer("hotspot_list", project_df)
    dip_par <- dipParServer("dip_par", hotspot_list)
    snp_action <- shiny::reactive({dip_par$snp_action})
    snp_list <- snpListServer("snp_list", hotspot_list, project_df, snp_action)
    pairprobs_obj <-
      pairProbsServer("pairprobs", hotspot_list$win_par, project_df)
    pattern_list <- patternDataServer("pattern_list", dip_par, hotspot_list,
                                      snp_list, pairprobs_obj, project_df)
    download_list <-
      scatterServer("scatter_panel", hotspot_list, pattern_list, snp_list,
                     pairprobs_obj, project_df)
    downr::downloadServer("download", download_list)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname scatterApp
scatterServer <- function(id, hotspot_list, pattern_list, snp_list,
                          pairprobs_obj, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Gather genotype data at selected pos_Mbp position using genoDataServer
    geno_list <- genoDataServer("geno_list", hotspot_list, snp_list,
                                pairprobs_obj, project_df)

    # UI inputs for X and Y variable selection
    output$x_var_input <- shiny::renderUI({
      choices <- shiny::req(hotspot_list$pheno_names())
      selected <- choices[1]
      shiny::selectInput(ns("x_var"), "X Variable:", choices = choices, selected = selected)
    })
    
    output$y_var_input <- shiny::renderUI({
      choices <- shiny::req(hotspot_list$pheno_names())
      selected <- ifelse(length(choices) > 1, choices[2], choices[1])
      shiny::selectInput(ns("y_var"), "Y Variable:", choices = choices, selected = selected)
    })

    # Prepare merged reactive data frame containing x, y, and metadata for plotting
    scatter_df <- shiny::reactive({
      shiny::req(input$x_var, input$y_var, hotspot_list$pheno_mx(), 
                 geno_list$Table())
      
      phe_mx <- hotspot_list$pheno_mx()
      geno_tab <- geno_list$Table()
      
      # Determine row names matching subjects
      subjects <- rownames(phe_mx)
      
      # Build X and Y columns (phenotype names only)
      x_val <- phe_mx[, input$x_var]
      y_val <- phe_mx[, input$y_var]
      
      # Create X and Y data frame
      plot_df <- data.frame(
        subject = subjects,
        x = x_val,
        y = y_val,
        stringsAsFactors = FALSE
      )
      
      # Add scan patterns (all columns after the first one)
      if (ncol(geno_tab) > 1) {
        for (col in colnames(geno_tab)[-1]) {
          plot_df[[col]] <- factor(geno_tab[, col])
        }
      }
      
      # Add covariates (only sex and diet) if available
      cov_df <- hotspot_list$covar_df()
      if (!is.null(cov_df) && nrow(cov_df) > 0) {
        df_cov <- data.frame(subject = rownames(cov_df), cov_df, check.names = FALSE)
        if (m <- match("sex", tolower(colnames(df_cov)), nomatch = 0)) {
          plot_df$sex <- factor(df_cov[[m]])
        }
        if (md <- match("diet", tolower(colnames(df_cov)), nomatch = 0)) {
          plot_df$diet <- factor(df_cov[[md]])
        }
      }
        
      plot_df
    })

    # Call the generic scatterPlotServer module
    scatter_plot_obj <- scatterPlotServer("scatter_plot", scatter_df,
                                          shiny::reactive(input$x_var),
                                          shiny::reactive(input$y_var))

    # Download interface
    download_Plot <- shiny::reactive({
      shiny::req(scatter_plot_obj())
    })
    
    download_Table <- shiny::reactive({
      shiny::req(scatter_df())
    })
    
    download_Filename <- shiny::reactive({
      out <- paste0(shiny::req(pattern_list$pheno_name()), "_Scatter")
      c(Plot = out, Table = out)
    })
    
    download_Type <- shiny::reactive({
      "Plot"
    })
    
    download_list <- shiny::reactiveValues(
      Filename = download_Filename,
      Plot     = download_Plot,
      Table    = download_Table,
      Type     = download_Type)
      
    download_list
  })
}
#' @export
#' @rdname scatterApp
scatterInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    genoDataInput(ns("geno_list")),
    shiny::uiOutput(ns("x_var_input")),
    shiny::uiOutput(ns("y_var_input")),
    scatterPlotInput(ns("scatter_plot"))
  )
}
#' @export
#' @rdname scatterApp
scatterOutput <- function(id) {
  ns <- shiny::NS(id)
  scatterPlotOutput(ns("scatter_plot"))
}

#' Shiny phenotype selection
#'
#' Shiny module for phenotype selection.
#'
#' @param id identifier for shiny reactive
#' @param set_par,win_par,peak_df,analyses_tbl,covar,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom dplyr arrange desc filter select
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny moduleServer NS radioButtons reactive req tagList uiOutput
#' @importFrom rlang .data
phenoApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Pheno",
    sidebar = bslib::sidebar(
      projectUI("project"),
      shiny::uiOutput("pheno_group_input"),
      shiny::uiOutput("dataset_input"),
      peakInput("peak"),
      hotspotInput("hotspot"), # chr_ct, minLOD, window_Mbp
      phenoInput("pheno"),
      phenoUI("pheno")),
    phenoOutput("pheno")
  )
  server <- function(input, output, session) {
    projects_info <- shiny::reactive({projects})
    
    peak_df <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "peaks")
    })
    pmap_obj <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "pmap")
    })
    covar <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "covar")
    })
    
    # `analyses_tbl` has `pheno_group`, `pheno_type`
    analyses_tbl <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "analyses")
    })
    # Input `set_par$pheno_group`.
    output$pheno_group_input <- shiny::renderUI({
      shiny::req(analyses_tbl())
      choices <- sort(unique(analyses_tbl()$pheno_group))
      if(is.null(selected <- input$pheno_group))
        selected <- choices[1]
      shiny::selectInput("pheno_group", "",
        choices = as.list(choices), selected = selected, multiple = TRUE)
    })
    # Input `set_par$dataset`.
    output$dataset_input <- shiny::renderUI({
      shiny::req(analyses_tbl())
      choices <- c("all", sort(unique(analyses_tbl()$pheno_type)))
      if(is.null(selected <- input$dataset))
        selected <- NULL
      shiny::selectInput("dataset", "Phenotype Set",
        choices = as.list(choices), selected = selected, multiple = TRUE)
    })

    project_df <- projectServer("project", projects_df)
    hotspot_df <- hotspotServer("hotspot", input, peak_df, pmap_obj,
                                project_df)
    win_par <- peakServer("peak", input, peak_df, pmap_obj, hotspot_df,
                          project_df)
    pheno_names <- phenoServer("pheno", input, win_par, peak_df, analyses_tbl,
                               covar, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname phenoApp
phenoServer <- function(id, set_par, win_par, peak_df, analyses_tbl, covar,
                        project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Restrict peaks to region.
    analyses_set <- shiny::reactive({
      shiny::req(project_df(), analyses_tbl())
      set_analyses(set_par$dataset, set_par$pheno_group, analyses_tbl())
    })
    output$filter <- shiny::renderUI({
      shiny::checkboxInput(ns("filter"),
        paste0("Peak on chr ", win_par$chr_id, " in ",
               paste(win_par$peak_Mbp + c(-1,1) * win_par$window_Mbp,
                     collapse = "-"), "?"),
        TRUE)
    })
    peak_dataset_df <- shiny::reactive({
      shiny::req(project_df(), analyses_set(), peak_df())
      chr_id <- shiny::req(win_par$chr_id)
      peak_Mbp <- shiny::req(win_par$peak_Mbp)
      window_Mbp <- shiny::req(win_par$window_Mbp)
      peaks_in_pos(analyses_set(), peak_df(),
                   shiny::isTruthy(input$filter),
                   chr_id, peak_Mbp, window_Mbp)
    })

    # Input `input$pheno_names`.
    output$pheno_names_input <- shiny::renderUI({
      shiny::req(project_df(), win_par$chr_id, win_par$peak_Mbp, win_par$window_Mbp)
      out <- select_phenames(input$pheno_names, peak_dataset_df(), win_par$local,
                             win_par$chr_id, win_par$peak_Mbp, win_par$window_Mbp)
      shiny::selectInput(ns("pheno_names"), out$label,
                         choices = out$choices,
                         selected = out$selected,
                         multiple = TRUE)
    })
    shiny::observeEvent(project_df(), {
      output$num_pheno <- shiny::renderText({
        num_pheno(input$pheno_names, analyses_tbl())
      })
      shiny::req(win_par$chr_id, win_par$peak_Mbp, win_par$window_Mbp)
      out <- select_phenames("none", peak_dataset_df(), win_par$local,
        win_par$chr_id, win_par$peak_Mbp, win_par$window_Mbp)
      updateSelectInput(session, "pheno_names", out$label,
                        choices = out$choices, selected = "none")
    })
    
    ## Density or scatter plot of phenotypes.
    analyses_plot <- shiny::reactive({
      shiny::req(analyses_tbl())
      phename <- shiny::req(input$pheno_names)
      dplyr::filter(analyses_tbl(), .data$pheno %in% phename)
    })
    phe_mx <- shiny::reactive({
      pheno_read(project_df(), analyses_plot())
    })
    raw_phe_mx <- shiny::reactive({
      pheno_read(project_df(), analyses_plot(), FALSE)
    })
    
    pheno_names <- shiny::reactive(input$pheno_names)
    analyses_df <- shiny::reactive({
      phename <- pheno_names()
      if(is.null(phename)) return(NULL)
      dplyr::filter(analyses_tbl(), .data$pheno %in% phename)
    })
    cov_df <- shiny::reactive({
      analyses <- analyses_df() 
      if(is.null(analyses)) return(NULL)
      qtl2mediate::get_covar(covar(), analyses_df())
    })
    
    phenoPlotServer("PhenoPlotRaw", pheno_names, raw_phe_mx, cov_df)
    phenoPlotServer("PhenoPlotTrans", pheno_names, phe_mx, cov_df)
    
    # Show data.
    output$radio_input <- renderUI({
      shiny::radioButtons(ns("radio"), NULL,
                          c("Covariates","Trans Data","Raw Data"),
                          input$radio)
    })
    output$show_data <- renderUI({
      switch(shiny::req(input$radio),
        "Raw Data"   = phenoPlotUI(ns("PhenoPlotRaw")), # pheno_plot, pheno_table
        "Trans Data" = phenoPlotUI(ns("PhenoPlotTrans")), # pheno_plot, pheno_table
        "Covariates" = DT::dataTableOutput(ns("analyses_tbl"))) # analyses_table
    })
    
    # Output the analyses table
    output$analyses_tbl <- DT::renderDataTable({
      collapse_covar(analyses_plot())
    }, options = list(scrollX = TRUE, pageLength = 5))
    
    ## Return.
    pheno_names
  })
}
#' @export
#' @rdname phenoApp
phenoInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("pheno_names_input")),
    shiny::uiOutput(ns("filter"))
  )
}
#' @export
#' @rdname phenoApp
phenoUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("radio_input"))
}
#' @export
#' @rdname phenoApp
phenoOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("show_data"))
}

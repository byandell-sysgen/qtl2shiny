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
      peakUI("peak"),
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
    analyses_tbl <- shiny::reactive({
      shiny::req(project_df())
      ## The analyses_tbl should only have one row per pheno.
      read_project(project_df(), "analyses")
    })
    covar <- shiny::reactive({
      shiny::req(project_df())
      read_project(project_df(), "covar")
    })
    
    # Input `set_par$pheno_group`.
    pheno_group <- shiny::reactive({
      shiny::req(project_df())
      sort(unique(shiny::req(analyses_tbl())$pheno_group))
    }, label = "pheno_group")
    output$pheno_group_input <- shiny::renderUI({
      shiny::req(choices <- pheno_group())
      if(is.null(selected <- input$pheno_group)) {
        selected <- choices[1]
      }
      shiny::selectInput("pheno_group", "",
                         choices = as.list(choices),
                         selected = selected,
                         multiple = TRUE)
    })

    # Reactive `pheno_type()`.
    pheno_type <- shiny::reactive({
      shiny::req(project_df())
      phe_gp <- shiny::req(input$pheno_group)
      analyses_group <- 
        dplyr::filter(
          shiny::req(analyses_tbl()),
          pheno_group %in% phe_gp)
      sort(unique(analyses_group$pheno_type))
    })
    
    # Input `set_par$dataset`.
    output$dataset_input <- shiny::renderUI({
      shiny::req(project_df())
      choices <- c("all", shiny::req(pheno_type()))
      if(is.null(selected <- input$dataset))
        selected <- NULL
      shiny::selectInput("dataset", "Phenotype Set",
                         choices = as.list(choices),
                         selected = selected,
                         multiple = TRUE)
    })
    
    project_df <- projectServer("project", projects_df)
    win_par <- peakServer("peak", input, peak_df, pmap_obj, project_df)
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
    
    # Output the peaks table
    output$peaks <- DT::renderDataTable({
      dplyr::arrange(
        dplyr::select(
          peak_df(), .data$pheno, .data$chr, .data$pos, .data$lod),
        dplyr::desc(.data$lod))
    }, options = list(scrollX = TRUE, pageLength = 5,
                      lengthMenu = c(5,10,25)))
    
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
                          c("LOD Peaks","Covariates",
                            "Trans Data","Raw Data"),
                          input$radio)
    })
    output$show_data <- renderUI({
      shiny::tagList(
        switch(shiny::req(input$radio),
               "Raw Data"   = phenoPlotUI(ns("PhenoPlotRaw")),
               "Trans Data" = phenoPlotUI(ns("PhenoPlotTrans")),
               "Covariates" = DT::dataTableOutput(ns("analyses_tbl"))),
        if(!(input$radio %in% c("Raw Data","Trans Data")))
          DT::dataTableOutput(ns("peaks")))
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

#' Shiny Genes and Exons with nearby SNPs module
#'
#' Shiny module for scan1 analysis and plots, with interfaces \code{geneExonInput}, \code{geneExonUI} and  \code{geneExonOutput}.
#'
#' @param id identifier for shiny reactive
#' @param snp_list reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom ggplot2 autoplot ggtitle
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny column downloadButton downloadHandler fluidRow moduleServer
#'             NS plotOutput reactive renderPlot renderUI req selectInput
#'             setProgress uiOutput updateSelectInput withProgress
#' @importFrom utils write.csv
#' @importFrom grDevices dev.off pdf
#' @importFrom rlang .data
#' @importFrom bslib card layout_sidebar navset_tab nav_panel
#'             page_navbar sidebar
geneExonApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Gene Exon",
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
    bslib::nav_panel(
      title = "geneExon",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          geneExonInput("gene_exon"),            # gene_name
          snpListInput2("snp_list"),             # minLOD
          snpListUI("snp_list"),                 # pheno_name
          snpListInput("snp_list")),             # scan_window
        bslib::card(geneExonOutput("gene_exon"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    geneExonServer("gene_exon", snp_list)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname geneExonApp
geneExonServer <- function(id, snp_list) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pheno_names <- shiny::reactive({
      sort(unique(shiny::req(snp_list$top_snps_tbl()$pheno)))
    })
    summary_gene_exon <- shiny::reactive({
      summary(shiny::req(snp_list$gene_exon_tbl()),
              top_snps_tbl = shiny::req(snp_list$top_snps_tbl()))
    })
    gene_names <- shiny::reactive({
      pheno_name <- shiny::req(snp_list$snp_par$pheno_name)
      gene_in <- summary_gene_exon()
      if(nrow(gene_in)) {
        if(pheno_name %in% names(gene_in))
          gene_in <- gene_in[!is.na(gene_in[[pheno_name]]),]
        else
          return(NULL)
      }
      ## Order by decreasing LOD.
      if(nrow(gene_in))
        gene_in$gene[order(-gene_in[[pheno_name]])]
      else
        NULL
    })
    gene_exon_pheno <- shiny::reactive({
      pheno_name <- shiny::req(snp_list$snp_par$pheno_name)
      gene_in <- gene_names()
      if(length(gene_in)) {
        subset(snp_list$gene_exon_tbl(), gene_in)
      } else {
        NULL
      }
    })
    
    output$exon_table <- DT::renderDataTable({
      shiny::withProgress(message = 'Gene Exon Table ...', value = 0, {
        shiny::setProgress(1)
        summary_gene_exon()
      })
    }, options = list(scrollX = TRUE, pageLength = 5,
                      lengthMenu = list(c(5,10,20,-1),
                                        list("5","10","20","all"))))
    output$gene_name <- shiny::renderUI({
      selected <- input$gene_name
      choices <- gene_names()
      if(!isTruthy(selected %in% choices))
        selected <- choices[1]
      shiny::selectInput(ns("gene_name"), NULL,
                         choices = choices,
                         selected = selected)
    })
    observeEvent(gene_names(), {
      selected <- input$gene_name
      choices <- gene_names()
      if(!isTruthy(selected %in% choices))
        selected <- choices[1]
      shiny::updateSelectInput(session, "gene_name", NULL,
                               choices = choices,
                               selected = selected)
    })
    
    output$exon_plot <- shiny::renderPlot({
      if(is.null(input$gene_name)) {
        plot_null()
      } else {
        shiny::req(snp_list$top_snps_tbl(), snp_list$gene_exon_tbl(),
                   gene_names())
        gene_name <- shiny::req(input$gene_name)
        pheno_name <- shiny::req(snp_list$snp_par$pheno_name)
        shiny::withProgress(message = 'Gene Exon Plot ...', value = 0, {
          shiny::setProgress(1)
          plot_gene_exons(gene_exon_pheno(), 
                          dplyr::filter(snp_list$top_snps_tbl(),
                                        .data$pheno == pheno_name),
                          gene_name, paste(pheno_name, snp_list$snp_action()))
        })
      }
    })
    
    ## Outputs
    output$exon_input <- shiny::renderUI({
      switch(shiny::req(input$exon_tab),
             Plot    = shiny::uiOutput(ns("gene_name")))
    })
  })
}
#' @export
#' @rdname geneExonApp
geneExonInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("exon_input"))
}
#' @export
#' @rdname geneExonApp
geneExonOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    id = ns("exon_tab"),
    bslib::nav_panel("Plot", bslib::card(
      shiny::plotOutput(ns("exon_plot")))),
    bslib::nav_panel("Summary", bslib::card(
      DT::dataTableOutput(ns("exon_table")))))
}

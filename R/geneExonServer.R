#' Shiny Genes and Exons with nearby SNPs module
#'
#' Shiny module for scan1 analysis and plots, with interfaces \code{geneExonInput}, \code{geneExonUI} and  \code{geneExonOutput}.
#'
#' @param id identifier for shiny reactive
#' @param snp_par,chr_pos,top_snps_tbl,gene_exon_tbl,snp_action reactive arguments
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
geneExonServer <- function(id, snp_par, chr_pos, top_snps_tbl, gene_exon_tbl,
                          snp_action = shiny::reactive({"basic"})) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pheno_names <- shiny::reactive({
      sort(unique(shiny::req(top_snps_tbl()$pheno)))
    })
    summary_gene_exon <- shiny::reactive({
      summary(shiny::req(gene_exon_tbl()),
              top_snps_tbl = shiny::req(top_snps_tbl()))
    })
    gene_names <- shiny::reactive({
      pheno_name <- shiny::req(snp_par$pheno_name)
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
      pheno_name <- shiny::req(snp_par$pheno_name)
      gene_in <- gene_names()
      if(length(gene_in)) {
        subset(gene_exon_tbl(), gene_in)
      } else {
        NULL
      }
    })
    
    output$gene_sum <- DT::renderDataTable({
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
    
    output$gene_plot <- shiny::renderPlot({
      if(is.null(input$gene_name)) {
        plot_null()
      } else {
        shiny::req(top_snps_tbl(), gene_exon_tbl(), gene_names())
        gene_name <- shiny::req(input$gene_name)
        pheno_name <- shiny::req(snp_par$pheno_name)
        shiny::withProgress(message = 'Gene Exon Plot ...', value = 0, {
          shiny::setProgress(1)
          plot_gene_exons(gene_exon_pheno(), 
                          dplyr::filter(top_snps_tbl(), .data$pheno == pheno_name),
                          gene_name, paste(pheno_name, snp_action()))
        })
      }
    })
    
    ## Outputs
    output$exon_input <- shiny::renderUI({
      switch(shiny::req(input$button),
             Plot    = shiny::uiOutput(ns("gene_name")))
    })
    output$exon_output <- shiny::renderUI({
      switch(shiny::req(input$button),
             Plot    = shiny::plotOutput(ns("gene_plot")),
             Summary = DT::dataTableOutput(ns("gene_sum")))
    })
    
    ## Downloads.
    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        file.path(paste0("gene_exon_", chr_pos(), "_", snp_action(), ".csv")) },
      content = function(file) {
        utils::write.csv(shiny::req(summary_gene_exon()), file)
      }
    )
    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        file.path(paste0("gene_exon_", chr_pos(), "_", snp_action(), ".pdf")) },
      content = function(file) {
        gene_exon <- shiny::req(gene_exon_pheno())
        pheno_name <- shiny::req(snp_par$pheno_name)
        top_snps <- dplyr::filter(shiny::req(top_snps_tbl()), .data$pheno == pheno_name)
        grDevices::pdf(file, width = 9)
        for(gene_name in shiny::req(gene_names())) {
          print(plot_gene_exons(gene_exon, top_snps,
                                gene_name, pheno_name))
        }
        grDevices::dev.off()
      }
    )
    output$select <- shiny::renderUI({
      shiny::selectInput(ns("button"), NULL, c("Plot","Summary"),
                         input$button)
    })
  })
}
#' @export
#' @rdname geneExonServer
geneExonInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::uiOutput(ns("select")),
    shiny::uiOutput(ns("exon_input")))
}
#' @export
#' @rdname geneExonServer
geneExonUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(6, shiny::downloadButton(ns("downloadData"), "CSV")),
    shiny::column(6, shiny::downloadButton(ns("downloadPlot"), "Plots")))
}
#' @export
#' @rdname geneExonServer
geneExonOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("exon_output"))
}

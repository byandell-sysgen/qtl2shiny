#' Shiny SNP Features in SNP Region module
#'
#' Shiny module for scan1 analysis and plots, with interfaces \code{snpFeatureInput}, \code{snpFeatureUI} and  \code{snpFeatureOutput}.
#'
#' @param id identifier for shiny reactive
#' @param snp_list reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
#' @importFrom ggplot2 autoplot
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny column downloadButton downloadHandler fluidRow moduleServer
#'             NS plotOutput reactive renderPlot renderUI req selectInput
#'             setProgress tagList uiOutput withProgress
#' @importFrom utils write.csv
#' @importFrom grDevices dev.off pdf   
snpFeatureServer <- function(id, snp_list) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    top_feature <- shiny::reactive({
      shiny::req(snp_list$top_snps_tbl(), snp_list$snp_scan_obj(),
                 snp_list$snpinfo(), snp_list$gene_exon_tbl())
      shiny::withProgress(message = 'Merging gene info ...', value = 0,
                          {
                            shiny::setProgress(1)
                            topSNPs(snp_list$top_snps_tbl(), snp_list$snpinfo(),
                                    snp_list$snp_scan_obj(), snp_list$gene_exon_tbl(),
                                    snp_list$snp_par$pheno_name)
                          })
    })
    output$feature_cons_table <- DT::renderDataTable({
      tops <- shiny::req(top_feature(), "SNP type")
      summary(tops)
    }, options = list(scrollX = TRUE, paging = FALSE, searching=FALSE))
    output$feature_pattern_table <- DT::renderDataTable({
      summary(top_feature(), "pattern")
    }, options = list(scrollX = TRUE, paging = FALSE, searching=FALSE))
    phename <- shiny::reactive({dimnames(snp_list$snp_scan_obj())[[2]]})
    output$feature_pattern_plot <- shiny::renderPlot({
      shiny::req(top_feature(), snp_list$snp_par$pheno_name)
      ggplot2::autoplot(top_feature(), snp_list$snp_par$pheno_name, "consequence")
    })
    output$feature_cons_plot <- shiny::renderPlot({
      shiny::req(top_feature(), snp_list$snp_par$pheno_name)
      ggplot2::autoplot(top_feature(), snp_list$snp_par$pheno_name, "pattern")
    })
    output$by_choice_input <- shiny::renderUI({
      switch(input$by_choice,
             Pattern = {
               shiny::tagList(
                 shiny::plotOutput(ns("feature_pattern_plot")),
                 DT::dataTableOutput(ns("feature_pattern_table")))
             },
             Consequence = {
               shiny::tagList(
                 shiny::plotOutput(ns("feature_cons_plot")),
                 DT::dataTableOutput(ns("feature_cons_table")))
             })
    })
    
    ## Downloads.
    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        file.path(paste0("top_feature_", snp_list$chr_pos(), "_", snp_list$snp_action(), ".csv")) },
      content = function(file) {
        utils::write.csv(shiny::req(top_feature()), file)
      }
    )
    ## This does not work as items below do not exist.
    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        file.path(paste0("top_feature_", snp_list$chr_pos(), "_", snp_list$snp_action(), ".pdf")) },
      content = function(file) {
        shiny::req(top_feature())
        grDevices::pdf(file, width = 9)
        for(phenoi in phename()) {
          print(ggplot2::autoplot(top_feature(), phenoi, "consequence"))
          print(ggplot2::autoplot(top_feature(), phenoi, "pattern"))
        }
        grDevices::dev.off()
      }
    )
  })
}
#' @export
#' @rdname snpFeatureServer
snpFeatureInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::selectInput(ns("by_choice"), NULL, c("Pattern","Consequence")) # by_choice
}
#' @export
#' @rdname snpFeatureServer
snpFeatureUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(6, shiny::downloadButton(ns("downloadData"), "CSV")),
    shiny::column(6, shiny::downloadButton(ns("downloadPlot"), "Plots")))
}
#' @export
#' @rdname snpFeatureServer
snpFeatureOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("by_choice_input"))
}

#' Shiny Diplotype module
#'
#' Shiny diplotype SNP/Gene action analysis, with interface \code{diploUI}.
#' 
#' @param id identifier for shiny reactive
#' @param win_par,phe_mx,cov_df,K_chr,analyses_df,project_df,allele_info reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom shiny mainPanel moduleServer NS radioButtons reactive renderText
#'             renderUI req selectInput sidebarPanel strong tagList textOutput
#'             uiOutput
diploServer <- function(id, win_par, phe_mx, cov_df, K_chr, analyses_df,
                       project_df, allele_info) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Replace with decode_hotspot of win_par$hotspot
    chr_pos <- shiny::reactive({
      make_chr_pos(win_par$chr_id, 
                   win_par$peak_Mbp, 
                   win_par$window_Mbp)
    })
    
    ## Probs object for allele pair diplotypes.
    pairprobs_obj <- pairProbsServer("pairprobs", win_par, project_df)
    
    snp_action <- shiny::reactive({input$snp_action})
    
    ## SNP Association
    patterns <- snpSetupServer("snp_setup", input, win_par, phe_mx, cov_df, K_chr,
                              analyses_df, project_df, allele_info, snp_action)
    
    patternServer("dip_pat", input, chr_pos, win_par, phe_mx, cov_df, pairprobs_obj, K_chr,
                 analyses_df, patterns, project_df, allele_info, snp_action)
    
    output$allele_names <- shiny::renderText({
      shiny::req(allele_info())
      paste(allele_info()$code, allele_info()$shortname, sep = "=", collapse = ", ")
    })
    
    output$dip_input <- shiny::renderUI({
      switch(shiny::req(input$button),
             "Genome Scans"    = patternUI(ns("dip_pat")),
             "SNP Association" =,
             "Allele Pattern"  = snpSetupInput(ns("snp_setup")))
    })
    output$dip_output <- shiny::renderUI({
      switch(shiny::req(input$button),
             "Genome Scans"    = patternOutput(ns("dip_pat")),
             "SNP Association" = ,
             "Allele Pattern"  = snpSetupOutput(ns("snp_setup")))
    })
    output$button_input <- shiny::renderUI({
      shiny::radioButtons(ns("button"), "",
                          c("SNP Association","Allele Pattern","Genome Scans"),
                          input$button)
    })
    output$snp_action_input <- shiny::renderUI({
      shiny::selectInput(ns("snp_action"), "",
                         c("add+dom","additive","non-add",
                           "recessive","dominant"),
                         input$snp_action)
    })
    output$sex_type_input <- shiny::renderUI({
      choices <- c("A","I","F","M","all")
      if(ncol(shiny::req(phe_mx())) > 1 | shiny::req(input$button) == "Genome Scans") {
        choices <- choices[1:4]
      }
      shiny::radioButtons(ns("sex_type"), "Sex:",
                          choices,
                          input$sex_type, inline = TRUE)
    })
    output$project <- shiny::renderUI({
      shiny::strong(shiny::req(paste("Project:",
                                     project_df()$project,
                                     "\n")))
    })
  })
}
#' @export
#' @rdname diploServer
diploUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarPanel(
      shiny::uiOutput(ns("project")),
      shiny::strong("SNP/Gene Action"),
      shiny::uiOutput(ns("button_input")),
      shiny::uiOutput(ns("snp_action_input")),
      shiny::uiOutput(ns("sex_type_input")),
      shiny::uiOutput(ns("dip_input")),
      shiny::textOutput(ns("allele_names"))),
  shiny::mainPanel(
    shiny::uiOutput(ns("dip_output")))
  )
}

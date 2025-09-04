#' Shiny Pattern App
#'
#' Shiny module for SNP pattern plots, with interfaces \code{patternUI} and  \code{patternOutput}.
#'
#' @param id identifier for shiny reactive
#' @param hotspot_list,dip_par,pairprobs_obj,patterns,snp_action,project_df reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' 
#' @importFrom qtl2pattern sdp_to_pattern
#' @importFrom dplyr filter
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny checkboxInput column downloadButton downloadHandler
#'             fluidRow moduleServer NS observeEvent plotOutput radioButtons
#'             reactive renderPlot renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom grDevices dev.off pdf
#' @importFrom utils write.csv
#' @importFrom rlang .data
patternApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Pattern",
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
      title = "Pattern",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          dipParInput("dip_par"),
          dipParUI("dip_par"),
          snpSetupInput("snp_setup"),
          patternUI("dip_pat")),      
        bslib::card(patternOutput("dip_pat"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    dip_par <- diploServer("dip_par")
    pairprobs_obj <-
      pairProbsServer("pairprobs", hotspot_list$win_par, project_df)
    snp_action <- shiny::reactive({dip_par$snp_action})
    patterns <-
      snpSetupServer("snp_setup", hotspot_list, dip_par, project_df, snp_action)
    patternServer("dip_pat", hotspot_list, dip_par, pairprobs_obj, patterns,
                  snp_action, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname patternApp
patternServer <- function(id, hotspot_list, dip_par, pairprobs_obj, patterns,
                          snp_action = shiny::reactive({NULL}), project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    phe1_mx <- reactive({
      hotspot_list$pheno_mx()[, shiny::req(input$pheno_name), drop = FALSE]
    })
    #** analyses_df no longer used.
    alleleServer("allele", hotspot_list$win_par,  phe1_mx, hotspot_list$covar_df, pairprobs_obj, hotspot_list$kinship_list,
                analyses_df, patterns, scan_pat, project_df, snp_action)
    
    ## Select phenotype for plots.
    output$pheno_name_input <- shiny::renderUI({
      shiny::selectInput(ns("pheno_name"), NULL,
                         choices = colnames(shiny::req(hotspot_list$pheno_mx())),
                         selected = input$pheno_name)
    })
    ## Select pattern for plots.
    pats <- shiny::reactive({
      shiny::req(input$pheno_name, patterns())
      pull_patterns(patterns(), colnames(shiny::req(hotspot_list$pheno_mx())))
    })
    haplos <- reactive({
      shiny::req(hotspot_list$allele_info())$code
    })
    pattern_choices <- shiny::reactive({
      qtl2pattern::sdp_to_pattern(pats()$sdp, haplos())
    })
    output$pattern <- shiny::renderUI({
      shiny::req(pattern_choices(), snp_action())
      choices <- pattern_choices()
      if(!length(choices)) {
        choices <- input$pattern
      }
      shiny::selectInput(ns("pattern"), NULL,
                         choices = choices,
                         selected = input$pattern)
    })
    shiny::observeEvent(patterns(), update_patterns())
    shiny::observeEvent(input$pheno_name, update_patterns())
    update_patterns <- function() {
      shiny::req(snp_action(), input$pheno_name, patterns())
      pats <- dplyr::filter(patterns(), .data$pheno == input$pheno_name)
      if(nrow(pats)) {
        choices <- qtl2pattern::sdp_to_pattern(pats$sdp, haplos())
      } else {
        choices <- input$pattern
      }
      if(!is.null(selected <- input$pattern)) {
        if(!(selected %in% choices))
          selected <- choices[1]
      }
      shiny::updateSelectInput(session, "pattern", NULL,
                               choices, selected)
    }
    
    scan_pat <- shiny::reactive({
      req(snp_action())
      pheno_in <- shiny::req(input$pheno_name)
      shiny::req(hotspot_list$pheno_mx(), hotspot_list$covar_df(), pairprobs_obj(), hotspot_list$kinship_list(),
                 pats(), analyses_df(), dip_par$sex_type)
      withProgress(message = 'Scan Patterns ...', value = 0, {
        setProgress(1)
        scan1_pattern(pheno_in, hotspot_list$pheno_mx(), hotspot_list$covar_df(), 
                      pairprobs_obj(), hotspot_list$kinship_list(), analyses_df(),
                      pats(), dip_par$sex_type, input$blups)
      })
    })
    
    output$pattern_plot_lod <- shiny::renderPlot({
      if(is.null(scan_pat()))
        return(plot_null())
      shiny::req(scan_pat(), pattern_choices(), input$pheno_name, pairprobs_obj())
      withProgress(message = 'Pattern LODs ...', value = 0, {
        setProgress(1)
        scan_pat_type(scan_pat(), pairprobs_obj()$map, "lod", pattern_choices(), 
                      input$pheno_name, haplos())
      })
    })
    output$pattern_plot_coef <- shiny::renderPlot({
      if(is.null(scan_pat()))
        return(plot_null())
      shiny::req(scan_pat(), pattern_choices(), input$pheno_name, pairprobs_obj())
      withProgress(message = 'Pattern Effects ...', value = 0, {
        setProgress(1)
        scan_pat_type(scan_pat(), pairprobs_obj()$map, "coef", pattern_choices(), 
                      input$pheno_name, haplos())
      })
    })
    output$pattern_table <- DT::renderDataTable({
      shiny::req(scan_pat())
      withProgress(message = 'Pattern summary ...', value = 0, {
        setProgress(1)
        summary(scan_pat(), pairprobs_obj()$map)
      })
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
    
    output$pattern_plot_both <- shiny::renderPlot({
      if(is.null(scan_pat()))
        return(plot_null())
      shiny::req(scan_pat(), pattern_choices(), input$pheno_name, pairprobs_obj())
      withProgress(message = 'Pattern Effects & LOD ...', value = 0, {
        setProgress(1)
        scan_pat_type(scan_pat(), pairprobs_obj()$map, "coef_and_lod", pattern_choices(), 
                      input$pheno_name, haplos())
      })
    })
    
    output$pattern_choice <- shiny::renderUI({
      switch(shiny::req(input$button),
             LOD =,
             "LOD & Effects" =,
             Effects = shiny::uiOutput(ns("pattern")))
    })
    
    output$LOD <- shiny::renderUI({
      switch(shiny::req(input$button),
             LOD             = shiny::plotOutput(ns("pattern_plot_lod")))
    })
    output$Effects <- shiny::renderUI({
      switch(shiny::req(input$button),
             Effects         = shiny::plotOutput(ns("pattern_plot_coef")))
    })
    output$Both <- shiny::renderUI({
      switch(shiny::req(input$button),
             "LOD & Effects" = shiny::plotOutput(ns("pattern_plot_both")))
    })
    output$Means <- shiny::renderUI({
      switch(shiny::req(input$button),
             "Allele Means"  = alleleOutput(ns("allele")))
    })
    output$Summary <- shiny::renderUI({
      switch(shiny::req(input$button),
             Summary = DT::dataTableOutput(ns("pattern_table")))
    })
    
    ## Downloads
    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        shiny::req(hotspot_list$win_par())
        pheno_in <- shiny::req(input$pheno_name)
        file.path(paste0(
          paste(pheno_in, snp_action(), "effects",
                hotspot_list$win_par()$chr, hotspot_list$win_par()$pos,
                sep = "_"),
          ".csv"))
      },
      content = function(file) {
        scan_in <- shiny::req(scan_pat())
        shiny::req(pairprobs_obj())
        utils::write.csv(summary(scan_in, pairprobs_obj()$map), file)
      }
    )
    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        shiny::req(input$pheno_name)
        file.path(paste0(
          paste(pheno_in, snp_action(), "effects",
                hotspot_list$win_par()$chr, hotspot_list$win_par()$pos,
                sep = "_"),
          ".pdf"))
      },
      content = function(file) {
        shiny::req(scan_pat(), pattern_choices(), dip_par$sex_type)
        scan_in <- shiny::req(scan_pat())
        top_panel_prop <- 0.65
        grDevices::pdf(file, width = 9, height = 9)
        for(pheno_in in colnames(hotspot_list$pheno_mx())) {
          pats <- dplyr::filter(patterns(), .data$pheno == pheno_in)
          pat_choices <- qtl2pattern::sdp_to_pattern(pats$sdp, haplos())
          
          scan_now <- scan1_pattern(pheno_in, hotspot_list$pheno_mx(), hotspot_list$covar_df(), 
                                    pairprobs_obj(), hotspot_list$kinship_list(), analyses_df(),
                                    pats, dip_par$sex_type, input$blups)
          
          scan_pat_type(scan_now, pairprobs_obj()$map, "coef_and_lod", pat_choices,
                        pheno_in, haplos())
        }
        grDevices::dev.off()
      }
    )
    output$button_input <- shiny::renderUI({
      shiny::radioButtons(ns("button"), "",
                          c("LOD","Effects","LOD & Effects","Allele Means","Summary"),
                          input$button)
    })
    output$blups_input <- shiny::renderUI({
      shiny::checkboxInput(ns("blups"), "BLUPs?")
    })
    output$select <- shiny::renderUI({
      switch(shiny::req(input$button),
             "Allele Means"  = alleleUI(ns("allele")),
             uiOutput(ns("patterndown")))
      
    })
    output$patterndown <- shiny::renderUI({
      shiny::fluidRow(
        shiny::column(6, shiny::downloadButton(ns("downloadData"), "CSV")),
        shiny::column(6, shiny::downloadButton(ns("downloadPlot"), "Plots")))
    })
  })
}
#' @export
#' @rdname patternApp
patternUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("button_input"))),
      shiny::column(6, shiny::uiOutput(ns("blups_input")))),
    shiny::uiOutput(ns("pheno_name_input")),
    shiny::uiOutput(ns("select")))
}
#' @export
#' @rdname patternApp
patternOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("LOD")),
    shiny::uiOutput(ns("Effects")),
    shiny::uiOutput(ns("Both")),
    shiny::uiOutput(ns("Means")),
    shiny::uiOutput(ns("Summary"))
  )
}

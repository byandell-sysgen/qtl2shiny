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
#' @importFrom qtl2pattern scan1pattern sdp_to_pattern
#' @importFrom dplyr across filter mutate where
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shiny checkboxInput column
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
          bslib::card(
            dipParInput("dip_par"),       # sex_type
            dipParUI("dip_par")),         # button, snp_action
          bslib::card(
            snpSetupInput("snp_setup")),  # <various>
          bslib::card(
            patternInput("pattern_list"), # button, blups, pheno_name
            patternUI("pattern_list")),   # pattern
          width = 400),
        bslib::card(patternOutput("pattern_list"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    dip_par <- dipParServer("dip_par")
    snp_action <- shiny::reactive({dip_par$snp_action})
    patterns <-
      snpSetupServer("snp_setup", hotspot_list, dip_par, project_df, snp_action)
    pairprobs_obj <-
      pairProbsServer("pairprobs", hotspot_list$win_par, project_df)
    pattern_list <- patternServer("pattern_list", hotspot_list, dip_par,
      pairprobs_obj, patterns, snp_action, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname patternApp
patternServer <- function(id, hotspot_list, dip_par, pairprobs_obj, patterns,
                          snp_action = shiny::reactive({"basic"}), project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Inputs `pat_par`: pheno_name, button, blups, pattern
    output$pheno_name_input <- shiny::renderUI({
      shiny::selectInput(ns("pheno_name"), NULL,
                         choices = colnames(shiny::req(hotspot_list$pheno_mx())),
                         selected = input$pheno_name)
    })
    output$button_input <- shiny::renderUI({
      shiny::radioButtons(ns("button"), "",
                          c("LOD","Effects","LOD & Effects","Allele Means","Summary"),
                          input$button)
    })
    output$blups_input <- shiny::renderUI({
      shiny::checkboxInput(ns("blups"), "BLUPs?")
    })
    output$pattern_input <- shiny::renderUI({
      shiny::req(pattern_choices(), snp_action())
      choices <- pattern_choices()
      if(!length(choices)) {
        choices <- input$pattern
      }
      shiny::selectInput(ns("pattern"), NULL,
                         choices = choices,
                         selected = input$pattern)
    })
    
    ## Select pattern for plots.
    haplos <- reactive({
      shiny::req(hotspot_list$allele_info())$code
    })
    pattern_choices <- shiny::reactive({
      qtl2pattern::sdp_to_pattern(shiny::req(pats())$sdp, haplos())
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
    
    # Pattern scan
    pats <- shiny::reactive({
      pull_patterns(shiny::req(patterns()),
                    colnames(shiny::req(hotspot_list$pheno_mx())))
    })
    scan_pat <- shiny::reactive({
      shiny::req(snp_action())
      pheno_name <- shiny::req(input$pheno_name)
      shiny::req(hotspot_list$pheno_mx(), hotspot_list$covar_df(),
                 pairprobs_obj(), hotspot_list$kinship_list(),
                 hotspot_list$peak_df(), pats(), dip_par$sex_type)
      withProgress(message = 'Scan Patterns ...', value = 0, {
        setProgress(1)
        scan1_pattern(pheno_name, hotspot_list$pheno_mx(),
                      hotspot_list$covar_df(), pairprobs_obj(),
                      hotspot_list$kinship_list(), hotspot_list$peak_df(),
                      pats(), dip_par$sex_type, input$blups)
      })
    })
    output$pattern_table <- DT::renderDataTable({
      shiny::req(scan_pat(), pairprobs_obj())
      withProgress(message = 'Pattern summary ...', value = 0, {
        setProgress(1)
        dplyr::mutate(summary(scan_pat(), pairprobs_obj()$map),
          dplyr::across(dplyr::where(is.numeric), signif, digits = 4))
      })
    }, escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 5))
    
    # Returns `pattern_list`.
    shiny::reactiveValues(
      pat_par = input,
      haplos = haplos,
      pattern_choices = pattern_choices,
      scan_pat = scan_pat
    )
  })
}
#' @export
#' @rdname patternApp
patternInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("button_input"))), # button
      shiny::column(6, shiny::uiOutput(ns("blups_input")))), # blups
    shiny::uiOutput(ns("pheno_name_input")))                 # pheno_name
}
#' @export
#' @rdname patternApp
patternUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("pattern_input"))                       # pattern
}
#' @export
#' @rdname patternApp
patternOutput <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("pattern_table"))
}

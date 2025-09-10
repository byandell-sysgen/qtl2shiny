#' Shiny Triad App
#'
#' @param id identifier for shiny reactive
#' @param hotspot_list,snp_list,mediate_list,probs_obj reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom ggplot2 autoplot
#' @importFrom qtl2mediate mediation_triad_qtl2
#' @importFrom shiny isolate isTruthy
#'             moduleServer NS plotOutput reactive renderPlot renderUI req
#'             selectInput setProgress tagList uiOutput withProgress
#' @importFrom utils write.csv
#' @importFrom grDevices dev.off pdf
#
triadApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_navbar(
    title =  "Test Triad",
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
      title = "Triad",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          triadUI("triad"),                   # triad, med_name, triad_plot
          mediateInput("mediate_list"),       # qtls, pos_Mbp
          snpListInput("snp_list"),           # scan_window
          snpListInput2("snp_list"),          # minLOD
          snpListUI("snp_list")),             # pheno_name
        # ** Output not working yet.
        bslib::card(triadOutput("triad"))
      )
    )
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project", projects_df)
    hotspot_list <- hotspotPanelServer("hotspot_list", project_df)
    probs_obj <- probsServer("probs", hotspot_list$win_par, project_df)
    snp_list <- snpListServer("snp_list", hotspot_list, project_df)
    mediate_list <-
      mediateServer("mediate_list", hotspot_list, snp_list, probs_obj, project_df)
    triadServer("triad", hotspot_list, snp_list, mediate_list, probs_obj)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname triadApp
triadServer <- function(id, hotspot_list, snp_list, mediate_list, probs_obj) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
  
    win_par <- shiny::isolate(hotspot_list$win_par)
    chr_id <- shiny::reactive({
      shiny::req(win_par()$chr_id[1])
    })
    med_par <- shiny::isolate(mediate_list$med_par)
    patterns <- shiny::isolate(snp_list$patterns)
    
    sdp <- shiny::reactive({
      shiny::req(input$pattern)
      choices <- choices_pattern()
      shiny::req(mediate_list$sdps())[match(input$pattern, choices, nomatch = 1)]
    })
    output$pattern_input <- shiny::renderUI({
      shiny::selectInput(ns("pattern"), NULL, choices_pattern(), input$pattern)
    })
    choices_pattern <- shiny::reactive({
      shiny::req(mediate_list$sdps())
      haplos <- shiny::req(hotspot_list$allele_info())$code
      qtl2pattern::sdp_to_pattern(mediate_list$sdps(), haplos)
    })
    shiny::observeEvent(shiny::req(input$checkplot, choices_pattern()), {
      choices <- choices_pattern()
      selected <- input$pattern
      if(is.null(selected)) {
        selected <- choices[1]
      }
      if(!(selected %in% choices)) {
        selected <- choices[1]
      }
      shiny::updateSelectInput(session, "pattern",
                               choices = choices, selected = selected)
    })
    
    ## Select triad for plots.
    output$triad_input <- shiny::renderUI({
      triad <- shiny::req(mediate_list$mediate_obj())$best$triad
      choices <- levels(triad)
      choices <- choices[choices %in% unique(triad)]
      shiny::selectInput(ns("triad"), NULL,
                         choices = choices, input$triad)
    })
    ## Select mediator for plots.
    output$med_name_input <- shiny::renderUI({
      shiny::req(mediate_list$mediate_obj(), input$triad)
      choices <- dplyr::filter(mediate_list$mediate_obj()$best, .data$triad == input$triad)$id
      shiny::selectInput(ns("med_name"), NULL,
                         choices = choices, input$med_name)
    })
    
    triad_df <- reactive({
      shiny::req(mediate_list$phe1_mx(), mediate_list$med_ls(), hotspot_list$covar_df(), probs_obj(), chr_id(),
                 input$med_name, med_par$pos_Mbp, sdp())
      qtl2mediate::mediation_triad_qtl2(
        target = mediate_list$phe1_mx(),
        mediator = mediate_list$med_ls()[[1]][, input$med_name, drop = FALSE],
        annotation = mediate_list$med_ls()[[2]],
        covar_tar = hotspot_list$covar_df(),
        covar_med = mediate_list$med_ls()$covar,
        genoprobs = probs_obj()$probs,
        map = probs_obj()$map,
        chr = chr_id(),
        pos = med_par$pos_Mbp,
        kinship = hotspot_list$kinship_list()[[1]],
        sdp = sdp())
    })
    
    ## Select plot format.
    output$triad_plot_input <- shiny::renderUI({
      shiny::selectInput(ns("triad_plot"), NULL,
                         choices = c("by_mediator", 
                                     "by_target", 
                                     "driver_offset", 
                                     "driver"),
                         input$triad_plot)
    })
    
    ## Triad plot
    output$triad_plot_output <- shiny::renderPlot({
      if(!shiny::isTruthy(patterns())) {
        return(plot_null("first run\nAllele Patterns"))
      }
      if(!shiny::isTruthy(triad_df())) {
        plot_null("too much\nmissing data\nin mediators\nreduce window width")
      } else {
        shiny::req(input$triad_plot, input$med_name, triad_df(),
                   mediate_list$phe1_mx(), mediate_list$peak_mar())
        shiny::withProgress(message = 'Triad Plot ...', value = 0, {
          shiny::setProgress(1)
          p <- ggplot2::autoplot(triad_df(), type = input$triad_plot,
                                 dname = mediate_list$peak_mar(),
                                 mname = input$med_name,
                                 tname = colnames(mediate_list$phe1_mx()),
                                 fitlines = "sdp-parallel",
                                 centerline = NULL)
        })
        p
      }
    })
  })
}
#' @export
#' @rdname triadApp
triadUI <- function(id) { # med_name, triad_plot
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("triad_input")),      # triad
    shiny::uiOutput(ns("med_name_input")),   # med_name
    shiny::uiOutput(ns("pattern_input")),    # pattern
    shiny::uiOutput(ns("triad_plot_input"))) # triad_plot
}
#' @export
#' @rdname triadApp
triadOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("triad_plot_output")) # triad_plot
}

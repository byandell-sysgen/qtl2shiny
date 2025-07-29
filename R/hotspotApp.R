#' Shiny Hotspot App
#'
#' Shiny module to view hotspots for peak selection, with interfaces \code{hotspotInput} and  \code{hotspotOutput}.
#'
#' @param id identifier for shiny reactive
#' @param set_par,peak_df,pmap_obj,project_info reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @return list of inputs and scan summary
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom dplyr arrange desc distinct filter
#' @importFrom shiny checkboxInput column fluidRow isTruthy moduleServer
#'             numericInput observeEvent reactive renderPlot renderTable
#'             renderUI req selectInput setProgress strong tableOutput tagList
#'             uiOutput updateNumericInput updateSelectInput withProgress
#' @importFrom DT dataTableOutput renderDataTable 
#' @importFrom rlang .data
#' @importFrom bslib page_sidebar sidebar
hotspotApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Hotspot",
    sidebar = bslib::sidebar(
      projectUI("project"),
      shiny::uiOutput("pheno_group_input"),
      shiny::uiOutput("dataset_input"),
      hotspotInput("hotspot")),
    hotspotOutput("hotspot")
  )
  server <- function(input, output, session) {
    peak_df <- shiny::reactive({
      shiny::req(project_info())
      read_project(project_info(), "peaks")
    })
    pmap_obj <- shiny::reactive({
      shiny::req(project_info())
      read_project(project_info(), "pmap")
    })

    project_df <- projectServer("project", projects_df)
    scan_tbl <- hotspotServer("hotspot", input, peak_df, pmap_obj, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname hotspotApp
hotspotServer <- function(id, set_par, peak_df, pmap_obj, project_info) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observeEvent(project_info(), {
      choices <- chr_names()
      shiny::updateSelectInput(session, "chr_ct", shiny::strong("chrs"),
                               choices = c("all", choices),
                               selected = NULL)
      shiny::updateNumericInput(session, "window_Mbp", "width",
                                1, 0.1, 100)
      if(shiny::isTruthy(peak_df())) {
        value <- minLOD(NULL, peak_df())
        shiny::updateNumericInput(session, "minLOD", "min LOD", value, min = 0, step = 0.5)
      }
    })
    chr_names <- shiny::reactive({
      shiny::req(project_info())
      names(shiny::req(pmap_obj()))
    })
    # Hotspot Search (if desired--not used)
    output$hotspot_input <- shiny::renderUI({
      shiny::checkboxInput(ns("hotspot"), "Search Hotspots?", input$hotspot)
    })
    # Select chromosome.
    output$chr_ct_input <- shiny::renderUI({
      shiny::req(project_info())
      choices <- chr_names()
      if(is.null(selected <- input$chr_ct))
        selected <- "all"
      shiny::selectInput(ns("chr_ct"), strong("chrs"),
                         choices = c("all", choices),
                         selected = selected,
                         multiple = TRUE)
    })
    shiny::observeEvent(input$chr_ct, {
      is_all <- grep("all", input$chr_ct)
      if(length(is_all)) {
        if(length(input$chr_ct) > 1) {
          selected <- input$chr_ct[-is_all]
          choices <- chr_names()
          shiny::updateSelectInput(session, "chr_ct", strong("Chr"),
                                   choices = c("all", choices),
                                   selected = selected)
        }
      }
    })
    
    ## Window numeric
    output$window_Mbp_input <- shiny::renderUI({
      shiny::req(project_info())
      if(is.null(win <- input$window_Mbp))
        win <- 1
      shiny::numericInput(ns("window_Mbp"), "width",
                          win, 0.1, 100)
    })
    
    scan_obj_all <- shiny::reactive({
      shiny::req(project_info(), input$window_Mbp, input$minLOD)
      shiny::withProgress(message = 'Hotspot scan ...', value = 0, {
        shiny::setProgress(1)
        hotspot_wrap(pmap_obj(), peak_df(), input$window_Mbp, input$minLOD,
                     project_info())
        })
    })
    
    scan_obj <- shiny::reactive({
      out_peaks <- scan_obj_all()
      shiny::withProgress(message = 'Hotspot search ...', value = 0, {
        shiny::setProgress(1)
        chr_ct <- input$chr_ct
        if(!("all" %in% chr_ct)) {
          out_peaks <- subset(out_peaks, chr_ct)
          }
        })
      out_peaks
    })
    
    output$peak_show <- shiny::renderUI({
      if(input$peak_ck) {
        shiny::plotOutput(ns("peak_plot"))
      } else {
        DT::dataTableOutput(ns("peak_tbl"))
      }
    })
    # ** This is causing problems.**
    output$peak_plot <- shiny::renderPlot({
      shiny::req(scan_obj())
      window_Mbp <- shiny::req(input$window_Mbp)
      peak_grp <- set_par$pheno_group
      if(shiny::isTruthy(set_par$dataset)) {
        peak_set <- set_par$dataset
        dat_sets <- dplyr::distinct(peak_df(), 
                                    .data$pheno_type, .data$pheno_group)
        dat_groups <- unique(dplyr::filter(dat_sets,
          .data$pheno_type %in% peak_set)$pheno_group)
        peak_set <- c(peak_grp[!(peak_grp %in% dat_groups)], peak_set)
      } else {
        peak_set <- peak_grp
      }
      
      shiny::withProgress(message = 'Hotspot show ...',
                          value = 0, {
                            shiny::setProgress(1)
                            plot_hot(peak_set, scan_obj(), window_Mbp)
                          })
    })
    scan_tbl <- shiny::reactive({
      shiny::req(scan_obj())
      if(shiny::isTruthy(set_par$dataset)) {
        peak_set <- set_par$dataset
      } else {
        peak_set <- set_par$pheno_group
      }
      shiny::withProgress(message = 'Hotspot summary ...', value = 0, {
        shiny::setProgress(1)
        summary_hot(peak_set, scan_obj())
      })
    })
    output$peak_tbl <- DT::renderDataTable({
      shiny::req(scan_tbl(), peak_df())
      peakDataTable(scan_tbl(), peak_df())
    }, escape = FALSE,
    options = list(lengthMenu = c(5,10,20,50), pageLength = 5))
    output$peak_table <- DT::renderDataTable({
      shiny::req(scan_tbl())
      dplyr::arrange(scan_tbl(), desc(.data$count))
    }, escape = FALSE,
    options = list(lengthMenu = c(5,10,20,50), pageLength = 5))
    
    # Minimum LOD for SNP top values.
    minLOD <- function(value, peak_df) {
      if(shiny::isTruthy(value)) {
        value
      } else {
        max(5.5, round(min(peak_df$lod), 1))
      }
    }
    output$minLOD_input <- shiny::renderUI({
      shiny::req(peak_df())
      value <- minLOD(input$minLOD, peak_df())
      shiny::numericInput(ns("minLOD"), "min LOD", value, min = 0, step = 0.5)
    })
    
    ## Return.
    scan_tbl
  })
}
#' @export
#' @rdname hotspotApp
hotspotInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::strong("Hotspot Info")),
      shiny::column(6, shiny::checkboxInput(ns("peak_ck"), "plot?", FALSE))),
    shiny::fluidRow(
      shiny::column(4, shiny::uiOutput(ns("chr_ct_input"))),
      shiny::column(4, shiny::uiOutput(ns("minLOD_input"))),
      shiny::column(4, shiny::uiOutput(ns("window_Mbp_input")))))
}
#' @export
#' @rdname hotspotApp
hotspotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::strong("Hotspot Info"),
    shiny::uiOutput(ns("peak_show")),
    DT::dataTableOutput(ns("peak_table"))
  )
}

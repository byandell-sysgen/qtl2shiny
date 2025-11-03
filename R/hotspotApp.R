#' Shiny Hotspot Data App
#'
#' Shiny module to select hotspots for peak selection.
#'
#' @param id identifier for shiny reactive
#' @param set_par,peak_df,pmap_obj,project_df reactive arguments
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
#' @importFrom shiny column fluidRow isTruthy moduleServer
#'             numericInput observeEvent reactive renderPlot renderTable
#'             renderUI req selectInput setProgress tableOutput
#'             uiOutput updateNumericInput updateSelectInput withProgress
#' @importFrom DT dataTableOutput renderDataTable 
#' @importFrom rlang .data
#' @importFrom bslib page_sidebar sidebar
hotspotDataApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- bslib::page_sidebar(
    title =  "Test Hotspot",
    sidebar = bslib::sidebar(
      projectUI("project_df"),      # project
      setParInput("set_par"),       # class, subject_model 
      setParUI("set_par"),          # window_Mbp 
      hotspotDataInput("hotspot_obj")), # chr_ct, minLOD
    hotspotDataOutput("hotspot_obj")    # hotspot_obj
  )
  server <- function(input, output, session) {
    project_df <- projectServer("project_df", projects_df)
    set_par <- setParServer("set_par", project_df)
    peak_df <- peakServer("peak_df", set_par, project_df)
    pmap_obj <- shiny::reactive(read_project(project_df(), "pmap"))
    hotspot_obj <- 
      hotspotDataServer("hotspot_obj", set_par, peak_df, pmap_obj, project_df)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname hotspotDataApp
hotspotDataServer <- function(id, set_par, peak_df, pmap_obj, project_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Select chromosome.
    chr_names <- shiny::reactive({
      shiny::req(project_df())
      names(shiny::req(pmap_obj()))
    })
    output$chr_ct_input <- shiny::renderUI({
      shiny::req(project_df())
      choices <- chr_names()
      if(is.null(selected <- input$chr_ct))
        selected <- "all"
      shiny::selectInput(ns("chr_ct"), "chrs",
                         choices = c("all", choices),
                         selected = selected,
                         multiple = TRUE)
    })
    shiny::observeEvent(project_df(), {
      choices <- chr_names()
      shiny::updateSelectInput(session, "chr_ct", "chrs",
                               choices = c("all", choices),
                               selected = NULL)
      if(shiny::isTruthy(peak_df())) {
        value <- minLOD(NULL, peak_df())
        shiny::updateNumericInput(session, "minLOD", "min LOD", value, min = 0, step = 0.5)
      }
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

    # Minimum LOD for SNP top values.
    minLOD <- function(value, peak_df) {
      if(shiny::isTruthy(value)) {
        value
      } else {
        max(5.5, round(min(peak_df$qtl_lod), 1))
      }
    }
    output$minLOD_input <- shiny::renderUI({
      shiny::req(peak_df())
      value <- minLOD(input$minLOD, peak_df())
      shiny::numericInput(ns("minLOD"), "min LOD", value, min = 0, step = 0.5)
    })
    
    hotspot_obj <- shiny::reactive({
      shiny::req(pmap_obj(), peak_df(), project_df(),
                 set_par$window_Mbp, input$minLOD)
      chrs <- input$chr_ct
      if(!shiny::isTruthy(chrs) || "all" %in% chrs) chrs <- NULL
      shiny::withProgress(message = 'Hotspot scan ...', value = 0, {
        shiny::setProgress(1)
        hotspot(pmap_obj(), peak_df(), set_par$window_Mbp, input$minLOD, chrs)
      })
    })
    output$hotspot_str <- shiny::renderUI({
      hotspot_list <- shiny::req(hotspot_obj())
      shiny::tagList(
        shiny::renderText(paste("hotspot_obj: ",
                                paste(names(hotspot_list), collapse = ", "))),
        shiny::renderText(paste("scan: ",
                                paste(dim(hotspot_list$scan), collapse = ", "))),
        shiny::renderText(paste("map: ",
                                paste(names(hotspot_list$map), collapse = ", ")))
      )
    })
    
    ## Return.
    hotspot_obj
  })
}
#' @export
#' @rdname hotspotDataApp
hotspotDataInput <- function(id) {                             # chr_ct, minLOD 
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(6, shiny::uiOutput(ns("chr_ct_input"))), # chr_ct
    shiny::column(6, shiny::uiOutput(ns("minLOD_input")))) # minLOD
}
#' @export
#' @rdname hotspotDataApp
hotspotDataOutput <- function(id) { 
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("hotspot_str"))
}

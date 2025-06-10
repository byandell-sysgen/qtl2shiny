#' Shiny peaks module
#'
#' Shiny module for peaks selection, with interfaces \code{peaksInput}, \code{peaksUI} and  \code{peaksOutput}.
#'
#' @param id identifier for shiny reactive
#' @param set_par,pheno_type,peaks_tbl,pmap_obj,project_info reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @return list of inputs and scan summary
#'
#' @export
#' 
#' @importFrom dplyr filter
#' @importFrom shiny checkboxInput column fluidRow observeEvent moduleServer NS
#'             numericInput renderUI req selectInput strong tagList textInput
#'             uiOutput updateNumericInput updateSelectInput
#' @importFrom rlang .data
peaksServer <- function(id, set_par, pheno_type, peaks_tbl, pmap_obj, 
                       project_info) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observeEvent(project_info(), {
      choices <- names(pmap_obj())
      shiny::updateSelectInput(session, "chr_id", shiny::strong("chr"),
                               choices = choices,
                               selected = NULL)
      shiny::updateNumericInput(session, "window_Mbp", "width",
                                1, 0.1, 100)
    })
    
    # Select chromosome. Defaults to blank.
    output$chr_id_input <- shiny::renderUI({
      shiny::req(project_info())
      choices <- names(pmap_obj())
      selected <- input$chr_id
      if(!isTruthy(selected))
        choices <- c("", choices)
      shiny::selectInput(ns("chr_id"), shiny::strong("chr"),
                         choices = choices,
                         selected = selected)
    })
    
    ## Window numeric
    output$window_Mbp_input <- shiny::renderUI({
      shiny::req(project_info())
      if(is.null(win <- input$window_Mbp))
        win <- 1
      shiny::numericInput(ns("window_Mbp"), "width",
                          win, 0.1, 100)
    })
    
    # Peak position slider.
    output$peak_Mbp_input <- shiny::renderUI({
      shiny::req(project_info(), pmap_obj())
      chr_id <- shiny::req(input$chr_id)
      rng <- round(range(pmap_obj()[[chr_id]]), 2)
      pos <- input$peak_Mbp
      if(is.null(pos)) {
        pos <- round(mean(rng), 2)
      } else {
        if(!is.na(pos) && (pos < rng[1] | pos > rng[2]))
          pos <- round(mean(rng), 2)
      }
      shiny::numericInput(ns("peak_Mbp"), "pos", 
                          pos, rng[1], rng[2])
    })
    
    ## shorthand 
    output$chr_pos <- shiny::renderUI({
      shiny::req(project_info())
      shiny::textInput(ns("chr_pos"), "pos", input$chr_pos)
    })
    
    scan_tbl <- hotspotServer("hotspot", set_par, pheno_type, peaks_tbl, pmap_obj, project_info)
    
    shiny::observeEvent(scan_tbl(), {
      update_chr()
      update_peak()
    })
    shiny::observeEvent(input$chr_id, update_peak())
    update_chr <- function() {
      scan_in <- shiny::req(scan_tbl())
      choices <- scan_in$chr[scan_in$count > 0]
      scan_in <- dplyr::filter(scan_in, .data$count == max(.data$count))
      
      chr_ct <- as.character(scan_in$chr)
      if(!any(chr_ct == input$chr_id)) {
        chr_ct <- chr_ct[1]
        shiny::updateSelectInput(session, "chr_id", shiny::strong("chr"),
                                 choices, chr_ct)
      }
    }
    update_peak <- function() {
      scan_in <- shiny::req(scan_tbl())
      chr_ct <- shiny::req(input$chr_id)
      scan_in <- dplyr::filter(scan_in, 
                               .data$chr == chr_ct)
      if(nrow(scan_in)) {
        scan_in <- dplyr::filter(scan_in, .data$count == max(.data$count))
        peak_Mbp <- scan_in$pos[1]
      } else {
        peak_Mbp <- input$peak_Mbp
      }
      pmap <- shiny::req(pmap_obj())
      rng <- round(range(pmap[[chr_ct]]), 2)
      shiny::updateNumericInput(session, "peak_Mbp",
                                value=peak_Mbp, 
                                min=rng[1], max=rng[2])
    }
    shiny::observeEvent(input$chr_pos, {
      #    chr_pos <- strsplit(input$chr_pos, ":|@|_| |,")[[1]]
      #    if(length(chr_pos) == 2) {
      #      chr <- chr_pos[1]
      #      pmap <- pmap_obj()
      #      choices <- names(pmap)
      #      if(chr %in% choices) {
      #        pos <- as.numeric(chr_pos[2])
      chr <- shiny::req(input$chr_id)
      pos <- as.numeric(input$chr_pos)
      if(is.numeric(pos)) {
        if(!is.na(pos)) {
          pmap <- shiny::req(pmap_obj())
          rng <- round(range(pmap[[chr]]), 2)
          if(pos >= rng[1] & pos <= rng[2]) {
            up <- is.null(input$chr_id)
            if(!up) {
              up <- (chr != input$chr_id)
            }
            if(up) {
              shiny::updateSelectInput(session, "chr_id",
                                       selected=chr, choices=choices)
            }
            shiny::updateNumericInput(session, "peak_Mbp",
                                      value=pos, min=rng[1], max=rng[2])
          }
        }
      }
    })
    
    ## Return.
    input
  })
}
#' @export
#' @rdname peaksServer
peaksInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::checkboxInput(ns("local"), "Local Scan in Window?", TRUE),
    shiny::fluidRow(
      shiny::column(4, shiny::uiOutput(ns("chr_id_input"))),
      shiny::column(4, shiny::uiOutput(ns("peak_Mbp_input"))),
      shiny::column(4, shiny::uiOutput(ns("window_Mbp_input")))
    ))
}
#' @export
#' @rdname peaksServer
peaksUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    hotspotInput(ns("hotspot")))
}
#' @export
#' @rdname peaksServer
peaksOutput <- function(id) {
  ns <- shiny::NS(id)
  hotspotOutput(ns("hotspot"))
}

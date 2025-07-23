#' Download App
#'
#' @param id identifier for shiny reactive
#' @param download_list reactiveValues object
#' @param selected_item index to selected plot or table
#' @param plot_table either "Plots" or "Tables"
#'
#' @importFrom shiny br downloadButton downloadHandler h5 moduleServer NS
#'             observeEvent radioButtons reactive reactiveValues renderPlot
#'             renderUI req selectInput shinyApp textAreaInput uiOutput
#'             updateRadioButtons
#' @importFrom utils write.csv    
#' @importFrom grDevices dev.off pdf png
#' @importFrom bslib card card_header layout_columns page_sidebar sidebar
#' @export
downloadApp <- function(selected_item = 1, plot_table = "Plots") {
  ui <- bslib::page_sidebar(
    title = "Test Download",
    sidebar = bslib::sidebar("side_panel", width = 400,
      shiny::selectInput("plot_table", "", c("Plots","Tables")),
      shiny::uiOutput("selected_item"),
      downloadOutput("download") # downloadButton, filename
    ),
    bslib::card(
      downloadInput("download"), # inputs for Plots or Tables
      downloadUI("download")     # width and height for plot
    ),
    bslib::card(
      bslib::card_header("Download Preview"),
      downloadPreview("download")    # Only for Preview of downloadApp().
    )
  )
  server <- function(input, output, session) { 
    # Test sets
    prefix <- "Download"
    download_list <- shiny::reactiveValues(
      filename = "panelID_instanceID",
      no_download = "Plots_skip",
      Plots = shiny::reactiveValues(
        skip = shiny::reactive(plot_null("skip")),
        none = shiny::reactive(plot_null("nothing")),
        some = shiny::reactive(plot_null("something"))),
      Tables = shiny::reactiveValues(
        twelve = shiny::reactive(matrix(1:12,nrow=3)),
        twenty = shiny::reactive(matrix(1:20,nrow=4)))
    )
    output$selected_item <- shiny::renderUI({
      plot_table <- shiny::req(input$plot_table)
      choices <- names(download_list[[plot_table]])
      if(shiny::isTruthy(download_list$no_download)) {
        choices <- choices[
          !(paste0(plot_table, "_", choices) %in% download_list$no_download)]
      }
      shiny::selectInput("selected_item", "", choices)
    })
    downloadServer("download", download_list,
      selected_item = shiny::reactive(input$selected_item),
      plot_table = shiny::reactive(input$plot_table))
  }
  shiny::shinyApp(ui, server)
}
#' @rdname downloadApp
#' @export
downloadServer <- function(id, download_list,
                           selected_item = shiny::reactive(1),
                           plot_table = shiny::reactive("Plots")) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    plot_width_rv <- shiny::reactiveVal(1200)
    plot_height_rv <- shiny::reactiveVal(600)
    use_alternating_colors_rv <- shiny::reactiveVal(TRUE)
    clicked_plotly_point_details_rv <- shiny::reactiveVal(NULL)
    
    shiny::observeEvent(input$plot_width, { plot_width_rv(input$plot_width) }, ignoreNULL = TRUE)
    shiny::observeEvent(input$plot_height, { plot_height_rv(input$plot_height) }, ignoreNULL = TRUE)
    
    shiny::observeEvent(input$preset_1to1, {
      shiny::updateNumericInput(session, "plot_width", value = 800)
      shiny::updateNumericInput(session, "plot_height", value = 800)
    })
    shiny::observeEvent(input$preset_3to2, {
      shiny::updateNumericInput(session, "plot_width", value = 900)
      shiny::updateNumericInput(session, "plot_height", value = 600)
    })
    shiny::observeEvent(input$preset_16to9, {
      shiny::updateNumericInput(session, "plot_width", value = 1280)
      shiny::updateNumericInput(session, "plot_height", value = 720)
    })
    
    shiny::observeEvent(input$color_toggle, {
      if(is.logical(input$color_toggle)){
        use_alternating_colors_rv(input$color_toggle)
      } else {
        use_alternating_colors_rv(!use_alternating_colors_rv())
      }
    })
    
    # Preview download app.
    output$preview <- shiny::renderUI({
      list(
        "filename",
        download_list$filename,
        shiny::br(),
        "no download",
        paste(download_list$no_download, collapse = ", "),
        shiny::br(),
        "Plots",
        paste(names(download_list$Plots), collapse = ", "),
        shiny::br(),
        "Tables", 
        paste(names(download_list$Tables), collapse = ", "),
        switch(shiny::req(plot_table()),
          Plots  = shiny::uiOutput(ns("preview_plots")),
          Tables = DT::dataTableOutput(ns("preview_table")))
      )
    })
    output$preview_plots <- shiny::renderUI({
      width <- shiny::reactive(plot_width_rv() / 2)
      height <- shiny::reactive(plot_height_rv() / 2)
      shiny::renderPlot({
        shiny::req(selected_download())
      }, width = width, height = height)
    })
    output$preview_table <- DT::renderDataTable({
      if(shiny::req(plot_table()) == "Tables")
        shiny::req(selected_download())
    })
    
    # Select plot or table from `download_list`.
    choices_download <- function(type, selected) {
      # Choices excluding any in `no_download` vector.
      choices = names(download_list[[type]])
      if(shiny::isTruthy(download_list$no_download)) {
        choices <- choices[
          !(paste0(type, "_", choices) %in% download_list$no_download)]
      }
      choices[grep(selected, choices)]
    }
    selected_download <- shiny::reactive({
      type <- shiny::req(plot_table())
      choice <- choices_download(type, shiny::req(selected_item()))
      message(type, " ", choice)
      if(length(choice))
        shiny::req(download_list[[type]][[choice]]())
    })

    ## Switch betwwen Plots or Tables.
    output$buttons <- shiny::renderUI({
      shiny::uiOutput(ns(paste0("choices_", shiny::req(plot_table()))))
    })
    output$dims <- shiny::renderUI({
      switch(shiny::req(plot_table()),
             Plots = shiny::uiOutput(ns("dims_Plots"))
      )
    })
    
    ## Plot buttons.
    output$choices_Plots <- shiny::renderUI({
      # Use supplied `create_` functions or standard `shiny`.
      if (!exists("create_button", mode = "function")) {
        create_button <- shiny::actionButton
      }
      if (!exists("create_download_button", mode = "function")) {
        create_download_button <- shiny::downloadButton
        button_class <- "btn-sm"
      } else {
        button_class <- "btn-sm btn-light"
      }
      # Row for plot title, download buttons, and preset buttons
      shiny::div(
        style = paste("display: flex; justify-content: space-between;",
                      "align-items: center; margin-bottom: 10px;",
                      "flex-wrap: wrap;"),
        shiny::h4(
          "LOD Score Plot",
          style = "margin: 0 15px 0 0; color: #2c3e50; font-weight: 600;"),
        shiny::div(
          style = paste("display: flex; align-items: center; gap: 10px;",
                        "flex-grow: 1; justify-content: flex-end;"),
          # Preset Aspect Ratio Buttons
          shiny::div(
            style = "display: flex; gap: 5px; margin-right: 15px;",
            shiny::tagList(
              create_button(ns("preset_1to1"), "1:1", class = button_class),
              create_button(ns("preset_3to2"), "3:2", class = button_class),
              create_button(ns("preset_16to9"), "16:9", class = button_class)
            )
          ),
          # Download Buttons
          shiny::tagList(
            create_download_button(ns("download_plot_png"), "PNG",
                                   class = "btn-sm"),
            create_download_button(ns("download_plot_pdf"), "PDF",
                                   class = "btn-sm")
          )
        )
      )
    })
    output$dims_Plots <- shiny::renderUI({
      if (!exists("create_numeric_input", mode = "function")) {
        create_numeric_input <- shiny::numericInput
      }
      if (!exists("create_lever_switch", mode = "function")) {
        create_lever_switch <- shiny::checkboxInput
      }
      # Row for plot dimension controls and color toggle
      shiny::div(
        style = "display: flex; gap: 10px; align-items: center; margin-bottom: 5px; flex-wrap: wrap;",
        shiny::div(
          style = "display: flex; align-items: center; gap: 10px;",
          create_numeric_input(ns("plot_width"), "Width:",
                               value = 1200, min = 400, max = 2000, step = 50, width = "100px"),
          create_numeric_input(ns("plot_height"), "Height:",
                               value = 600, min = 300, max = 1200, step = 50, width = "100px")
        ),
        create_lever_switch(ns("color_toggle"), "Alt Colors", value = TRUE)
      )
    })
    
    # Download filename.
    base_filename <- shiny::reactive({
      paste0(shiny::req(download_list$filename),
             "_", format(Sys.time(), "%Y%m%d"))
    })
    # Optional UI to edit filename
    output$filename <- renderUI({
      filename <- shiny::req(base_filename())
      shiny::textAreaInput(ns("filename"), "File Basename:", filename)
    })
    download_filename <- function(mime = "png") {
      function() {
        filename <- shiny::req(base_filename())
        if(shiny::isTruthy(input$filename))
          filename <- input$filename
        paste0(filename, ".", mime)
      }
    }
    
    # Download handlers for plot
    output$download_plot_png <- shiny::downloadHandler(
      filename = download_filename("png"),
      content = function(file) {
        selected_plot <- shiny::req(selected_download())
        # Use dynamic width/height for saving
        ggplot2::ggsave(file, plot = selected_plot, 
          width = plot_width_rv()/96, # Assuming 96 DPI for conversion from px
          height = plot_height_rv()/96, 
          dpi = 300, units = "in")
      }
    )
    output$download_plot_pdf <- shiny::downloadHandler(
      filename = download_filename("pdf"),
      content = function(file) {
        selected_plot <- shiny::req(selected_download())
        ggplot2::ggsave(file, plot = selected_plot, 
          width = plot_width_rv()/96, 
          height = plot_height_rv()/96, 
          device = cairo_pdf, units = "in") # Use cairo_pdf for better PDF quality
      }
    )
    
    # Download handlers for table
    output$choices_Tables <- shiny::renderUI({
      shiny::downloadButton(ns("Tables"), "Table", class = "btn-sm")
    })
    output$Tables <- shiny::downloadHandler(
      filename = download_filename("csv"),
      content = function(file) {
        table <- shiny::req(selected_download())
        utils::write.csv(table, file, row.names = FALSE)
      }
    )
  })
}
#' @rdname downloadApp
#' @export
downloadInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("buttons"))
}
#' @rdname downloadApp
#' @export
downloadUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("dims"))
}
#' @rdname downloadApp
#' @export
downloadOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("filename"))
}
downloadPreview <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("preview"))
}
  
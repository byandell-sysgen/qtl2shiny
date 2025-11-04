#' Download App
#'
#' @param id identifier for shiny reactive
#' @param download_list reactiveValues object
#'
#' @importFrom shiny actionButton br checkboxInput div downloadButton
#'             downloadHandler h4 isTruthy moduleServer NS numericInput
#'             observeEvent reactive reactiveVal reactiveValues renderPlot
#'             renderUI req selectInput shinyApp tagList textAreaInput textInput
#'             uiOutput updateNumericInput
#' @importFrom bslib card card_header page_sidebar sidebar
#' @importFrom utils write.csv    
#' @importFrom ggplot2 ggsave
#' @export
downloadApp <- function() {
  ui <- bslib::page_sidebar(
    title = "Test Download",
    sidebar = bslib::sidebar("side_panel", width = 400,
      # Set up arguments for `downloadServer`.
      shiny::uiOutput("selected_plot"),
      shiny::uiOutput("selected_table")
    ),
    downloadInput("download"), # inputs for Plot or Table
    downloadUI("download"),     # width and height for plot
    bslib::card(
      bslib::card_header("Download Preview"),
      downloadPreview("download")    # Only for Preview of downloadApp().
    )
  )
  server <- function(input, output, session) { 
    # Test Plots
    download_Plot <- shiny::reactiveValues(
      none = shiny::reactive(plot_null("none")),
      some = shiny::reactive(plot_null("some")))
    output$selected_plot <- shiny::renderUI({
      choices <- names(download_Plot)
      shiny::selectInput("selected_plot", "", choices)
    })
    selected_plot <- shiny::reactive({
      download_Plot[[shiny::req(input$selected_plot)]]()
    })
    # Test Tables
    download_Table <- shiny::reactiveValues(
      twelve = shiny::reactive(matrix(1:12,nrow=3)),
      twenty = shiny::reactive(matrix(1:20,nrow=4)))
    output$selected_table <- shiny::renderUI({
      choices <- names(download_Table)
      shiny::selectInput("selected_table", "", choices)
    })
    selected_table <- shiny::reactive({
      download_Table[[shiny::req(input$selected_table)]]()
    })
    download_Filename <- shiny::reactive({
      # Trick to get input from `downloadServer`.
      switch(input$`download-plot_table`,
        Table = input$selected_table,
        Plot = input$selected_plot)
    })

    download_list <- shiny::reactiveValues(
      Filename = download_Filename,
      Plot = selected_plot,
      Table = selected_table)
    
    downloadServer("download", download_list, addDate = TRUE)
  }
  shiny::shinyApp(ui, server)
}
#' @rdname downloadApp
#' @export
downloadServer <- function(id, download_list, addDate = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Plot `width` and `height`.
    plot_width_rv <- shiny::reactiveVal(1200)
    plot_height_rv <- shiny::reactiveVal(600)
    shiny::observeEvent(input$plot_width, {
      plot_width_rv(input$plot_width) },
      ignoreNULL = TRUE)
    shiny::observeEvent(input$plot_height, {
      plot_height_rv(input$plot_height) },
      ignoreNULL = TRUE)
    
    # Plot dimension presets
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
    
    # Preview download app.
    output$preview <- shiny::renderUI({
      list(
        "Filename",
        download_list$Filename(),
        shiny::br(),
        switch(shiny::req(input$plot_table),
          Plot  = shiny::uiOutput(ns("preview_plots")),
          Table = DT::dataTableOutput(ns("preview_table")))
      )
    })
    output$preview_plots <- shiny::renderUI({
      width <- shiny::reactive(plot_width_rv() / 2)
      height <- shiny::reactive(plot_height_rv() / 2)
      shiny::renderPlot({
        shiny::req(download_list$Plot())
      }, width = width, height = height)
    })
    output$preview_table <- DT::renderDataTable({
      shiny::req(download_list$Table())
    })

    ## Switch between `Plot` or `Table`.
    output$buttons <- shiny::renderUI({
      shiny::uiOutput(ns(paste0("choices_", shiny::req(input$plot_table))))
    })
    output$dims <- shiny::renderUI({
      switch(shiny::req(input$plot_table),
             Plot = shiny::uiOutput(ns("dims_Plot"))
      )
    })
    
    ## Plot buttons.
    output$choices_Plot <- shiny::renderUI({
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
    output$dims_Plot <- shiny::renderUI({
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
        )
      )
    })
    
    # Download Filename.
    base_filename <- shiny::reactive({
      filename <- shiny::req(download_list$Filename())
      if(addDate) {
        filename <- paste0(filename, "_", format(Sys.time(), "%Y%m%d"))
      }
      filename
    })
    # Optional UI to edit filename
    output$filename <- renderUI({
      filename <- shiny::req(base_filename())
      shiny::textInput(ns("filename"), "", filename)
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
        selected_plot <- download_list$Plot()
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
        selected_plot <- download_list$Plot()
        ggplot2::ggsave(file, plot = selected_plot, 
          width = plot_width_rv()/96, 
          height = plot_height_rv()/96, 
          device = cairo_pdf, units = "in") # Use cairo_pdf for better PDF quality
      }
    )
    
    # Download handlers for table
    output$choices_Table <- shiny::renderUI({
      # Row for plot title, download buttons, and preset buttons
      shiny::div(
        style = paste("display: flex; justify-content: space-between;",
                      "align-items: center; margin-bottom: 10px;",
                      "flex-wrap: wrap;"),
        shiny::downloadButton(ns("Table"), "CSV", class = "btn-sm")
      )
    })
    output$Table <- shiny::downloadHandler(
      filename = download_filename("csv"),
      content = function(file) {
        table <- shiny::req(download_list$Table())
        utils::write.csv(table, file, row.names = FALSE)
      }
    )
  })
}
#' @rdname downloadApp
#' @export
downloadInput <- function(id) {
  ns <- shiny::NS(id)
  # ** Would like these to be aligned vertically better. **
  bslib::layout_columns(
    col_widths = c(2, 4, 6),
    shiny::selectInput(ns("plot_table"), "", c("Plot","Table")),
    shiny::uiOutput(ns("buttons")),
    shiny::uiOutput(ns("filename")))
}
#' @rdname downloadApp
#' @export
downloadUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("dims"))
}
downloadPreview <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("preview"))
}
  
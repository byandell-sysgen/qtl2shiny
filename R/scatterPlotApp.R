#' Shiny Scatter Plot App
#'
#' @param id identifier for shiny reactive
#' @param plot_df reactive data frame containing columns: x, y, and optionally sex, diet, geno
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @return No return value; called for side effects.
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth theme_minimal scale_color_brewer scale_color_hue facet_wrap
#' @importFrom plotly renderPlotly plotlyOutput ggplotly
#' @importFrom shiny checkboxInput fluidRow column isolate isTruthy moduleServer NS observeEvent plotOutput radioButtons reactive renderPlot renderUI req selectInput sliderInput strong tagList uiOutput updateSelectInput updateSliderInput withProgress
#' @importFrom bslib card layout_sidebar page_sidebar sidebar
scatterPlotApp <- function() {
  # Mock data frame creation
  set.seed(42)
  n <- 200
  mock_df <- data.frame(
    subject = paste0("ind_", 1:n),
    x = rnorm(n),
    sex = sample(c("Female", "Male"), n, replace = TRUE),
    diet = sample(c("Chow", "HF"), n, replace = TRUE),
    geno = sample(c("AA", "AB", "BB"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  # y is a function of x, sex, diet, and genotype
  mock_df$y <- 2 * mock_df$x + 
    ifelse(mock_df$sex == "Male", 1.5, 0) + 
    ifelse(mock_df$diet == "HF", -1, 0) + 
    ifelse(mock_df$geno == "AB", 0.5, ifelse(mock_df$geno == "BB", 1, 0)) + 
    rnorm(n, sd = 0.8)
  
  rownames(mock_df) <- mock_df$subject

  ui <- bslib::page_sidebar(
    title = "Test Generic Scatter Plot",
    sidebar = bslib::sidebar(
      scatterPlotInput("scatter")
    ),
    bslib::card(scatterPlotOutput("scatter"))
  )

  server <- function(input, output, session) {
    scatterPlotServer("scatter", shiny::reactive(mock_df))
  }

  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname scatterPlotApp
scatterPlotServer <- function(id, plot_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render dynamic UI choices based on columns of plot_df
    output$scatter_inputs <- shiny::renderUI({
      dat <- shiny::req(plot_df())
      cols <- colnames(dat)
      
      # Exclude x, y, and subject for color/shape/facet variables
      candidate_vars <- cols[!(cols %in% c("x", "y", "subject"))]
      
      # If sex and diet are present, sex_diet is a candidate
      if(all(c("sex", "diet") %in% cols)) {
        candidate_vars <- c(candidate_vars, "sex_diet")
      }
      
      # Clean choices
      color_choices <- c("None" = "none")
      for(var in candidate_vars) {
        color_choices[var] <- var
      }
      
      shape_choices <- c("None" = "none")
      for(var in candidate_vars[candidate_vars != "sex_diet"]) {
        shape_choices[var] <- var
      }
      
      facet_choices <- c("None" = "none")
      for(var in candidate_vars) {
        facet_choices[var] <- var
      }
      
      shiny::tagList(
        shiny::radioButtons(ns("static"), "Plot Type:", c("Static", "Interactive"), "Static"),
        shiny::selectInput(ns("color_by"), "Color by:", choices = color_choices, selected = "none"),
        shiny::selectInput(ns("shape_by"), "Shape by:", choices = shape_choices, selected = "none"),
        shiny::selectInput(ns("facet_by"), "Facet by:", choices = facet_choices, selected = "none"),
        shiny::checkboxInput(ns("add_line"), "Add regression line?", TRUE),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == true && input['%s'] != 'none'", ns("add_line"), ns("color_by")),
          shiny::checkboxInput(ns("group_line"), "Separate line per group?", FALSE)
        ),
        shiny::sliderInput(ns("point_size"), "Point Size:", min = 1, max = 10, value = 3, step = 0.5),
        shiny::sliderInput(ns("point_alpha"), "Transparency (Alpha):", min = 0.1, max = 1, value = 0.7, step = 0.1)
      )
    })

    # Render static ggplot
    output$plot_static <- shiny::renderPlot({
      print(shiny::req(plot_obj()))
    })

    # Render interactive plotly
    output$plot_interactive <- plotly::renderPlotly({
      p <- shiny::req(plot_obj())
      plotly::ggplotly(p)
    })

    # Renders the UI wrapper that switches static/interactive
    output$plot_ui <- shiny::renderUI({
      static_val <- ifelse(is.null(input$static), "Static", input$static)
      switch(static_val,
             Static = shiny::plotOutput(ns("plot_static")),
             Interactive = plotly::plotlyOutput(ns("plot_interactive")))
    })

    # Construct the plot object
    plot_obj <- shiny::reactive({
      dat <- shiny::req(plot_df())
      if(!all(c("x", "y") %in% colnames(dat))) {
        return(plot_null("dataframe must contain x and y columns"))
      }

      # Convert candidates to factors if present
      for(col in c("sex", "diet", "geno")) {
        if(col %in% colnames(dat)) {
          dat[[col]] <- factor(dat[[col]])
        }
      }

      # Calculate sex_diet combination
      if(all(c("sex", "diet") %in% colnames(dat))) {
        dat$sex_diet <- factor(paste(dat$sex, dat$diet, sep = "_"))
      }

      # Ensure subject/rownames exists for label
      if(!("subject" %in% colnames(dat))) {
        dat$subject <- rownames(dat)
      }

      # Set up ggplot mapping
      aes_args <- list(x = quote(x), y = quote(y), label = quote(subject))

      # Color grouping
      col_var <- input$color_by
      if(!is.null(col_var) && col_var != "none" && col_var %in% colnames(dat)) {
        aes_args$col <- as.name(col_var)
        aes_args$group <- as.name(col_var)
      }

      # Shape grouping
      shape_var <- input$shape_by
      if(!is.null(shape_var) && shape_var != "none" && shape_var %in% colnames(dat)) {
        aes_args$shape <- as.name(shape_var)
      }

      p <- ggplot2::ggplot(dat) + do.call(ggplot2::aes, aes_args)

      p_size <- ifelse(is.null(input$point_size), 3, input$point_size)
      p_alpha <- ifelse(is.null(input$point_alpha), 0.7, input$point_alpha)

      # Add scatter points
      p <- p + ggplot2::geom_point(size = p_size, alpha = p_alpha)

      # Add regression lines
      if(shiny::isTruthy(input$add_line)) {
        if(shiny::isTruthy(input$group_line) && !is.null(col_var) && col_var != "none" && col_var %in% colnames(dat)) {
          p <- p + ggplot2::geom_smooth(ggplot2::aes(label = NULL), method = "lm", formula = y ~ x, se = FALSE, linewidth = 1)
        } else {
          p <- p + ggplot2::geom_smooth(ggplot2::aes(group = 1, label = NULL), method = "lm", formula = y ~ x, se = FALSE, col = "black", linetype = "dashed", linewidth = 1)
        }
      }

      # Faceting
      facet_var <- input$facet_by
      if(!is.null(facet_var) && facet_var != "none" && facet_var %in% colnames(dat)) {
        p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var)))
      }

      # Apply theme and colors
      p <- p + ggplot2::theme_minimal()
      if (!is.null(col_var) && col_var != "none" && col_var %in% colnames(dat)) {
        num_levels <- length(unique(dat[[col_var]]))
        if (num_levels <= 8) {
          p <- p + ggplot2::scale_color_brewer(palette = "Dark2")
        } else {
          p <- p + ggplot2::scale_color_hue()
        }
      }

      p
    })

    # Return the ggplot reactive
    plot_obj
  })
}
#' @export
#' @rdname scatterPlotApp
scatterPlotInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("scatter_inputs"))
}
#' @export
#' @rdname scatterPlotApp
scatterPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("plot_ui"))
}

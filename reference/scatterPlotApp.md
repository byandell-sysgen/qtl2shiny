# Shiny Scatter Plot App

Shiny Scatter Plot App

## Usage

``` r
scatterPlotApp()

scatterPlotServer(
  id,
  plot_df,
  x_label = shiny::reactive("x"),
  y_label = shiny::reactive("y")
)

scatterPlotInput(id)

scatterPlotOutput(id)
```

## Arguments

- id:

  identifier for shiny reactive

- plot_df:

  reactive data frame containing columns: x, y, and optionally sex,
  diet, geno

## Value

No return value; called for side effects.

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

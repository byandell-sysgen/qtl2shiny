# Generic Scatter Plot Shiny Module

This document outlines the design prompt, implementation walkthrough, and recent bug fixes/enhancements for the generic scatter plot Shiny module ([R/scatterPlotApp.R](/R/scatterPlotApp.R)) and its integration in [R/scatterApp.R](/R/scatterApp.R).

- [/R/scatterPlotApp.R](/R/scatterPlotApp.R)
- [/R/scatterApp.R](/R/scatterApp.R)

## Table of Contents

- [Initial Prompt / Requirements](#1-initial-prompt--requirements)
- [Walkthrough & Implementation Details](#2-walkthrough--implementation-details)
- [Creating R/scatterApp.R as a Combined App](#3-creating-rscatterappr-as-a-combined-app)
- [Global Layout and Theme Customization](#4-global-layout-and-theme-customization)
- [Walkthrough of Bug Fixes and Enhancements](#5-walkthrough-of-bug-fixes-and-enhancements)

---

## 1. Initial Prompt / Requirements

Create a generic and reusable Shiny module for displaying scatter plots of numeric data. It should support:
- **Reactive Data Input**: Consume a reactive data frame containing columns `x` and `y` (and optionally metadata like `sex`, `diet`, `geno`, `subject`, etc.).
- **Dynamic Grouping & Faceting Options**:
  - Automatically identify candidate variables for coloring, shapes, and faceting based on the columns present in the data frame (excluding `x`, `y`, and `subject`).
  - Automatically construct a combined `sex_diet` grouping variable if both `sex` and `diet` columns are present.
- **Static vs. Interactive Toggling**: Let the user choose between a static `ggplot` rendering and an interactive `plotly` chart (via `ggplotly`).
- **Regression Lines**: Solid regression lines plotted behind symbols (separate by color when color is selected).
- **Visual Fine-Tuning**: Sliders to control point size and transparency (alpha).
- **Aesthetic Color Palettes & Shapes**: Use professional color palettes (e.g., `Dark2` for 8 or fewer levels, standard hue for more) and hollow/open symbols (like open circle, triangle, diamond, square) to prevent overlaps and keep points readable when paired with transparency (alpha).

---

## 2. Walkthrough & Implementation Details

The module consists of the following components:
1. **Main App Launcher**: `scatterPlotApp()` — A standalone function generating a mock dataset to test and display the module in action.
2. **UI Inputs**: `scatterPlotInput()` — Generates a sidebar panel with controls for plot types, coloring, shapes, faceting, lines, point size, and transparency.
3. **UI Outputs**: `scatterPlotOutput()` — A dynamic wrapper UI that switches between `plotOutput()` and `plotlyOutput()` depending on selection.
4. **Server Module**: `scatterPlotServer()` — Main reactive backend logic.

### Dynamic Input Generation
The color, shape, and facet options are dynamically rendered via `shiny::renderUI()` inside `scatterPlotServer()` to adapt to the column names of the supplied reactive dataset:

```r
candidate_vars <- cols[!(cols %in% c("x", "y", "subject"))]
if(all(c("sex", "diet") %in% cols)) {
  candidate_vars <- c(candidate_vars, "sex_diet")
}
```

### Static & Interactive Plotting
The server constructs a single `ggplot` object reactively based on the parameter settings:
- Solid regression lines (`geom_smooth`) are added first (underneath points), drawing separate lines by color when a color variable is selected.
- Points are mapped via `ggplot2::geom_point(shape = 1, stroke = 1.5, size = point_size, alpha = point_alpha)` by default to display hollow/open circles with a bold border, and map to dynamic hollow shape codes (`1`, `2`, `5`, `0`, `6`, `3`, `4`...) when shape variables are selected.
- If interactive mode is chosen, the reactive `ggplot` object is wrapped via `plotly::ggplotly(p)`.

---

## 3. Creating `R/scatterApp.R` as a Combined App

**Prompt:**
Using `R/genoApp.R` as a model, build `R/scatterApp.R` that gathers phenotype data via `R/hotspotApp.R` and genotype data via `R/genoDataApp.R`, and uses `R/scatterPlotApp.R` to plot the data.

**Implementation:**
Following [R/genoApp.R](file:///Users/brianyandell/Documents/Research/byandell-sysgen/qtl2shiny/R/genoApp.R) as a model, we built [R/scatterApp.R](file:///Users/brianyandell/Documents/Research/byandell-sysgen/qtl2shiny/R/scatterApp.R) to orchestrate data collection from `hotspot_list` (phenotype data) and `geno_list` (genotype data) and route it to the generic `scatterPlotServer()` module.

### How it Works
1. **Instantiation**:
   - Gathers diplotype/allele pair genotype probabilities at the position `pos_Mbp` using `genoDataServer()`.
   - Offers dropdown selections (`x_var` and `y_var`) containing the selected phenotype names.
2. **Merging Logic**:
   - The reactive data frame `scatter_df` dynamically retrieves values for `x` and `y` from the selected phenotype columns.
   - Extracts and factorizes the **scan pattern** columns from the genotype table (columns after the first one `"allele_pair"`).
   - Only merges these scan patterns, `sex`, and `diet` columns into the tidy data frame, filtering out other covariates.
3. **Plotting**:
   - Passes the merged reactive data frame to `scatterPlotServer()`, along with reactives for `x_label` and `y_label` containing the selected phenotype names (`input$x_var` and `input$y_var`).
   - `scatterPlotServer()` applies these custom labels to the plot axes using `ggplot2::labs()`.
   - Returns a `download_list` reactive values object containing custom downloads for the resulting plots and dataset.

---

## 4. Global Layout and Theme Customization

To ensure that the dashboard maximizes display size and is easy to read, we introduced layout and theme customizations:

1. **Collapsible Right-Hand Sidepanel**:
   - Instead of placing the scatter plot configuration options in a card above the plot, `R/scatterApp.R` nests them in a right-hand sidebar panel (`bslib::sidebar(position = "right")`). 
   - This leaves a much larger, unobstructed area in the center for the plot.
2. **Smaller Font Sizes Globally**:
   - We introduced global theme helpers inside `R/theme.R` (`qtl2shiny_theme()` and `qtl2shiny_plot_theme()`).
   - `qtl2shiny_theme()` configures a standard `bslib::bs_theme` with smaller font sizes (`font-size-base = "0.85rem"`).
   - `qtl2shiny_plot_theme()` defines a standard smaller font size (`base_size = 9`) for ggplot2 figures.
   - Applied the smaller font sizes to `qtl2shinyUI()`, `scatterApp()`, `genoApp()`, and `mediatePlotApp()`.

---

## 5. Walkthrough of Bug Fixes and Enhancements

We implemented several key bug fixes and enhancements during this feature's development cycle:

1. **Resolved selectInput Flickering (in `patternDataApp.R`)**:
   - *Problem*: Changing phenotype selections caused `patternDataUI`'s dropdown to flicker and reset. This was because its `renderUI` block depended on `pattern_choices()`, destroying and recreating the HTML dropdown element in the DOM on every choices change.
   - *Fix*: Simplified `output$pattern_input` to render the container select element exactly once. All dynamic choice updates are now handled smoothly on the client via `updateSelectInput()` inside the `update_patterns()` observer.
2. **Resolved ggplot2 dropped label aesthetic warnings (in `scatterPlotApp.R` and `geno_ggplot.R`)**:
   - *Problem*: `ggplot2` issued warnings that the `label` aesthetic was ignored or dropped during statistical transformations inside `geom_smooth()` layers.
   - *Fix*: Kept `label` globally defined for `plotly` tooltips but explicitly mapped `label = NULL` inside the `aes()` definitions of all `geom_smooth()` layers. This suppresses the warnings cleanly.
3. **Genotype Effects Plot and Summary Table Fix (in `genoEffectApp.R`)**:
   - *Problem*: Sidebar simplification refactoring broke the genotype effects page. The `pheno_name` input was removed from `patternDataInput`, leaving references to `pattern_list$pat_par$pheno_name` evaluating to `NULL`.
   - *Fix*: Exposed `pheno_name` as a reactive expression in the returned list from `patternDataServer()` and updated downstream plot/table consumers to invoke it as `pattern_list$pheno_name()`.
4. **Phenotype Names Initialization Fix (in `phenoNamesApp.R`)**:
   - *Problem*: Removed redundant `isTruthy()` check guards on empty phenotype input parameters that were blocking initial select choice lists from loading when datasets initialized.
5. **Scan Patterns for Grouping Selection**:
   - *Problem*: The first column `"allele_pair"` (diplotype categories) was used as a single `"Genotype"` group choice.
   - *Fix*: Replaced `"Genotype"` with the scan patterns columns (columns 2 and onwards of the genotype table) to allow users to select from one or more specific scan patterns for coloring, shape, and faceting.
6. **Facet Wrap Colons Fix (in `scatterPlotApp.R`)**:
   - *Problem*: Faceting by a scan pattern containing a colon `":"` (e.g. `"A:B"`) failed because `stats::as.formula(paste("~", facet_var))` parsed the colon as an R interaction operator rather than a literal column name.
   - *Fix*: Replaced formula construction with `ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]))`, which safely treats the pattern name as a literal column reference.
7. **Open Symbols & Bolder Outlines**:
   - *Enhancement*: Configured points to use open/hollow symbols by default (shape 1, open circle) and mapped discrete shape variables to hollow shape codes (`1`, `2`, `5`, `0`, `6`, `3`, `4`...). Configured `stroke = 1.5` inside `geom_point()` to make the symbol borders bolder and more legible.
8. **Regression Line Plotting Order**:
   - *Enhancement*: Swapped plotting order to draw solid regression lines *before* (behind) the scatter points. This keeps the symbols clean and visible. Removed the redundant `"Separate line per group?"` checkbox, always drawing separate lines by color when a color variable is selected.

---

## 6. Quickstart Code Example

To run the standalone test application locally, call:

```r
library(qtl2shiny)
scatterApp()
```

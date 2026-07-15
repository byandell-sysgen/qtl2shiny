# Generic Scatter Plot Shiny Module

This document outlines the design prompt and implementation walkthrough for the generic scatter plot Shiny module, [R/scatterPlotApp.R](/R/scatterPlotApp.R), and its integration as a combined application
in [R/scatterApp.R](/R/scatterApp.R).

- [/R/scatterPlotApp.R](/R/scatterPlotApp.R)
- [/R/scatterApp.R](/R/scatterApp.R)

## Table of Contents

- [Initial Prompt / Requirements](#1-initial-prompt--requirements)
- [Walkthrough & Implementation Details](#2-walkthrough--implementation-details)
- [Creating R/scatterApp.R as a Combined App](#3-creating-rscatterappr-as-a-combined-app)

---

## 1. Initial Prompt / Requirements

Create a generic and reusable Shiny module for displaying scatter plots of numeric data. It should support:

- **Reactive Data Input**: Consume a reactive data frame containing columns `x` and `y` (and optionally metadata like `sex`, `diet`, `geno`, `subject`, etc.).
- **Dynamic Grouping & Faceting Options**:
  - Automatically identify candidate variables for coloring, shapes, and faceting based on the columns present in the data frame (excluding `x`, `y`, and `subject`).
  - Automatically construct a combined `sex_diet` grouping variable if both `sex` and `diet` columns are present.
- **Static vs. Interactive Toggling**: Let the user choose between a static `ggplot` rendering and an interactive `plotly` chart (via `ggplotly`).
- **Regression Lines**: Option to add a regression line, either globally or separately for each color group.
- **Visual Fine-Tuning**: Sliders to control point size and transparency (alpha).
- **Aesthetic Color Palettes**: Use professional color palettes (e.g., `Dark2` for 8 or fewer levels, standard hue for more).

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

- Points are mapped via `ggplot2::geom_point(size = point_size, alpha = point_alpha)`.
- If a regression line is enabled, it adds `ggplot2::geom_smooth(method = "lm")`.
- If interactive mode is chosen, the reactive `ggplot` object is wrapped via `plotly::ggplotly(p)`.

---

## 3. Creating `R/scatterApp.R` as a Combined App

**Prompt:**
Using `R/genoApp.R` as a model, build `R/scatterApp.R`
that gathers phenotype data via `R/hotspotApp.R` and
genotype data via `R/genoDataApp.R`, and uses `R/scatterPlotApp.R` to plot the data.

**Implementation:**
Following [R/genoApp.R](file:///Users/brianyandell/Documents/Research/byandell-sysgen/qtl2shiny/R/genoApp.R) as a model, we built [R/scatterApp.R](file:///Users/brianyandell/Documents/Research/byandell-sysgen/qtl2shiny/R/scatterApp.R) to orchestrate data collection from `hotspot_list` (phenotype data) and `geno_list` (genotype data) and route it to the generic `scatterPlotServer()` module.

### How it Works

1. **Instantiation**:
   - Gathers diplotype/allele pair genotype probabilities at the position `pos_Mbp` using `genoDataServer()`.
   - Offers dropdown selections (`x_var` and `y_var`) containing the selected phenotype names and a `"Genotype"` option.
2. **Merging Logic**:
   - The reactive data frame `scatter_df` dynamically retrieves values for `x` and `y` depending on the dropdown choices (mapping `"Genotype"` to the first column of the genotype table, or mapping to selected phenotype columns).
   - Combines coordinate fields (`x`, `y`), covariate factors (`sex`, `diet`), and the genotype groupings (`geno`) into a single tidy data frame using `dplyr::left_join`.
3. **Plotting**:
   - Passes the merged reactive data frame to `scatterPlotServer()`.
   - Returns a `download_list` reactive values object containing custom downloads for the resulting plots and dataset.

---

## 4. Quickstart Code Example

To run the standalone test application locally, call:

```r
library(qtl2shiny)
scatterPlotApp()
scatterApp()
```

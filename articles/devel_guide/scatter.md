# Scatter Plot Module

*[Developer’s Guide to the `qtl2shiny`
Package](https://byandell-sysgen.github.io/qtl2shiny/articles/devel_guide/index.md)*

## Overview

The **Scatter Plot** panel allows researchers to interactively explore
and visualize correlations between numeric variables (phenotypes) across
study subjects. To aid in dissecting genetic architecture, the module
enables coloring, shaping, and faceting points by Strain Distribution
Patterns (SDPs), genotype probability patterns, sex, or diet covariates.

It coordinates two primary Shiny modules:

1.  **`scatterApp`**: The top-level panel coordinator that loads founder
    probabilities, queries variants, aligns phenotype columns with
    ancestral patterns/covariates, and packages the download interface.
2.  **`scatterPlotApp`**: The reusable generic plotting submodule that
    configures plot aesthetics, draws regression lines, handles
    interactive plotly/ggplotly toggling, and displays the final
    graphic.

------------------------------------------------------------------------

### Module Hierarchy & Entrypoints

- **Top-Level Container**:
  - Standalone Application:
    [`scatterApp()`](https://byandell-sysgen.github.io/qtl2shiny/reference/scatterApp.md)
  - Server Module:
    `scatterServer(id, hotspot_list, pattern_list, snp_list, pairprobs_obj, project_df)`
  - UI Input: `scatterInput(id)`
  - UI Output: `scatterOutput(id)`
- **Sub-Modules**:
  - **Generic Scatter Plot (`scatterPlotApp`)**: Standard plotting
    engine. Server: `scatterPlotServer(id, plot_df, x_label, y_label)`.
    UI Input: `scatterPlotInput(id)`. UI Output:
    `scatterPlotOutput(id)`.

------------------------------------------------------------------------

## 1. Top-Level Container (`scatterApp`)

The `scatterApp` module coordinates data collection, input routing, and
download packaging for the scatter panel.

### Server Logic & Reactive Flow (`scatterServer`)

1.  **Probabilities & Genotypes**:
    - Calls `genoDataServer("geno_list", ...)` to extract diplotype
      probabilities at the active marker coordinate `pos_Mbp`.
    - Obtains the genotype table, extracting ancestral alleles and
      Strain Distribution Patterns (SDPs).
2.  **Tidy Merging**:
    - Gathers selected phenotype values (from X and Y variable dropdown
      inputs) via `hotspot_list$pheno_mx()`.
    - Factorizes scan patterns and filters out unmapped covariates,
      keeping only the selected patterns, `sex`, and `diet` for
      aesthetic options.
    - Combines coordinates, patterns, and covariates into a single tidy
      reactive data frame (`scatter_df`).
3.  **Axis Labeling**:
    - Passes the raw phenotype names `shiny::reactive(input$x_var)` and
      `shiny::reactive(input$y_var)` to
      [`scatterPlotServer()`](https://byandell-sysgen.github.io/qtl2shiny/reference/scatterPlotApp.md)
      to dynamically apply custom, user-friendly axis labels.

------------------------------------------------------------------------

## 2. Reusable Scatter Plot Module (`scatterPlotApp`)

The `scatterPlotApp` module is a generic, reusable visualization engine
that plots coordinate pairs from any reactive data frame containing
columns `x` and `y`.

### Plot Customization & Aesthetics

- **Hollow/Open Shapes**:
  - To prevent overplotting and maintain point visibility when multiple
    samples overlap, points default to shape `1` (open/hollow circle).
  - Mapped discrete shapes (e.g. `shape_by`) utilize hollow shape codes
    (`1`, `2`, `5`, `0`, `6`…) to keep categories distinct.
- **Bolder Outlines**:
  - Configures `stroke = 1.5` on all point geometries, widening the
    circle and symbol outlines to remain highly legible when combined
    with transparency (alpha).
- **Regression Lines**:
  - Automatically draws solid regression lines (`linetype = "solid"`)
    underneath scatter symbols, preventing line overlays from obscuring
    point centers.
  - Automatically groups and draws separate regression lines when a
    coloring aesthetic (`color_by`) is selected.
- **Robust Faceting**:
  - Since strain distribution pattern names can contain colons `":"`
    (e.g., `"A:B"`), faceting avoids formula parsing syntax (which
    interprets colons as interaction variables). It instead uses the
    tidyverse data pronoun reference:
    `facet_wrap(ggplot2::vars(.data[[facet_var]]))`

# Create a generic scatterplot shiny app

## Prompt

Create a generic scatterplot shiny app using the style of `*App.R` modules in `./R`. It should have input of a data frame that incluces x, y, sex, diet and SNP genotype. The plot should be like the plot in `R/mediatePlotApp.R`. The genotypes should come from `R/genoDataApp.R`. In fact, the goal is to
have this new shiny module be used in a refactore `mediatePlotApp.R`.

## Result

I have successfully completed the tasks to create a generic scatter plot Shiny module and integrate it into the refactored `mediatePlotApp.R` file.

Here is a summary of the achievements:

1. **Created a Generic Scatter Plot Module**:
   - Implemented `scatterPlotApp.R` which includes UI inputs (`scatterPlotInput`), plot output container (`scatterPlotOutput`), and a server module (`scatterPlotServer`) that handles plotting of a reactive data frame with `x` and `y` columns, plus optional `sex`, `diet`, and `geno` columns.
   - The plot supports dynamic aesthetics mapping (Color, Shape, Facet) based on available columns, handles overall or per-group regression lines, and dynamically renders either static `ggplot2` or interactive `plotly` charts.
   - Includes a standalone `scatterPlotApp()` launcher for simple testing with mock data.

2. **Refactored `mediatePlotApp.R`**:
   - Integrated the new scatter plot module into `mediatePlotServer` under a new `"Scatter Plot"` format choice.
   - Instantiated diplotype allele pair queries using `pairProbsServer` and generated the genotype table dynamically at the mediation position `pos_Mbp`.
   - Fixed critical bugs: resolved the undefined `mediate_obj` reference to correctly query `mediate_list$mediate_obj()`, and fixed the `med_type` namespace bug to use `mediate_list$med_par$med_type`.
   - Routed the active plot to the return value of `mediatePlotServer` so that downloads (implemented in parent containers like `mediateApp.R`) automatically receive whichever plot is currently visible.

3. **Verified and Documented**:
   - Verified that both standalone `scatterPlotApp()` and integrated `mediatePlotApp()` return valid Shiny app objects.
   - Re-generated documentation and NAMESPACE entries using `devtools::document()`.
   - Recorded progress and details in the task tracker [task.md](file:///Users/brianyandell/.gemini/antigravity-ide/brain/fdc63a13-d0a4-4e0b-8649-bf752ad67457/task.md) and the walkthrough document [walkthrough.md](file:///Users/brianyandell/.gemini/antigravity-ide/brain/fdc63a13-d0a4-4e0b-8649-bf752ad67457/walkthrough.md).

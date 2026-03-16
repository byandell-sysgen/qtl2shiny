# Sidebar Simplification Walkthrough

The sidebar for `scanApp` and related modules has been simplified. The `pheno_name` selection and `scan_window` slider are now unified, appearing only once in the sidebar (via `snpListInput`) and reactive across all modules.

## Initial Prompt

The sidebar for scanApp is unduly complicated. It should be simplified to just the following:

- only one slider to be used in scanDataApp.R and snpGeneApp.R
- want to have only one selection of `pheno_name`
  - see output$pheno_choice used in scanDataInput() via scanInput()
  - see output$pheno_name_input used in snpListInput()
- want only one slider for `scan_window`
  - see output$scan_window_input used in scanDataInput() via scanInput()
  - see output$scan_window_input used in snpListInput()

The position of `pheno_name` and `scan_window` will need to be adjusted for several files.

## Changes Made

### Unified Inputs

- **Master Inputs**: `snpListInput` now serves as the primary source for `pheno_name` and `scan_window`.
- **Removed Redundancy**: Redundant inputs were removed from:
  - `scanDataInput`
  - `snpGeneInput` (via `snpListInput` coordination)
  - `mediateDataInput`
  - `patternDataInput`

### Server Updates

- **Reactive Values**: All respective server modules (`scanDataServer`, `snpGeneServer`, `mediateDataServer`, `patternDataServer`) now consume these values from the `snp_list` object.
- **Fallbacks**: Fallback logic ensures that if `snp_list` is not provided (e.g., in standalone module tests), the internal defaults or local calculations still work.

### UI Improvements

- **Layout**: The order of inputs in `snpListInput` was adjusted for better flow:
  1. Phenotype Selection
  2. Scan Window Slider
  3. LOD Threshold

## Verified Files

The following files were updated and verified (syntax and logical flow):

- [scanDataApp.R](file:///Users/brianyandell/Documents/Research/byandell-sysgen/qtl2shiny/R/scanDataApp.R)
- [scanApp.R](file:///Users/brianyandell/Documents/Research/byandell-sysgen/qtl2shiny/R/scanApp.R)
- [snpGeneApp.R](file:///Users/brianyandell/Documents/Research/byandell-sysgen/qtl2shiny/R/snpGeneApp.R)
- [snpListApp.R](file:///Users/brianyandell/Documents/Research/byandell-sysgen/qtl2shiny/R/snpListApp.R)
- [mediateDataApp.R](file:///Users/brianyandell/Documents/Research/byandell-sysgen/qtl2shiny/R/mediateDataApp.R)
- [patternDataApp.R](file:///Users/brianyandell/Documents/Research/byandell-sysgen/qtl2shiny/R/patternDataApp.R)

## Visual Verification (Manual Steps)

1. Run `scanApp()` or `qtl2shinyApp()`.
2. Observe the sidebar: it should show "SNP phenotype" and "Scan Window" only once.
3. Changing the phenotype should update plots in both "Genome Scans" and "SNP Association" (and "Patterns"/"Mediation" if applicable).
4. Adjusting the "Scan Window" should similarly affect all relevant plots.

# Shiny Phenotype Plot App

Shiny module to plot phenotypes.

## Usage

``` r
phenoPlotApp()

phenoPlotServer(id, pheno_mx, covar_df)

phenoPlotOutput(id)
```

## Arguments

- id:

  identifier for shiny reactive

- pheno_mx, covar_df:

  reactive arguments

## Value

2-element vector of scan window

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

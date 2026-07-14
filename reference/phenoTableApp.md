# Shiny Phenotype Table App

Shiny module to plot phenotypes.

## Usage

``` r
phenoTableApp()

phenoTableServer(id, pheno_mx, covar_df)

phenoTableOutput(id)
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

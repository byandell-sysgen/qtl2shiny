# Shiny Phenotype Names App

Shiny module for phenotype name selection.

## Usage

``` r
phenoNamesApp()

phenoNamesServer(id, set_par, peak_df, pheno_mx, covar_df, project_df)

phenoNamesInput(id)

phenoNamesOutput(id)
```

## Arguments

- id:

  identifier for shiny reactive

- set_par, win_par, peak_df, analyses_df, covar, project_df:

  reactive arguments

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

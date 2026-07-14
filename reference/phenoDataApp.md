# Shiny Phenotype Data App

Shiny module to filter phenotypes by selected names and transform using
\`rankZ\`.

## Usage

``` r
phenoDataApp()

phenoDataServer(id, pheno_names, pheno_mx, covar_df)

phenoDataInput(id)

phenoDataOutput(id)
```

## Arguments

- id:

  identifier for shiny reactive

- pheno_names, pheno_mx, covar_df:

  reactive arguments

## Value

2-element vector of scan window

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

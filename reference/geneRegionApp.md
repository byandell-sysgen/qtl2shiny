# Shiny Genes in SNP Region module

Shiny module for scan1 analysis and plots, with interfaces
`geneRegionInput`, `geneRegionUI` and `geneRegionOutput`.

## Usage

``` r
geneRegionApp()

geneRegionServer(id, snp_list, project_df)

geneRegionInput(id)

geneRegionOutput(id)
```

## Arguments

- id:

  identifier for shiny reactive

- snp_list, project_df:

  reactive arguments

## Value

No return value; called for side effects.

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

# Shiny Genes and Exons with nearby SNPs module

Shiny module for scan1 analysis and plots, with interfaces
`geneExonInput`, `geneExonUI` and `geneExonOutput`.

## Usage

``` r
geneExonApp()

geneExonServer(id, snp_list)

geneExonInput(id)

geneExonOutput(id)
```

## Arguments

- id:

  identifier for shiny reactive

- snp_list:

  reactive arguments

## Value

No return value; called for side effects.

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

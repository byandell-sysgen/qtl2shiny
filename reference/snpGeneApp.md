# Shiny SNP Gene App

Shiny module for SNP association mapping, with interfaces
`snpGeneInput`, `snpGeneUI` and `snpGeneOutput`.

## Usage

``` r
snpGeneApp()

snpGeneServer(id, snp_list, project_df)

snpGeneInput(id)

snpGeneOutput(id)
```

## Arguments

- id:

  identifier for shiny reactive

- snp_list, project_df:

  reactive arguments

## Value

tbl with top SNPs

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

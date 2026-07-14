# Shiny SNP summary module

Shiny module for SNP summary, with interfaces `shinySNPInput`,
`shinySNPUI` and `shinySNPOutput`.

## Usage

``` r
snpTableApp()

snpTableServer(id, snp_list, project_df)

snpTableOutput(id)
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

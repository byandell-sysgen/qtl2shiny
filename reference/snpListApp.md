# Shiny SNP List App

Shiny module to create list of SNP objects.

## Usage

``` r
snpListApp()

snpListServer(
  id,
  hotspot_list,
  project_df,
  snp_action = shiny::reactive({
     "add+dom"
 })
)

snpListInput(id)

snpListOutput(id)
```

## Arguments

- id:

  identifier for shiny reactive

- hotspot_list, project_df, snp_action:

  reactive arguments

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

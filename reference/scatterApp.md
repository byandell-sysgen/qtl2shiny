# Shiny Scatter App

Shiny Scatter App

## Usage

``` r
scatterApp()

scatterServer(
  id,
  hotspot_list,
  pattern_list,
  snp_list,
  pairprobs_obj,
  project_df
)

scatterInput(id)

scatterOutput(id)
```

## Arguments

- id:

  identifier for shiny reactive

- hotspot_list, pattern_list, snp_list, pairprobs_obj, project_df:

  reactive arguments

## Value

No return value; called for side effects.

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

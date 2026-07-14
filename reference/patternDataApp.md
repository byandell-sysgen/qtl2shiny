# Shiny Pattern Data App

Shiny module for SNP pattern plots.

## Usage

``` r
patternDataApp()

patternDataServer(
  id,
  dip_par,
  hotspot_list,
  snp_list,
  pairprobs_obj,
  project_df
)

patternDataInput(id)

patternDataUI(id)

patternDataOutput(id)
```

## Arguments

- id:

  identifier for shiny reactive

- hotspot_list, dip_par, pairprobs_obj, patterns, snp_action,
  project_df:

  reactive arguments

## Value

No return value; called for side effects.

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

# Shiny Allele App

Shiny Allele App

## Usage

``` r
genoEffectApp()

genoEffectServer(
  id,
  hotspot_list,
  pattern_list,
  snp_list,
  geno_list,
  pairprobs_obj,
  project_df
)

genoEffectUI(id)

genoEffectOutput(id)
```

## Arguments

- id:

  identifier for shiny reactive

- hotspot_list, pattern_list, pairprobs_obj, patterns, project_df,
  snp_action:

  reactive arguments

## Value

No return value; called for side effects.

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

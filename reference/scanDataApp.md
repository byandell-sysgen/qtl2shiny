# Shiny Scan Module

Shiny module for scan1 LOD and coefficient plots.

## Usage

``` r
scanDataApp()

scanDataServer(id, hotspot_list, snp_list, probs_obj, project_df)

scanDataInput(id)

scanDataOutput(id)
```

## Arguments

- id:

  identifier for shiny reactive

- hotspot_list, snp_list, probs_obj, project_df:

  reactive arguments

## Value

No return value; called for side effects.

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

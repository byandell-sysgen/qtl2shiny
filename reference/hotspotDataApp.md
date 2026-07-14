# Shiny Hotspot Data App

Shiny module to select hotspots for peak selection.

## Usage

``` r
hotspotDataApp()

hotspotDataServer(id, set_par, peak_df, pmap_obj, project_df)

hotspotDataInput(id)

hotspotDataOutput(id)
```

## Arguments

- id:

  identifier for shiny reactive

- set_par, peak_df, pmap_obj, project_df:

  reactive arguments

## Value

list of inputs and scan summary

No return value; called for side effects.

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

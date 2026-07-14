# Shiny Hotspot Plot App

Shiny module to view hotspots for peak selection.

## Usage

``` r
hotspotPlotApp()

hotspotPlotServer(id, set_par, hotspot_obj)

hotspotPlotOutput(id)
```

## Arguments

- id:

  identifier for shiny reactive

- set_par, hotspot_obj:

  reactive arguments

## Value

list of inputs and scan summary

No return value; called for side effects.

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

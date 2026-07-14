# Shiny Phenotype App

Shiny module for peak selection.

## Usage

``` r
peakApp()

peakServer(id, set_par, win_par, peak_read_df, project_df)

peakInput(id)

peakOutput(id)
```

## Arguments

- id:

  identifier for shiny reactive

- set_par, peak_read_df, pmap_obj, project_df:

  reactive arguments

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

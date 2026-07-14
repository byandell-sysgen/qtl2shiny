# Shiny Mediate Panel App

Shiny module for mediation analysis.

## Usage

``` r
mediateApp()

mediateServer(id, hotspot_list, snp_list, probs_obj, project_df)

mediateInput(id)

mediateOutput(id)
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

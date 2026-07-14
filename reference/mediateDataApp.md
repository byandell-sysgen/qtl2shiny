# Shiny Mediate Data App

Shiny Mediate Data App

## Usage

``` r
mediateDataApp()

mediateDataServer(id, hotspot_list, snp_list, probs_obj, project_df)

mediateDataInput(id)

mediateDataUI(id)

mediateDataOutput(id)
```

## Arguments

- id:

  identifier for shiny reactive

- hotspot_list, probs_obj, patterns, project_df:

  reactive arguments

## Value

No return value; called for side effects.

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

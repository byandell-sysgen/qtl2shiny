# Shiny Project App

Shiny module for selection of project, with interface `projectUI`.

## Usage

``` r
projectApp()

projectServer(id, projects_df)

projectUI(id)

projectOutput(id)
```

## Arguments

- id:

  identifier for shiny reactive

- projects_df:

  static project info data frame

## Value

No return value; called for side effects.

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

# Shiny Probability modules

Shiny genotype probability access.

## Usage

``` r
probsApp()

probsServer(id, win_par, project_df)

pairProbsServer(id, win_par, project_df)

snpProbsServer(id, win_par, pheno_names, project_df)
```

## Arguments

- id:

  identifier for shiny reactive

- win_par, pheno_names, project_df:

  reactive arguments

## Value

Object of class `probs`.

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

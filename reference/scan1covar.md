# Scan1 for traits with same covariates.

Modified from qtl2mediate version for newer data organization. Could
build out a companion routine splitting on covariates.

## Usage

``` r
scan1covar(
  pheno_mx,
  covar_df,
  genoprobs,
  kinship,
  peaks_df,
  model = "normal",
  ...,
  force = FALSE
)
```

## Arguments

- pheno_mx:

  matrix of phenotypes

- covar_df:

  data frame of covariates

- genoprobs:

  object with genotype probabilities

- kinship:

  kinship matrix or list of kinship matrices

- peaks_df:

  data frame of peaks information

- model:

  model argument for [`scan1`](https://rdrr.io/pkg/qtl2/man/scan1.html)

- ...:

  additional arguments passed on

- force:

  force pheno names to be in peaks

## Value

object of class [`scan1`](https://rdrr.io/pkg/qtl2/man/scan1.html).

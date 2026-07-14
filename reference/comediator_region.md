# Create list with mediators in region

Create list with mediators in region

## Usage

``` r
comediator_region(
  pheno_name,
  chr_id,
  scan_window,
  covar_df,
  peaks_df,
  qtls = 2,
  pmap,
  pheno_data
)
```

## Arguments

- pheno_name:

  phenotype name

- chr_id, scan_window:

  chromosome and start and end value

- covar_df:

  covariate data frame

- peaks_df:

  table of peaks

- qtls:

  number of drivers (1 or 2; default is 2)

- pmap:

  physical map

- pheno_data:

  phenotype data

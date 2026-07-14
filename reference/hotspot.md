# Hotspots for phenotypes

Count hotspots by pheno_group and pheno_type.

## Usage

``` r
hotspot(map, peak_df, peak_window = 1, minLOD = 5.5, chrs = NULL)

# S3 method for class 'hotspot'
summary(object, ...)

# S3 method for class 'hotspot'
subset(x, chr = NULL, nonzero = NULL, ...)

# S3 method for class 'hotspot'
cbind(..., scannames = colnames(scan))
```

## Arguments

- map:

  list of genetic maps

- peak_df:

  data frame of peak information

- peak_window:

  half-width of peak window in Mbp

- minLOD:

  minimum LOD to include in count

- chrs:

  chromosomes to subset if not \`NULL\`

## Value

object of class hotspot as list of
[`scan1`](https://rdrr.io/pkg/qtl2/man/scan1.html) and `map` objects.

## Author

Brian S Yandell, <brian.yandell@wisc.edu>

## Examples

``` r
dirpath <- "https://raw.githubusercontent.com/rqtl/qtl2data/master/DOex"

# Read DOex example cross from 'qtl2data'
DOex <- qtl2::read_cross2(file.path(dirpath, "DOex.zip"))
DOex <- subset(DOex, chr = "2")

# Calculate genotype and allele probabilities
pr <- qtl2::calc_genoprob(DOex, error_prob=0.002)

# Summary of coefficients at scan peak
scan_pr <- qtl2::scan1(pr, DOex$pheno)
peak_df <- summary(scan_pr, DOex$pmap)

hotspot(DOex$pmap, peak_df)
#> Error in dplyr::filter(peak_df, .data$qtl_lod >= minLOD): ℹ In argument: `.data$qtl_lod >= minLOD`.
#> Caused by error in `.data$qtl_lod`:
#> ! Column `qtl_lod` not found in `.data`.

# Select Sex and Cohort columns of covariates
analyses_tbl <- data.frame(pheno = "OF_immobile_pct", Sex = TRUE, Cohort = TRUE)

# Get hotspot (only one phenotype here).
out <- hotspot(DOex$pmap, peak_df)
#> Error in dplyr::filter(peak_df, .data$qtl_lod >= minLOD): ℹ In argument: `.data$qtl_lod >= minLOD`.
#> Caused by error in `.data$qtl_lod`:
#> ! Column `qtl_lod` not found in `.data`.
summary(out)
#> Error: object 'out' not found
```

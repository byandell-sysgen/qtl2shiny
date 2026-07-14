# Create list with phenotypes in region

Create list with phenotypes in region

Create list with expression phenotypes in region

## Usage

``` r
pheno_region(
  chr_id,
  scan_window,
  covar,
  map,
  peaks_df,
  pheno_data,
  drivers = 2
)

expr_region(
  chr_id,
  scan_window,
  covar,
  map,
  project_dir,
  drivers = 2,
  query_mrna = create_mrna_query_func(project_dir)
)
```

## Arguments

- chr_id, scan_window:

  chromosome and start and end value

- covar:

  covariate data frame

- map:

  list or vector of map positions

- peaks_df:

  table of peaks

- pheno_data:

  matrix of phenotype data

- drivers:

  number of drivers (1 or 2; default is 2)

- project_dir:

  project directory with mRNA data in subdirector `RNAseq`

- query_mrna:

  query routine for mRNA data (see `create_mrna_query_func`)

## Value

list containing `pheno`, `annot` and `covar`.

## See also

`create_mrna_query_func`

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
peaks_df <- summary(scan_pr, DOex$pmap)

# Select Sex and Cohort columns of covariates
analyses_tbl <- data.frame(pheno = "OF_immobile_pct", Sex = TRUE, Cohort = TRUE)

# Get phenos in region.
out <- pheno_region("2", c(90, 100), DOex$covar, DOex$pmap, peaks_df, analyses_tbl, DOex$pheno)
#> Error in dplyr::group_by(peaks_df, .data$phenotype): Must group by variables found in `.data`.
#> ✖ Column `phenotype` is not found.
str(out)
#> Error: object 'out' not found
```

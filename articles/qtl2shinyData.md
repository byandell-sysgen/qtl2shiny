# Prepare qtl2 data for qtl2shiny

The popular [R/qtl](http://www.rqtl.org/) package for gene mapping has
been redeveloped for high volume, multi-parent systems genetics data as
[R/qtl2](http://www.rqtl.org/qtl2/). This new package provides a
[Shiny](http://shiny.rstudio.org/) interface to extensions of
[R/qtl2](http://www.rqtl.org/qtl2/).

<http://pages.stat.wisc.edu/~yandell/software/qtl2shiny/> contains
further information on the ‘qtl2shiny’ server in action, including
[R/qtl2shiny Screen
Shots](http://pages.stat.wisc.edu/~yandell/software/qtl2shiny/screenshots.md).

For more details on use of the package, see the [R/qtl2shiny
package](https://github.com/byandell/qtl2shiny), [R/qtl2shiny User
Guide](https://github.com/byandell/qtl2shiny/blob/master/vignettes/UserGuide.Rmd)
and [R/qtl2shiny Developer
Guide](https://github.com/byandell/qtl2shiny/blob/master/vignettes/DeveloperGuide.Rmd).
This document is created by
[inst/scripts/qtl2shiny_index.Rmd](https://github.com/byandell/qtl2shiny/blob/master/inst/scripts/qtl2shiny_index.Rmd).

See also

- [R/qtl2ggplot](https://github.com/byandell/qtl2ggplot/) and on CRAN at
  <https://cran.r-project.org/package=qtl2ggplot>
- [R/qtl2fst](https://github.com/rqtl/qtl2fst/): fast ‘qtl2’ data
- [R/qtl2pattern](https://github.com/byandell/qtl2pattern/): explore SNP
  patterns
- [R/qtl2mediate](https://github.com/byandell/qtl2mediate): qtl2
  mediation interface
  - [R/intermediate](https://github.com/byandell/intermediate/):
    mediation

This document sets up the `Recla` project using data from
<https://github.com/rqtl/qtl2data> for use with
[R/qtl2shiny](https://github.com/byandell/qtl2shiny).

The `project_info` data frame contains control information about each
project (see [R/qtl2shiny](https://github.com/byandell/qtl2shiny)).

This process takes some time, particularly for calculating genotype
probabilities with ‘qtl2::calc_genoprob’ and scanning the phenotypes
with ‘qtl2::scan1’; see <https://kbroman.org/qtl2/>.

``` r

(project_info <- data.frame(project = params$project,
                           taxa = params$taxa,
                           directory = params$shiny_directory))
```

    ##   project    taxa     directory
    ## 1   Recla CCmouse qtl2shinyData

Assume local directory already created with `taxa` and `project` under.

``` r

(localdir <- params$root_directory)
```

    ## [1] "../../qtl2shinyApp"

The processed data will reside in the following directory:

``` r

taxa_path <- file.path(project_info$directory, project_info$taxa)
taxa_dir <- file.path(localdir, taxa_path)
project_path <- file.path(taxa_path, project_info$project)
(project_dir <- file.path(localdir, project_path))
```

    ## [1] "../../qtl2shinyApp/qtl2shinyData/CCmouse/Recla"

The data is pulled from this internet directory:

``` r

(project_data <- file.path(
  "https://raw.githubusercontent.com/rqtl",
  "qtl2data/master/DO_Recla/recla.zip"))
```

    ## [1] "https://raw.githubusercontent.com/rqtl/qtl2data/master/DO_Recla/recla.zip"

If `dontrun` is TRUE then just show remaining code without running.

``` r

if(params$dontrun) {
  warning("Showing rest of code without running")
  knitr::opts_chunk$set(eval = FALSE, echo = TRUE)
}
```

    ## Warning: Showing rest of code without running

Stop if local directory does not exist.

``` r

if(!dir.exists(localdir)) {
  warning(paste("local directory", localdir, "does not exist and must be created"))
  knitr::knit_exit()
}
```

Stop if deepest `project_dir` already exists. Assumption is that data
already exists and do not want to overwrite.

``` r

if(dir.exists(project_dir)) {
  warning(paste("project directory", project_dir,
                "already exists and will not be overwritten"))
  knitr::knit_exit()
}
```

## Create project directories

Create directories if they do not exist.

``` r

if(!dir.exists(project_dir)) {
  if(!dir.exists(taxa_dir)) {
    if(!dir.exists(data_dir <- file.path(localdir,
                                       project_info$directory)))
      dir.create(data_dir)
    dir.create(taxa_dir)
  }
  dir.create(project_dir)
}
```

## qtl2shiny files

These are files that sit in the `localdir` for use by the shiny app.

``` r

for(filename in c("app.R", "about.md", "about-extended.md")) {
  if(file.exists(fname <- file.path(localdir, filename))) {
    warning(paste("qtl2shiny file", fname,
                  "already exists and will not be overwritten"))
  } else {
    file.copy(system.file(file.path("qtl2shinyApp", filename), package = "qtl2shiny"), fname)
  }
}
```

``` r

allele_info <- 
  data.frame(
    code = LETTERS[1:8],
    shortname = names(qtl2::CCcolors),
    longname = c("A/J","C57BL/6J","129S1/SvImJ","NOD/LtJ","NZO/HlLtJ","CAST/EiJ","PWK/PhJ","WSB/EiJ"),
    allelename = c("A_J","C57BL_6J","129S1_SvImJ","NOD_LtJ","NZO_HlLtJ","CAST_EiJ","PWK_PhJ","WSB_EiJ"),
    link = c("https://www.jax.org/strain/000646",
             "https://www.jax.org/strain/000664",
             "https://www.jax.org/strain/002448",
             "https://www.jax.org/strain/001289",
             "https://www.jax.org/strain/002105",
             "https://www.jax.org/strain/000928",
             "https://www.jax.org/strain/003715",
             "https://www.jax.org/strain/001145"),
    color = qtl2::CCcolors,
    color_name = c("yellow","grey","salmon","blue","aqua","green","red","purple"),
    origcolor = qtl2::CCorigcolors,
    stringsAsFactors = FALSE
  )
```

``` r

saveRDS(allele_info, file.path(taxa_dir, "allele_info.rds"))
```

``` r

taxa_info <- "Mus musculus"
```

``` r

saveRDS(taxa_info, file.path(taxa_dir, "taxa_info.rds"))
```

## Query functions

The query for genes and variants are usually common across all projects
for a taxa. Here we use query routines from R/qtl2 for the CC mouse. Do
not create if they already exist. Note that the `taxa_path` is a
relative address.

``` r

if(file.exists(qname <- file.path(taxa_dir, "query_genes.rds"))) {
  warning(paste("gene query", qname,
                "already exists and will not be overwritten"))
} else {
  query_genes <- 
    qtl2::create_gene_query_func(
      file.path(taxa_path, "mouse_genes.sqlite"))
  saveRDS(query_genes, qname)
}
```

``` r

if(file.exists(qname <- file.path(taxa_dir, "query_variants.rds"))) {
  warning(paste("variant query", qname,
                "already exists and will not be overwritten"))
} else {
  query_variants <- 
    qtl2::create_variant_query_func(
      file.path(taxa_path, "cc_variants.sqlite"))
  saveRDS(query_variants, qname)
}
```

Use `create_probs_query_func` from package ‘qtl2pattern’ to query
genotype probabilities. Save it in RDS format for later use. Note that
it requires a relative address to the project data provided by
`project_path`.

``` r

query_probs <- qtl2pattern::create_probs_query_func(project_path)
saveRDS(query_probs, file.path(project_dir, "query_probs.rds"))
```

These data do not include mRNA data, so set up null routine. Make sure
it has required arguments.

``` r

query_mrna <- qtl2shiny::create_mrna_query_func(NULL)
saveRDS(query_mrna, file.path(project_dir, "query_mrna.rds"))
```

``` r

if(httr::HEAD(project_data)$status != 200) {
  warning(paste("project data for", project_info$project, "does not exist"))
  knitr::knit_exit()
}
```

``` r

qtl2data <- qtl2::read_cross2(project_data)
```

``` r

covar <- dplyr::rename(qtl2data$covar, sex = "Sex")
```

``` r

saveRDS(covar, file.path(project_dir, "covar.rds"))
saveRDS(qtl2data$pheno, file.path(project_dir, "pheno_data.rds"))
```

``` r

gmap <- qtl2::insert_pseudomarkers(qtl2data$gmap, step=1)
pmap <- qtl2::interp_map(gmap, qtl2data$gmap, qtl2data$pmap)
saveRDS(pmap, file.path(project_dir, "pmap.rds"))
```

## Genotype probabilities

Genotype probabilities can be very large and are best put in a fast
database (such as “fst” or “feather”). Current recommendation is “fst”.
This step takes time, primarily in calculating the genotype
probabilities.

``` r

fast <- "fst"
```

``` r

if(!dir.exists(genoprob_dir <- file.path(project_dir, "genoprob"))) {
  dir.create(genoprob_dir)
}
faprobs_file <- file.path(genoprob_dir, paste0("aprobs_fstindex.rds"))
if(!file.exists(fprobs_file <- file.path(genoprob_dir,
                                         paste0("probs_fstindex.rds")))) {
  cat("allele pair genotype probabilities ...\n", file = stderr())
  pr <- qtl2::calc_genoprob(qtl2data, gmap, err=0.002)
  cat(paste0("allele pair prob conversion via qtl2", fast, "\n"), file = stderr())
  fast_genoprob <- switch(fast,
                          fst = qtl2fst::fst_genoprob,
                          feather = qtl2feather::feather_genoprob)
  fprobs <- fast_genoprob(pr, "probs", genoprob_dir, quiet = TRUE)
  saveRDS(fprobs, file = fprobs_file)

  cat("allele genotype probabilities\n", file = stderr())
  apr <- qtl2::genoprob_to_alleleprob(pr)
  cat(paste0("allele prob conversion via qtl2", fast, "\n"), file = stderr())
  faprobs <- fast_genoprob(apr, "aprobs", genoprob_dir, quiet = TRUE)
  saveRDS(faprobs, file = faprobs_file)
} else {
  fprobs <- readRDS(fprobs_file)
  faprobs <- readRDS(faprobs_file)
}
```

``` r

kinship_loco <- qtl2::calc_kinship(faprobs, "loco")
```

``` r

saveRDS(kinship_loco, file.path(project_dir, "kinship.rds"))
```

## Genome Scans

``` r

form <- formula(paste("~", paste(names(covar), collapse = "+")))
addcovar <- stats::model.matrix(form, covar)[, -1, drop = FALSE]
out <- qtl2::scan1(faprobs, qtl2data$pheno, kinship_loco, addcovar=addcovar)
```

``` r

peaks <- qtl2::find_peaks(out, pmap, threshold = 3)
```

## Set up analyses and peaks tables for qtl2shiny

``` r

peaks <- 
  dplyr::select(
    dplyr::mutate(
      dplyr::rename(
        peaks,
        pheno = "lodcolumn"),
    longname = .data$pheno,
    output = .data$pheno,
    pheno_group = "group", # used to group phenotypes
    pheno_type = "type"),  # used to type phenotypes
  -.data$lodindex)
```

``` r

analyses_tbl <- 
  dplyr::mutate(
    dplyr::select(
      dplyr::distinct(
        peaks,
        .data$pheno,
        .keep_all = TRUE),
    -.data$lod, -.data$pos, -.data$chr),
  model = "normal",
  transf = "identity",
  offset = 0,
  winsorize = FALSE)
for(i in names(covar))
  analyses_tbl[[i]] <- TRUE
```

``` r

saveRDS(peaks, file.path(project_dir, "peaks.rds"))
saveRDS(analyses_tbl, file.path(project_dir, "analyses.rds"))
```

## List files

``` r

localdir
```

``` r

list.files(localdir)
```

``` r

taxa_dir
```

``` r

list.files(taxa_dir)
```

``` r

project_dir
```

``` r

list.files(project_dir)
```

``` r

list.files(file.path(project_dir, "genoprob")) 
```

``` r

file.size(file.path(project_dir, "genoprob", "probs_fstindex.rds"))
```

``` r

file.size(file.path(project_dir, "genoprob", "probs_1.fst"))
```

## Hotspots

``` r

hots <- qtl2shiny::hotspot(pmap, peaks)
```

``` r

saveRDS(hots, file.path(project_dir, "hotspot.rds"))
```

``` r

# See shinyHotspot.R
hotspots <- readRDS(file.path(project_dir, "hotspot.rds"))
```

``` r

# See shinyHotspot.R
qtl2ggplot::ggplot_scan1(hotspots$scan, hotspots$map)
```

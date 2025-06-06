# README #

Yandell R/qtl2shiny project.

### What is this repository for? ###

* Code to create shiny interface for [R/qtl2](https://cran.r-project.org/package=qtl2).
* Version 1.1.2
* See following documents:
    + [R/qtl2shiny Screen Shots](http://pages.stat.wisc.edu/~yandell/software/qtl2shiny/screenshots.html)
    + [R/qtl2shiny User Guide](https://github.com/byandell/qtl2shiny/blob/master/vignettes/UserGuide.Rmd)
    + [R/qtl2shiny Developer Guide](https://github.com/byandell/qtl2shiny/blob/master/vignettes/DeveloperGuide.Rmd)
    + [R/qtl2shiny Data Preparation](https://github.com/byandell/qtl2shiny/blob/master/vignettes/qtl2shinyData.Rmd)

### What has been done ###

- created [Shiny](https://shiny.rstudio.com) interface for [R/qtl2](https://cran.r-project.org/package=qtl2) data
    + handles multiple phenotypes and multiple projects
    + creates plots and scans on the fly
    + uses Shiny modules and dashboard
- organized in package R/qtl2shiny
- depends on related packages
    + [R/qtl2](https://cran.r-project.org/package=qtl2)
    + [R/qtl2ggplot2](https://cran.r-project.org/package=qtl2ggplot)
    + [R/qtl2fst](https://cran.r-project.org/package=qtl2fst)
    + [R/qtl2pattern](https://cran.r-project.org/package=qtl2pattern)
    + [R/qtl2mediate](https://github.com/byandell/qtl2mediate)
    + [R/intermediate](https://github.com/byandell/intermediate)

### What are open development issues ###

Major issues

- speed up access
- save intermediate calculations that are reused
- make sure multiple taxa work smoothly

Minor issues

* shiny
    + reveal plots more (settings on sidebar? tabs?)
    + user save settings for quick replay of shiny
* markdown
    + flexdashboard for Rmd to create dynamic reports
    + link Rmd and shiny documents
* genes
    + GeneRegion and GeneExon for multiple traits giving different results
    + upgrade these to use `plot_genes`
* scans
    + multiple traits now use superset of covariates
    + fixed for Scan1Plot; need to do same for Scan1SNP and possibly elsewhere
* SNP/Gene Action
    + panel appears not to work at all
* reset
    + reset buttons when new dataset (i.e. to Genome Scan)

### Contribution guidelines ###

* Writing tests
* Code review
* Other guidelines

### Who do I talk to? ###

* [Brian Yandell](http://github.com/byandell)
* [Karl Broman](http://github.com/kbroman)
  + [R/qtl2](https://cran.r-project.org/package=qtl2)

### Installation

R/qtl2 is now available on CRAN, as are R/qtl2ggplot and R/qtl2pattern.

You first need to install the
[devtools](https://cran.r-project.org/package=devtools) package, plus a set of
package dependencies: [yaml](https://cran.r-project.org/package=yaml),
[jsonlite](https://cran.r-project.org/package=jsonlite),
[data.table](https://cran.r-project.org/package=data.table),
and [RcppEigen](https://cran.r-project.org/package=RcppEigen).
(Additional, secondary dependencies will also be installed)

    install.packages(c("devtools", "yaml", "jsonlite", "data.table", "RcppEigen", "RSQLite"))

You will also need the following packages for qtl2shiny dependencies:

    install.packages(c("tidyverse", "RColorBrewer", "fst",
      "shiny", "shinydashboard", "grid", "gridBase", "gdata", "GGally", "Rcpp",
      "mnormt", "corpcor", "qtl2", "qtl2fst", "qtl2ggplot", "qtl2pattern"))

Then, install plotly using `devtools::install_github()`.

    library(devtools)
    install_github("ropensci/plotly")

Once you have installed these, install qtl2shiny and related packages as

    install_github(paste0("byandell/",
      c("intermediate","qtl2mediate","qtl2shiny")))

To install `qtl2shiny` with vignettes (takes a bit longer):

    install_github("byandell/qtl2shiny", build_vignettes=TRUE)

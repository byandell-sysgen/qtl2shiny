# Shiny Module Organization

This package contains a collection of ~30 shiny modules organized
into panels.
Each panel on the `qtl2shiny` app is a shiny module that calls multiple
modules.

```
├── hotspotPanel              # Hotspots and Phenotypes
├── scanPanel                 # Allele and SNP Scans
├── mediatePanel              # Mediation
├── patternPanel              # SDP Pattern Scans
```

These are all being organized into the
[qtl2shinyApp()](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/qtl2shinyApp.R)

A module has several components, as described in the
[Shiny Modules](https://mastering-shiny.org/scaling-modules.html)
chapter of the `Mastering Shiny` site.

```
├── xxxApp()              # runnable shiny app
├── xxxServer()           # shiny server
├── xxxInput()            # (optional) shiny ui for input
├── xxxUI()               # (optional) shiny ui for user interface
├── xxxOutput()           # (optional) shiny ui for output
├── result <- xxServer()  # result object from server
```

Here is a prototype app with all these components.
The `"id"` is a common identifier across these components.
The user interface function `xxxUI()` might appear in the `sidebar`
or in the body of the `page`, depending on its use.

```
xxxApp() <- function() {
  ui <- bslib::page(
    sidebar = bslib::sidebar(
      xxxInput("id"),
      xxxUI("id")
    ),
    xxxOutput("id")
  )
  server <- function(input, output, session) {
    result <- xxxServer("id")
  }
  shiny::shinyApp(ui, server)
}
```

Each shiny module is organized in a single file.
Below, they are often referenced in terms of their `server` function,
which sets up the shiny logic.

# Data Reading

Data are read based on the `project`, which is determined through the 
`project` server
This uses a flat (`CSV`) file to identify where data are stored.
See
[qtl2shinyApp/README.md](https://github.com/byandell-sysgen/qtl2shinyApp/blob/main/README.md)
for detailed information on data organization.
In addition, the 
`class` (phenotype dataset) and
`subject_model` (subset of `subject`s and additive/interactive `model` type)
are determined with the `setPar` server.

- [projectServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/projectApp.R) # select `project`
- [setParServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/setParApp.R) # select `class` and `subject_model`

### Phenotype and Other Project Data

Project-specific phenotype data are read in as needed using the following modules 

- `peak_df <-` [peakServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/peakApp.R) # precomputed peaks and summaries
- `pheno_mx <-` [phenoServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/phenoApp.R) # raw phenotype data

The `peak` server is straight-forward, depending on `project`, `class` and `subject_model`
through the `project` and `setPar` servers.
The `pheno` server depends on `project` and `class`, but is a bit more complicated, as it uses the
[phenoNamesServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/phenoNamesApp.R)
server to identify the phenotype names.
Both `peak` and `pheno` servers depend on (directly and indirectly, resp.)
on the `hotspot` server to identify the hotspot of interest.

Other `project` data files are read in as needed using the
[read_project()](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/readproject.R)
function, which depends on the `project`.

- [covar_df](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/covarApp.R): covariate data frame
- [kinship_list](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/kinshipApp.R): kinship list of LOCO matrices
- `pmap_obj`: physical map object with list of markers and `Mbp` positions
- `allele_info`: allele information data frame

### Project Genotype and SNP Information

Genotype and SNP information are dynamically read for a specfied `hotspot`.
They a bit complicated, as data are stored in large `SQLite` or `FST` files.
They rely on specially designed `query` functions (see
[qtl2shinyApp/README.md](https://github.com/byandell-sysgen/qtl2shinyApp/blob/main/README.md)
and links) that are embedded in shiny modules

- [probsServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/probsApp.R) # read FST probability object
- [geneRegion()](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/geneRegionApp.R) # read and plot genes in a region
- [geneExon()](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/geneExonApp.R) # read and plot exons of a gene

The `probs` (alleles) and `pairProbs` (allele pairs) servers use the
`query_probs()` function, while the
`snpProbs` (SNP variants) server uses the
`query_variants()` function.
The `geneRegion` and `geneExon` servers use the
`query_genes()` function.
These are pre-determined functions saved as `RDS` files in the
data area and read in dynamically (as needed) with the
[read_query_rds](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/read_query_rds.R)
internal function.

## Panel organizations

Panels are dependent on each other based on input parameters
and read or calculated results.
For instance, the `pheno` panel depends on the `hotspots` panel
(and both are organized at present into one `Hotspots` panel),
while all other panels depend on these two.
Information is passed among panels via result lists,
which are typically
[reactiveValues](https://mastering-shiny.org/reactivity-objects.html).

The hotspot panel has tabs for `Hotspots` and `Phenotypes`.
It depends on the `peak_df` and `pmap_obj` objects, as well as the
`project` and `setPar` modules.
As currently configured, this panel has the following additional modules:

- [hotspotPanel](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/hotspotPanelApp.R) # Hotspot and phenotype panels
  - [hotspot](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/hotspotApp.R) # show hotspots from `peaks` with summary or plot, select a hotspot
  - [phenoPanel](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/phenoPanelApp.R) # Phenotype panel
  - [winPar](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/winParApp.R) # Hotspot window parameters
  
The phenotype panel consists of the following four modules:

- [phenoPanel](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/phenoPanelApp.R)                # Phenotype Panel
  - [phenoNames](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/phenoNamesApp.R)            # Select phenotype names
  - [pheno](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/phenoApp.R)                 # Create phenotype object and summary
  - [phenoPlot](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/phenoPlotApp.R)             # Plot phenotypes

The scan panel is not quite configured as needed,
residing for now in the
[haploApp](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/haploApp.R)
module.
It consists of the following components:

- [scan](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/scanApp.R) # allele-based genome scan
- [snpList](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/snpListApp.R) # calculation of SNP objects, including SNP scan
- [snpGene](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/snpGeneApp.R) # SNP association and genes
  - [snpSum](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/snpSumApp.R) # SNP association summary
  - [snpPlot](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/snpPlotApp.R) # SNP association plot
  - [geneRegion](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/geneRegionApp.R) # genes within `hotspot` region
  - [geneExon](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/geneExonApp.R) # exons within gene

The mediation panel consists of the following modules:

- [mediatePanel](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/phenoPanelApp.R)              # Mediation Panel
  - [mediate](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/phenoNamesApp.R)         # Run mediation
  - [mediatePlot](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/phenoApp.R)                       # Plot mediation results
  - [triad](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/triadApp.R)         # Plot D-M-T triad scatterplots

The pattern module is currently spread between the
[haploApp](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/haploApp.R)
and
[diploApp](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/diploApp.R)
modules.
It consists of the following components

- [snpPattern](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/snpPatternApp.R) # SNP Pattern Scan plot and summary
  - [snpFeature](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/snpFeatureApp.R)

- [pattern](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/patternApp.R) # SDP Scans summary
- [patternPlot](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/patternPlotApp.R) # SDP Scans plot
- [allele](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/alleleApp.R) # Allele Pattern plot and summary
- [dipPar](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/dipParApp.R) # input needed parameters across modules

### Download App

There is a
[downloadApp()](https://github.com/byandell-sysgen/qtl2shiny/blob/refactor/R/downloadApp.R)
that will eventually be connected to the panels.
The idea is that, at any time, each panel likely displays one plot
and one table (summary) that a user might want to download,
as PNG or PDF (plot) or CSV (table).
This will involve adjusting the panel `result`s to provide the
link back to plot or table object.
This is somewhat analagous to what was done in
[foundrShiny](https://github.com/AttieLab-Systems-Genetics/foundrShiny)
(see [foundrShiny/R/downloadApp.R](https://github.com/AttieLab-Systems-Genetics/foundrShiny/blob/main/R/downloadApp.R))
and
[qtlApp](https://github.com/AttieLab-Systems-Genetics/qtlApp)
(see [qtlApp/R/modules/downloadApp.R](https://github.com/AttieLab-Systems-Genetics/qtlApp/blob/refactor/fs-reorg/R/modules/downloadApp.R)).

### Deprecated Apps

- `hapPar` (subsumed as parameters no longer used)
- `dash` (see `qtl2shinyApp`)
- `main` (see `qtl2shinyApp`)
- `covar` (use `read_project` instead)

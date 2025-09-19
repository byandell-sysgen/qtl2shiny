# Shiny Module Organization

This package contains a collection of ~30 shiny modules organized
into panels.
Each panel on the `qtl2shiny` app is a shiny module that calls multiple
modules.

```
qtl2shinyApp
├── hotspotPanel              # Hotspots and Phenotypes
    └── phenoPanel            # Phenotypes
├── scanPanel                 # Allele and SNP Scans
├── mediatePanel              # Mediation
├── patternPanel              # SDP Pattern Scans
└── genoPanel                 # Genotypes
```

These are all being organized into the
[qtl2shinyApp()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/qtl2shinyApp.R)

A module has several components, as described in the
[Shiny Modules](https://mastering-shiny.org/scaling-modules.html)
chapter of the `Mastering Shiny` site.

```
xxxApp.R                  # file containing app functions
├── xxxApp()              # runnable shiny app
├── xxxServer()           # shiny server function with (optional) result
├── xxxInput()            # (optional) shiny ui function for input
├── xxxUI()               # (optional) shiny ui function for user interface
└── xxxOutput()           # (optional) shiny ui function for output
```

Here is a prototype app with all these components.
The `"id"` is a common identifier across these components.
The user interface function `xxxUI()` might appear in the `sidebar`
or in the body of the `page`, depending on its use.
Note the `result` returned from `xxxServer()`.

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

## Data Reading

Data are read based on the `project`, which is determined through the 
`project` server
This uses a flat (`CSV`) file called `projects.csv` in folder `data`
to identify where data are stored,
Something like this:

|project|taxa|directory|
|:------|:---|:--------|
|DO1200|CCmouse|qtl2shinyData|
|Recla|CCmouse|qtl2shinyData|

Both `project`s are in directory `qtl2shinyData`, in the taxa `CCmouse`.
See
[qtl2shinyApp/README.md](https://github.com/byandell-sysgen/qtl2shinyApp/blob/main/README.md)
for detailed information on data organization.
In addition, the 
`class` (phenotype dataset) and
`subject_model` (subset of `subject`s and additive/interactive `model` type)
are determined with the `setPar` server.

- `project_df <-` [projectServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/projectApp.R) #
select `project`
- `set_par <-` [setParServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/setParApp.R) #
select `class` and `subject_model`

### Phenotype and Other Project Data

Project-specific phenotype data are read in as needed using the following modules 

- `peak_df <-` [peakServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/peakApp.R) #
precomputed peaks and summaries
- `pheno_mx <-` [phenoServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoApp.R) #
raw phenotype data

The `peak` server is straight-forward, depending on `project`, `class` and `subject_model`
through the `project` and `setPar` servers.
The `pheno` server depends on `project` and `class`, but is a bit more complicated, as it uses the
[phenoNamesServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoNamesApp.R)
server to identify the phenotype names.
Both `peak` and `pheno` servers depend on (directly and indirectly, resp.)
on the `hotspot` server to identify the hotspot of interest.

Other `project` data files are read in as needed using the
[read_project()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/readproject.R)
function, which depends on the `project`.
The `kinship_list` has a server since it depends on the reactively chosen chromosome.

- `kinship_list <-` [kinshipServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/kinshipApp.R) #
kinship list of LOCO matrices
- `covar_df <- read_project(project_df(), "covar")` 
covariate data frame
- `pmap_obj <- read_project(project_df(), "pmap")` #
physical map object with list of markers and `Mbp` positions
- `allele_info <- read_project(project_df(), "allele")` #
allele information data frame

### Project Genotype and SNP Information

Genotype and SNP information are reactively read for a specfied `hotspot`.
They a bit complicated, as data are stored in large `SQLite` or `FST` files.
They rely on specially designed `query` functions (see
[qtl2shinyApp/README.md](https://github.com/byandell-sysgen/qtl2shinyApp/blob/main/README.md)
and links) that are embedded in shiny modules

- `probs_obj <-` [probsServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/probsApp.R) #
read FST probability object
- [geneRegion()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/geneRegionApp.R) #
read and plot genes in a region
- [geneExon()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/geneExonApp.R) #
read and plot exons of a gene

The `probs` (alleles) and `pairProbs` (allele pairs) servers use the
`query_probs()` function, while the
`snpProbs` (SNP variants) server uses the
`query_variants()` function.
The `geneRegion` and `geneExon` servers use the
`query_genes()` function.
These are pre-determined functions saved as `RDS` files in the
data area and read in reactively (as needed) with the
[read_query_rds](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/read_query_rds.R)
internal function.

## Panels Module Organization

Panels are dependent on each other based on input parameters
and other module results.
For instance, the `pheno` panel depends on the `hotspots` panel
(and both are organized at present into one `Hotspots` panel),
while all other panels depend on these two
(details involving results not shown here).
Information is passed among panels via result lists,
which are typically
[reactiveValues](https://mastering-shiny.org/reactivity-objects.html).

The overarching module is
[qtl2shiny](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/qtl2shinyApp.R),
which has the following functions:

- `qtl2shinyApp()`: run the app; code that could be copied to `app.R`
- `qtl2shinyServer()`: complete server logic
- `qtl2shinyUI()`: complete UI with panel structure

The top-level module app looks like this:

```
qtl2shinyApp <- function() {
  projects_df <- read.csv("qtl2shinyData/projects.csv", stringsAsFactors = FALSE)
  ui <- qtl2shinyUI("qtl2shiny")
  server <- function(input, output, session) {
    qtl2shinyServer("qtl2shiny", projects_df)
  }
  shiny::shinyApp(ui, server)
}
```

### Hotspot and Phenotype Panel

The hotspot panel has tabs for `Hotspots` and `Phenotypes`.
It depends on the `peak_df` and `pmap_obj` objects, as well as the
`project` and `setPar` modules.
This panel has the following modules:

- [hotspotPanel](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/hotspotPanelApp.R) #
Hotspot and Phenotype Panel
  - [hotspot](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/hotspotApp.R) #
summary or plot of `peak_df` hotspots, select hotspot
  - [phenoPanel](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoPanelApp.R) #
Phenotype panel
  - [winPar](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/winParApp.R) #
Hotspot window parameters

The phenotype panel, subsumed in the hotspot panel,
consists of the following four modules:

- [phenoPanel](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoPanelApp.R) #
Phenotype Panel
  - [phenoNames](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoNamesApp.R) #
Select phenotype names
  - [pheno](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoApp.R) #
Create phenotype object and summary
  - [phenoPlot](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoPlotApp.R) #
Plot phenotypes

### Scan Panel

The `scan` panel performs allele-based scan and SNP association,
and shows genes and exons in the selected hotspot.
It consists of the following components:

- [scanPanel](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/scanPanelApp.R) #
Scan by Alleles and SNPs Panel
  - [scan](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/scanApp.R) #
allele-based genome scan
  - [snpGene](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/snpGeneApp.R) #
SNP association and genes
    - [snpSum](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/snpSumApp.R) #
SNP association summary
    - [snpPlot](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/snpPlotApp.R) #
SNP association plot
    - [geneRegion](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/geneRegionApp.R) #
genes within `hotspot` region
    - [geneExon](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/geneExonApp.R) #
exons within gene

### Mediation Panel

The mediation panel consists of the following modules:

- [mediatePanel](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoPanelApp.R) #
Mediation Panel
  - [mediate](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoNamesApp.R) #
Run mediation
  - [mediatePlot](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoApp.R) #
Plot mediation results
  - [triad](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/triadApp.R) #
Plot D-M-T triad scatterplots

### SDP Pattern Panel

The pattern panel examines the strain distribution pattern (`SDP`)
in a variety of ways.
It consists of the following modules:

- [patternPanelApp](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/patternPanelApp.R) #
SDP Pattern Panel
  - [snpPattern](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/snpPatternApp.R) #
SNP Pattern Scan plot and summary
    - [snpFeature](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/snpFeatureApp.R) #
SNP Features (Pattern and Consequence)
  - [pattern](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/patternApp.R) #
SDP Scans summary
  - [patternPlot](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/patternPlotApp.R) #
SDP Scans plot

### Genotype Panel

Finally, the `geno` panel shows the allele, allele pair and SDP genotypes
for a selected genome location.

- [genoPanel](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/genoPanelApp.R) #
Genotype and Effects Panel
  - [geno](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/genoApp.R) #
Genotypes table
  - [genoEffect](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/genoApp.R) #
Genotype Effects plot and table

### Utility Modules

The following utility modules are used for several other modules

- [snpList](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/snpListApp.R) #
calculate SNP objects, including SNP scan
- [dipPar](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/dipParApp.R) #
input parameters needed across SNP and SDP modules

## Download App

There is a
[downloadApp()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/downloadApp.R)
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
(see [qtlApp/R/modules/downloadApp.R](https://github.com/AttieLab-Systems-Genetics/qtlApp/blob/master/fs-reorg/R/modules/downloadApp.R)).
Here is my current thinking

- each panel returns a `reactiveValue` with element `download`
  - this identifies the currently viewed `plot` or `table` and a `filename`
- these returns are collected into another `reactiveValue` `DL`
- pass `DL` and the current panel (`input$panel`) to `downloadServer()`

```
DL <- shiny::reactiveValues()
DL$hotspot <- hotspotPanelServer()
DL$scan    <- scanPanelServer()
DL$mediate <- mediatePanelServer()
DL$pattern <- patternPanelServer()
DL$geno    <- genoPanelServer()
downloadServer("download", DL, input$panel)
```

## Deprecated Files

- `dashApp.R` (see `qtl2shinyApp`)
- `mainApp.R` (see `qtl2shinyApp`)
- `haploApp.R` (early version of a panel)
- `hapParApp.R` (subsumed as parameters no longer used)
- `diploApp.R` (early version of a panel)
- `helpPopup.R`

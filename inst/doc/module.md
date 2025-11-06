# Shiny Module Organization

This package contains a collection of ~40 shiny modules organized
into panels.
Each panel on the `qtl2shiny` app is a shiny module that calls multiple
modules.

```
qtl2shinyApp
├── hotspot              # Hotspots and Phenotypes
    └── pheno            # Phenotypes
├── scan                 # Allele and SNP Scans
├── mediate              # Mediation
├── pattern              # SDP Pattern Scans
└── geno                 # Genotypes
```

These are organized into the app function
[qtl2shinyApp()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/qtl2shinyApp.R),
which is the basis for the application
[app.R](https://github.com/byandell-sysgen/qtl2shiny/blob/master/inst/qtl2shinyApp/app.R).

Each module is organized in a single file with several components,
following the conventions in
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

Here is a prototype module app with all these components.
The `"id"` is a common identifier across these components.
The user interface function `xxxUI()` might appear in the `sidebar`
or in the body of the `page`, depending on its use.
Note the reactive `result` returned from `xxxServer()`,
which could be input to another module server.

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

Modules in sections below are often referenced in terms of their `server` function,
which sets up the shiny logic.

- [Reading Data](#reading-data)
- [Navigating Panels](#navigating-panels)
- [Downloading Files](#downloading-files)

## Reading Data

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
are determined given the `project` with the `setPar` server.
Additional parameters for the viewing window come from the `winPar` server,
which depends on the `peak` and `hotspot` modules.

- `project_df <-` [projectServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/projectApp.R) #
select `project`
- `set_par    <-` [setParServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/setParApp.R) #
select `class` and `subject_model` (depends on `project` information)
- `win_par    <-` [winParServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/winParApp.R) #
Hotspot window parameters (depends on `hotspot` and `peak` module components)


### Phenotype and Other Project Data

Project-specific phenotype data are read in as needed using the following modules 

- `peak_df  <-` [peakReadServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/peakReadApp.R) #
precomputed peaks and summaries
- `pheno_mx <-` [phenoReadServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoReadApp.R) #
raw phenotype data

The `peak_df` depends on `project`, `class` and `subject_model`
through the `project` and `setPar` servers,
and provides data used for the `hotspot` selection,
which further filters the `peak_df` to the selected `hotspot`.

The `pheno` server depends on `project` and `class` and `peak_df`, using the
[phenoNamesServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoNamesApp.R)
server to identify the phenotype names.
Both `peak` and `pheno` servers depend (directly and indirectly, resp.)
on the `hotspot` server to identify the hotspot of interest.

Other `project` data files are read in as needed using the
[read_project()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/readproject.R)
function, which depends on the `project`.
The `kinship_list` has a server since it depends on the reactively chosen chromosome.

- `kinship_list <-` [kinshipServer()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/kinshipApp.R) #
kinship list of LOCO matrices
- `covar_df     <- read_project(project_df(), "covar")` # 
covariate data frame
- `pmap_obj     <- read_project(project_df(), "pmap")` #
physical map object with list of markers and `Mbp` positions
- `allele_info  <- read_project(project_df(), "allele")` #
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

## Navigating Panels

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
It depends on the `project` and `setPar` modules.

and 
`peak_df` and `pmap_obj` objects, as well as the
This panel has the following modules:

- [hotspot](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/hotspotApp.R) #
Hotspot and Phenotype Panel
  - [hotspotData](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/hotspotDataApp.R) #
summary or plot of `peak_df` hotspots, select hotspot
  - [hotspotTable](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/hotspotTableApp.R) #
Table of hotspots
  - [hotspotPlot](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/hotspotPlotApp.R) #
Plot of hotspot
  - [peak](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/peakApp.R) #
Peak panel
  - [pheno](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoApp.R) #
Phenotype panel

The phenotype panel, subsumed in the hotspot panel,
consists of the following six modules:

- [pheno](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoApp.R) #
Phenotype Panel
  - [phenoRead](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoReadApp.R) #
Create phenotype object and summary
  - [phenoNames](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoNamesApp.R) #
Select phenotype names
  - [phenoData](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoDataApp.R) #
Filter phenotypes by names and transform by `rankZ`
  - [phenoTable](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoTableApp.R) #
Table of phenotypes
  - [phenoPlot](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/phenoPlotApp.R) #
Plot of phenotypes

Interleaved with `hotspot` panel is the `peak` panel

- [peak](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/peakApp.R) #
Peaks filtered by selected `hotspot`
  - [peakRead](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/peakReadApp.R) #
Read peak dataframe

### Scan Panel

The `scan` panel performs allele-based scan and SNP association,
and shows genes and exons in the selected hotspot.
It consists of the following components:

- [scan](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/scanApp.R) #
Scan by Alleles and SNPs Panel
  - [scanData](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/scanDataApp.R) #
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

- [mediate](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/mediateApp.R) #
Mediation Panel
  - [mediateData](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/mediateDataApp.R) #
Run mediation
  - [mediatePlot](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/mediatePlotApp.R) #
Plot mediation results
  - [triad](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/triadApp.R) #
Plot D-M-T triad scatterplots

### SDP Pattern Panel

The pattern panel examines the strain distribution pattern (`SDP`)
in a variety of ways.
It consists of the following modules:

- [pattern](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/patternApp.R) #
SDP Pattern Panel
  - [snpPattern](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/snpPatternApp.R) #
SNP Pattern Scan plot and summary
    - [snpFeature](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/snpFeatureApp.R) #
SNP Features (Pattern and Consequence)
  - [patternData](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/patternDataApp.R) #
SDP Scans summary
  - [patternPlot](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/patternPlotApp.R) #
SDP Scans plot

### Genotype Panel

Finally, the `geno` panel shows the allele, allele pair and SDP genotypes
for a selected genome location.

- [geno](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/genoApp.R) #
Genotype and Effects Panel
  - [genoData](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/genoDataApp.R) #
Genotypes table
  - [genoEffect](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/genoApp.R) #
Genotype Effects plot and table

### Utility Modules

The following utility modules are used for several other modules

- [snpList](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/snpListApp.R) #
calculate SNP objects, including SNP scan
- [dipPar](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/dipParApp.R) #
input parameters needed across SNP and SDP modules

## Downloading Files

Downloading files is accomplished by going to a particular view in a panel.
One
[downloadApp()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/downloadApp.R)
enables the user to download a spreadsheet (`CSV`)
or a plot (`PNG` or `PDF`).
The idea is that, at any time, each panel displays one plot
or one table (summary) that could be downloaded.
The panel server function returns a `result` with reactive components
for a plot and/or table.
**Not fully implemented yet**.

This is somewhat analagous to what was done in
[foundrShiny](https://github.com/AttieLab-Systems-Genetics/foundrShiny)
(see [foundrShiny/R/downloadApp.R](https://github.com/AttieLab-Systems-Genetics/foundrShiny/blob/main/R/downloadApp.R))
and
[qtlApp](https://github.com/AttieLab-Systems-Genetics/qtlApp)
(see [qtlApp/R/modules/downloadApp.R](https://github.com/AttieLab-Systems-Genetics/qtlApp/blob/master/fs-reorg/R/modules/downloadApp.R)).
Here is my current thinking

- each panel returns a
[reactiveValue](https://mastering-shiny.org/reactivity-objects.html)
with element `download`
  - this identifies the currently viewed `plot` or `table` and a `filename`
- these returns are collected into another `reactiveValue` `DL`
- pass `DL` and the current panel (`input$panel`) to `downloadServer()`

```
DL <- shiny::reactiveValues()
DL$hotspot <- hotspotServer()
DL$scan    <- scanServer()
DL$mediate <- mediateServer()
DL$pattern <- patternServer()
DL$geno    <- genoServer()
downloadServer("download", DL, input$panel)
```

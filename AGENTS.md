# qtl2shiny — Project Memory

## Overview

**qtl2shiny** is an R package providing an interactive Shiny web interface for QTL (Quantitative Trait Loci) fine mapping analysis using the R/qtl2 framework. It lets researchers interactively explore genome scans, SNP associations, genotype effects, mediation analysis, and hotspot detection across multiple phenotypes and experimental projects.

- **Version**: 1.5.9
- **License**: GPL-3
- **Maintainer**: Brian S. Yandell (brian.yandell@wisc.edu), UW-Madison
- **Repo**: https://github.com/byandell-sysgen/qtl2shiny
- **Install**: `pak::pak("byandell-sysgen/qtl2shiny")` (use `pak::pak()` for all GitHub installs; `devtools::install_github()` is deprecated)

---

## Project Structure

```
R/                      # 90+ R source files (Shiny modules and utilities)
inst/
  qtl2shinyApp/
    app.R               # Standalone Shiny app launcher
    projects.csv        # Project registry (project | taxa | directory)
    about.md            # App "About" page
  doc/
    module.md           # Module organization guide
    walkthrough.md      # Recent UI refactoring notes
    Recla.md / .Rmd     # Recla dataset tutorial
vignettes/
  UserGuide.Rmd         # End-user guide
  DeveloperGuide.Rmd    # Data structures, workflow, development guide
  qtl2shinyData.Rmd     # Data preparation guide
man/                    # Roxygen2-generated documentation
```

---

## Running the App

```r
# Place app.R alongside qtl2shinyData/ directory, then:
shiny::runApp("app.R")
```

The app reads `qtl2shinyData/projects.csv` to discover available projects, then calls:
```r
ui     <- qtl2shinyUI("qtl2shiny")
server <- function(input, output, session) {
  qtl2shinyServer("qtl2shiny", projects_df)
  session$allowReconnect(TRUE)
}
shiny::shinyApp(ui, server)
```

---

## Data Organization

```
qtl2shinyData/
  projects.csv              # Project registry
  CCmouse/                  # Taxa directory
    allele_info.rds
    taxa_info.rds
    cc-variants.sqlite       # Structural variants DB
    mouse_genes_mgi.sqlite   # Gene annotations DB
    query_genes.rds          # Gene query function
    query_variants.rds       # Variant query function
    Recla/                   # One project directory
      genoprob/              # Genotype probabilities (FST format)
      kinship.rds            # LOCO kinship matrices
      pmap.rds               # Physical map
      covar.rds              # Covariates
      pheno_data.rds         # Phenotype matrix (individuals × phenotypes)
      analyses.rds           # Analysis metadata
      peaks.rds              # Precomputed QTL peaks
      hotspot.rds            # Hotspot object
      query_mrna.rds         # mRNA query function
      query_probs.rds        # Probability query function
```

---

## Architecture

### Shiny Module Pattern

Each analysis feature lives in `R/xxxApp.R` with this standard structure:

```r
xxxApp()      # Standalone demo/test app
xxxServer(id) # Server logic; returns reactive result
xxxInput(id)  # Sidebar input UI (optional)
xxxUI(id)     # Configuration UI (optional)
xxxOutput(id) # Output display UI
```

All modules use `shiny::moduleServer()` with `NS()` namespacing.

### UI Framework

- **bslib** (Bootstrap 5): `page_navbar()`, `page_sidebar()`, `nav_panel()`, `card()`, `layout_sidebar()`
- Main panels: Hotspots & Phenotypes, Allele & SNP Scans, Patterns, Genotypes, Mediation

### Main Module Hierarchy

```
qtl2shinyApp
├── Hotspots & Phenotypes  → hotspot*, pheno*
├── Allele & SNP Scans     → scan*, snpList, snpGene, snpTable, snpPlot
├── Pattern Analysis       → pattern*, snpPattern
├── Genotypes              → geno*, genoEffect
├── Mediation              → mediate*
└── Support                → project, setPar, winPar, probs, kinship, download
```

---

## Key Dependencies

| Category | Packages |
|---|---|
| QTL2 ecosystem | `qtl2`, `qtl2ggplot`, `qtl2mediate`, `qtl2pattern`, `qtl2fst` |
| Shiny / UI | `shiny`, `bslib`, `DT`, `plotly`, `downr` |
| Data handling | `dplyr`, `tidyr`, `data.table`, `fst`, `RSQLite` |
| Visualization | `ggplot2`, `GGally`, `RColorBrewer`, `plotly` |
| GitHub packages | `byandell-sysgen/qtl2ggplot`, `byandell-sysgen/qtl2pattern`, `byandell-sysgen/qtl2mediate`, `byandell-sysgen/intermediate`, `byandell/downr` — install with `pak::pak("user/repo")` |

---

## Development Notes

- **Docs for new contributors**: Start with `inst/doc/module.md` and `vignettes/DeveloperGuide.Rmd`
- **Recent refactoring**: Sidebar inputs unified—phenotype and scan window selection consolidated into `snpListInput`; see `inst/doc/walkthrough.md`
- **Hotspot S3 class**: `R/hotspot.R` defines `cbind`, `subset`, and `summary` methods
- **Phenotype transform**: `rankZ()` / `pheno_rankz()` for rank-Z normalization
- **Known gaps**: mRNA integration incomplete; multi-taxa handling needs refinement; SNP/gene action panel needs rework
- **downr reinstall**: If you see `lazy-load database ... downr.rdb is corrupt`, reinstall with `pak::pak("byandell/downr")`

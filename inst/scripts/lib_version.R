## Simple script to get package versions across installed libraries.

# Write package names and versions by library into CSV.
out <-   lapply(
  .libPaths(),
  function(x)data.frame(installed.packages(x))[,c(1,3)])
names(out) <- .libPaths()
out <- dplyr::bind_rows(out, .id = "Library")
out <- tidyr::pivot_wider(out, names_from = "Library", values_from = "Version")
write.csv(out, "lib_version.csv")

# Read CSV and filter on packages.
lib_version <- read.csv("inst/lib_version.csv")
lib_version[[1]] <- NULL
packs <- c("assertthat", "dplyr", "DT", "fst", "gdata", "GGally", "ggplot2",
           "intermediate", "plotly", "purrr", "qtl2", "qtl2ggplot",
           "qtl2mediate", "qtl2pattern", "rlang", "RSQLite", "shiny",
           "shinydashboard", "stringr", "tidyr", "tools", "utils")
names(lib_version)[-1] <- c("byandell", "local.lib", "usr.lib")
dplyr::filter(lib_version, Package %in% packs)

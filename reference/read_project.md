# Read Project Data

Read Project Data

## Usage

``` r
read_project(
  project_df,
  dataname,
  folder = NULL,
  columns = NULL,
  rownames = TRUE,
  ...,
  filetype,
  legacy = FALSE
)
```

## Arguments

- project_df:

  table of project information

- dataname:

  name of data object to read

- folder:

  folder containing data if not \`NULL\`

- columns:

  columns to select from data object

- rownames:

  row names to filter from data object (all if `TRUE`)

- ...:

  additional parameters

- filetype:

  type of file (one of c("fst","rds","csv"))

- legacy:

  look in \`legacy\` folder for old data if \`TRUE\`

## Value

data frame with `columns` and `rownames`.

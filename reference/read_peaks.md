# Read Peaks for Phenotypes by Class

Read Peaks for Phenotypes by Class

Create Peaks Directory with Phenotypes by Class

## Usage

``` r
read_peaks(project_df, class = NULL, subject_model = NULL, legacy = FALSE)

create_peak_class(project_df, force = FALSE)
```

## Arguments

- project_df:

  data frame with project information

- class:

  name of class

- legacy:

  use legacy data if \`TRUE\`

- force:

  force creation if \`TRUE\`

- ...:

  additional arguments

## Value

dataframe of peaks

character list of created RDS files

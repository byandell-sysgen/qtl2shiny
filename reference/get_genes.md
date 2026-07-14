# Get gene in region

Get gene using `query_genes`; see
[`create_gene_query_func`](https://rdrr.io/pkg/qtl2/man/create_gene_query_func.html).

## Usage

``` r
get_genes(chr_id, start, end, gene_tbl = query_genes(chr_id, start, end))
```

## Arguments

- chr_id:

  chromosome identifier

- start:

  start position in Mbp

- end:

  end position in Mbp

- gene_tbl:

  table of genes from user supplied `query_genes`; see
  [`create_gene_query_func`](https://rdrr.io/pkg/qtl2/man/create_gene_query_func.html)

## Value

object of class `feature_tbl`.

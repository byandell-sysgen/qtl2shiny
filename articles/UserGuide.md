# qtl2shiny UserGuide

This document concerns a suite of packages complementary to
[R/qtl2](http://www.rqtl.org/qtl2) that extend capabilities in terms of
plotting, fast data access, deeper probe of SNPs and structural
variants, and a user-friendly interface for deep investigation of small
intervals.

The QTL2 Shiny Interface can be invoked from the Shiny server
<http://www.statlab.wisc.edu/shiny/qtl2shiny>. We demonstrate with a
publicly available dataset from Recla (see
<https://github.com/rqtl/qtl2data>). However, this Shiny server is set
up with project-specific login and password protection for data that has
not been published, using [server side Apache
htaccess](https://httpd.apache.org/docs/current/howto/ssi.html).

## Strategy to Study Small Interval

Here is a strategy to look at multiple traits in a small interval. These
are rough and ready notes that will be smoothed out in time.

- use analyses table (or other approach) to identify region of interest
  - say 1-5Mb window
- identify set of traits in region
- estimate allele effects at peak for each trait
  - useful plots?
- find best SNP(s) and pattern(s)
  - identify SDP and pattern for each
  - useful plots?
- do same for full effects
  - 36 diplotypes
  - 3-level SNPs

Some challenges

- don’t have useful thresholds yet
- would like to think about inference across related patterns
- multiple peaks in small region
- pleiotropy vs close linkage

How to incorporate this into shiny tool?

- quick computation: limit size of region
- do we have to run genome scan?
  - or just consider flanking markers for each peak?

## QTL2 Shiny Interface

Information will appear here about how the Shiny Interface works.

## R/qtl2 companion packages

The popular [R/qtl](http://www.rqtl.org) package for gene mapping has
been redeveloped for high volume, multi-parent systems genetics data as
[R/qtl2](http://www.rqtl.org/qtl2). This new package is still in a state
of flux, so please work with us to improve.

This document describes companion packages that are used to build the
Shiny server:

- [R/qtl2ggplot](https://github.com/byandell/qtl2ggplot)
- [R/qtl2feather](https://github.com/byandell/qtl2feather)
- [R/qtl2pattern](https://github.com/byandell/qtl2pattern)
- [R/qtl2shiny](https://github.com/byandell/qtl2shiny)

The three other packages can be employed on their own separate from the
server. In particular,
[R/qtl2ggplot](https://github.com/byandell/qtl2ggplot) provides full
capability for `ggplot2` plot objects. The
[R/qtl2feather](https://github.com/byandell/qtl2feather) package
transforms large, slow `calc_genoprob` objects central to
[R/qtl2](http://www.rqtl.org/qtl2) into a set of `feather` databases for
quick access. See each package for its own documentation.

There is another package that is more specialized and still need
documentation: [R/CausalMST](https://github.com/byandell/CausalMST)
concerns Causal Model Selection Tests, which are used in conjunction
with mediation to formally infer causal relationship between a phenotype
and other phenotypes that co-map. Plan is to try to combine this with
[R/intermediate](https://github.com/churchill-lab/intermediate).

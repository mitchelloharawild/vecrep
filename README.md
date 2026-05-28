
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vecrep

<!-- badges: start -->

[![R-CMD-check](https://github.com/mitchelloharawild/vecrep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mitchelloharawild/vecrep/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/vecrep)](https://CRAN.R-project.org/package=vecrep) -->
<!-- badges: end -->

vecrep provides `rep_altrep()`, an ALTREP alternative to `base::rep()`
for numeric vectors. Rather than duplicating data immediately, it stores
a compact reference to the original vector and only expands it if a
write forces materialisation. This makes it well-suited to vectors with
many repetitions, especially if the reference vector is a regular
sequence represented with ALTREP. ALTREP sequences can be combined with
ALTREP replicates to create repeating regular sub-sequences.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("mitchelloharawild/vecrep")
```

## Usage

``` r
library(vecrep)

x <- as.numeric(1:5)

# Create a repeated vector — no extra allocation
y <- rep_altrep(x, times = 4)

length(y)   # 20
#> [1] 20
y[1:10]     # reads directly from x
#>  [1] 1 2 3 4 5 1 2 3 4 5
sum(y)      # aggregates stay lazy too
#> [1] 60
```

Read operations (`[`, `sum()`, `mean()`, `anyNA()`) work directly on the
parent vector without expanding it. The full vector is only materialised
on the first write, and copy-on-write ensures the parent is never
modified.

``` r
parent <- as.numeric(1:5)
y <- rep_altrep(parent, 3)

y[1] <- 999   # triggers expansion

parent        # unchanged
#> [1] 1 2 3 4 5
y[1:6]
#> [1] 999   2   3   4   5   1
```

## Caveats

- **Numeric only.** Integer, character, and list inputs are rejected.
- **Serialisation materialises.** `saveRDS()` expands the vector and
  emits a warning; the round-tripped object is correct but no longer
  compact.
- **Sorting expands the internal buffer**, though the ALTREP shell
  persists.

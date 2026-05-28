# vecrep

vecrep provides [`rep_altrep()`](reference/rep_altrep.md), an ALTREP
alternative to [`base::rep()`](https://rdrr.io/r/base/rep.html) that
works with most vector types. Rather than duplicating data immediately,
it stores a compact reference to the original vector and only expands it
if a write forces materialisation. This makes it well-suited to vectors
with many repetitions, especially if the reference vector is a regular
sequence represented with ALTREP. ALTREP sequences can be combined with
ALTREP replicates to create repeating regular sub-sequences.

Several common operations are accelerated by working directly on the
reference vector rather than the full expanded result:

- [`sum()`](https://rdrr.io/r/base/sum.html): computed on the reference
  vector and scaled by the number of replications.
- [`min()`](https://rdrr.io/r/base/Extremes.html) /
  [`max()`](https://rdrr.io/r/base/Extremes.html): dispatched to the
  reference vector without scanning replicated values.
- [`is.na()`](https://rdrr.io/r/base/NA.html) /
  [`anyNA()`](https://rdrr.io/r/base/NA.html): NA checks are performed
  on the reference vector and the result tiled, avoiding a full scan of
  repeated elements.
- [`sort()`](https://rdrr.io/r/base/sort.html): if the reference vector
  is already sorted, and the vector is only replicated by element
  (i.e. `each > 1` but `times == 1`), then the result is known to be
  sorted.

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

Read operations (`[`, [`sum()`](https://rdrr.io/r/base/sum.html),
[`mean()`](https://rdrr.io/r/base/mean.html),
[`anyNA()`](https://rdrr.io/r/base/NA.html)) work directly on the parent
vector without expanding it. The full vector is only materialised on the
first write, and copy-on-write ensures the parent is never modified.

``` r

parent <- as.numeric(1:5)
y <- rep_altrep(parent, 3)

y[1] <- 999   # triggers expansion

parent        # unchanged
#> [1] 1 2 3 4 5
y[1:6]
#> [1] 999   2   3   4   5   1
```

The `each` argument repeats each element in turn before moving to the
next, matching the behaviour of `base::rep(..., each = n)`:

``` r

x <- as.numeric(1:3)

# Each element repeated 3 times: 1 1 1 2 2 2 3 3 3
rep_altrep(x, each = 3)
#> [1] 1 1 1 2 2 2 3 3 3

# times and each can be combined — each is applied first, then times repeats the result
rep_altrep(x, times = 2, each = 3)
#>  [1] 1 1 1 2 2 2 3 3 3 1 1 1 2 2 2 3 3 3
```

## Supported types

[`rep_altrep()`](reference/rep_altrep.md) supports most vector types:

``` r

# integer
rep_altrep(1L:3L, 3L)
#> [1] 1 2 3 1 2 3 1 2 3

# logical
rep_altrep(c(TRUE, FALSE, NA), 2L)
#> [1]  TRUE FALSE    NA  TRUE FALSE    NA

# complex
rep_altrep(c(1+1i, 2+2i), 4L)
#> [1] 1+1i 2+2i 1+1i 2+2i 1+1i 2+2i 1+1i 2+2i

# raw
rep_altrep(as.raw(c(0x01, 0x02, 0x03)), 2L)
#> [1] 01 02 03 01 02 03

# character
rep_altrep(c("foo", "bar", "baz"), 3L)
#> [1] "foo" "bar" "baz" "foo" "bar" "baz" "foo" "bar" "baz"

# list
rep_altrep(list(1L, "a", TRUE), 2L)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] "a"
#> 
#> [[3]]
#> [1] TRUE
#> 
#> [[4]]
#> [1] 1
#> 
#> [[5]]
#> [1] "a"
#> 
#> [[6]]
#> [1] TRUE
```

Classed vectors such as `factor`, `Date`, and `POSIXct` are handled
transparently. The class and relevant attributes (e.g. `levels` for
factors) are preserved on the ALTREP object without forcing
materialisation, so S3 dispatch works as expected:

``` r

# factor: levels preserved without expansion
f <- rep_altrep(factor(c("cat", "dog", "cat")), 3L)
class(f)
#> [1] "factor"
levels(f)
#> [1] "cat" "dog"
table(f)
#> f
#> cat dog 
#>   6   3

# Date
d <- rep_altrep(as.Date("2024-01-01") + 0:2, 2L)
class(d)
#> [1] "Date"
d
#> [1] "2024-01-01" "2024-01-02" "2024-01-03" "2024-01-01" "2024-01-02"
#> [6] "2024-01-03"

# POSIXct
p <- rep_altrep(as.POSIXct("2024-01-01") + 0:2, 2L)
class(p)
#> [1] "POSIXct" "POSIXt"
```

Named vectors keep their names lazily — the names are themselves stored
as an ALTREP character vector rather than being eagerly expanded:

``` r

x <- c(a = 1.0, b = 2.0, c = 3.0)
y <- rep_altrep(x, 3L)
names(y)
#> [1] "a" "b" "c" "a" "b" "c" "a" "b" "c"
```

## Caveats

- Serialisation with [`saveRDS()`](https://rdrr.io/r/base/readRDS.html)
  expands the vector (it is correct but no longer compact).
- [`sort()`](https://rdrr.io/r/base/sort.html) materialises the vector
  if it is not already sorted, the ALTREP API does not provide any
  method for implementing a replicate-aware sorting algorithm.

## Acknowledgements

The initial codebase was adapted from Gabriel Becker’s
[vectorwindow](https://github.com/ALTREP-examples/vectorwindow/)
example, presented in his Bioconductor Developers Forum talk
([YouTube](https://www.youtube.com/watch?v=8i7ziLqsE2s)).

Substantial proportions of this package were developed in tandem with
[Claude Sonnet 4.6](https://www.anthropic.com/claude) (Anthropic). All
code has been reviewed and guided by humans.

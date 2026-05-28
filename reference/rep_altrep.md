# Repeat a vector using ALTREP

Creates a repeated vector backed by an ALTREP representation, avoiding
materialisation of the full vector in memory until necessary.

## Usage

``` r
rep_altrep(x, times = 1L, each = 1L)
```

## Arguments

- x:

  A vector to repeat. Must be one of: double, integer, logical, complex,
  raw, character, or list (including classed variants thereof).

- times:

  A single positive integer giving the number of times to repeat the
  whole (each-expanded) pattern. Defaults to `1L`.

- each:

  A single positive integer giving the number of times each element is
  repeated before moving to the next. Defaults to `1L`.

## Value

An ALTREP vector of the same type and class as `x`, with length
`length(x) * each * times`.

## Details

Supported types: double, integer, logical, complex, raw, character, and
list. Classed vectors (e.g. `factor`, `Date`, `POSIXct`) are handled
transparently: the class attribute is preserved on the returned object
so S3 dispatch continues to work without forcing materialisation.

`times` and `each` can be combined freely, matching the behaviour of
[`base::rep()`](https://rdrr.io/r/base/rep.html): `each` replicates
individual elements first, then `times` repeats the resulting pattern.
Providing only `times` is equivalent to `rep(x, times = times)`;
providing only `each` is equivalent to `rep(x, each = each)`.

## Examples

``` r
rep_altrep(letters[1:4], times = 2)
#> [1] "a" "b" "c" "d" "a" "b" "c" "d"
rep_altrep(letters[1:4], each = 2)
#> [1] "a" "a" "b" "b" "c" "c" "d" "d"
rep_altrep(letters[1:4], times = 2, each = 3)
#>  [1] "a" "a" "a" "b" "b" "b" "c" "c" "c" "d" "d" "d" "a" "a" "a" "b" "b" "b" "c"
#> [20] "c" "c" "d" "d" "d"
rep_altrep(1L:4L, each = 2L)
#> [1] 1 1 2 2 3 3 4 4
rep_altrep(c(TRUE, FALSE, NA), each = 2L, times = 3L)
#>  [1]  TRUE  TRUE FALSE FALSE    NA    NA  TRUE  TRUE FALSE FALSE    NA    NA
#> [13]  TRUE  TRUE FALSE FALSE    NA    NA
rep_altrep(factor(c("a", "b", "c")), each = 2L)
#> [1] a a b b c c
#> Levels: a b c
rep_altrep(as.Date("2024-01-01") + 0:2, each = 2L)
#> [1] "2024-01-01" "2024-01-01" "2024-01-02" "2024-01-02" "2024-01-03"
#> [6] "2024-01-03"
rep_altrep(c("foo", "bar"), times = 5L)
#>  [1] "foo" "bar" "foo" "bar" "foo" "bar" "foo" "bar" "foo" "bar"
rep_altrep(list(1, "a", TRUE), each = 2L, times = 2L)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 1
#> 
#> [[3]]
#> [1] "a"
#> 
#> [[4]]
#> [1] "a"
#> 
#> [[5]]
#> [1] TRUE
#> 
#> [[6]]
#> [1] TRUE
#> 
#> [[7]]
#> [1] 1
#> 
#> [[8]]
#> [1] 1
#> 
#> [[9]]
#> [1] "a"
#> 
#> [[10]]
#> [1] "a"
#> 
#> [[11]]
#> [1] TRUE
#> 
#> [[12]]
#> [1] TRUE
#> 
```

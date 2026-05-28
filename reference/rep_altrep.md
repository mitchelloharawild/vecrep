# Repeat a numeric vector using ALTREP

Creates a repeated numeric vector backed by an ALTREP representation,
avoiding materialisation of the full vector in memory.

## Usage

``` r
rep_altrep(x, times)
```

## Arguments

- x:

  A numeric vector to repeat.

- times:

  A single positive integer giving the number of times to repeat `x`.

## Value

An ALTREP numeric vector of length `length(x) * times`.

## Examples

``` r
rep_altrep(1:5, 3)
#> Error in rep_altrep(1:5, 3): is.double(x) is not TRUE
```

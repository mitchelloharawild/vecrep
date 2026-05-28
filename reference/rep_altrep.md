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
rep_altrep(rnorm(5), 3)
#>  [1] -1.400043517  0.255317055 -2.437263611 -0.005571287  0.621552721
#>  [6] -1.400043517  0.255317055 -2.437263611 -0.005571287  0.621552721
#> [11] -1.400043517  0.255317055 -2.437263611 -0.005571287  0.621552721
```

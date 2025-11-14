# Assert survival matrix

Asserts if the given input matrix is a (discrete) survival probabilities
matrix using [Rcpp](https://CRAN.R-project.org/package=Rcpp). The
following checks are performed:

1.  All values are probabilities, i.e. \\S(t) \in \[0,1\]\\

2.  Column names correspond to time-points and should therefore be
    coercable to `numeric` and increasing

3.  Per row/observation, the survival probabilities decrease
    non-strictly, i.e. \\S(t) \ge S(t+1)\\

## Usage

``` r
assert_surv_matrix(x)
```

## Arguments

- x:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  A matrix of (predicted) survival probabilities. Rows are observations,
  columns are (increasing) time points.

## Value

if the assertion fails an error occurs, otherwise `NULL` is returned
invisibly.

## Examples

``` r
x = matrix(data = c(1,0.6,0.4,0.8,0.8,0.7), nrow = 2, ncol = 3, byrow = TRUE)
colnames(x) = c(12, 34, 42)
x
#>       12  34  42
#> [1,] 1.0 0.6 0.4
#> [2,] 0.8 0.8 0.7

assert_surv_matrix(x)
```

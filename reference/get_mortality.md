# Calculate the expected mortality risks from a survival matrix

Many methods can be used to reduce a discrete survival distribution
prediction (i.e. matrix) to a relative risk / ranking prediction, see
Sonabend et al. (2022).

This function calculates a relative risk score as the sum of the
predicted cumulative hazard function, also called **ensemble/expected
mortality**. This risk score can be loosely interpreted as the expected
number of deaths for patients with similar characteristics, see Ishwaran
et al. (2008) and has no model or survival distribution assumptions.

## Usage

``` r
get_mortality(x)
```

## Arguments

- x:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  A survival matrix where rows are the (predicted) observations and
  columns the time-points. For more details, see
  [assert_surv_matrix](https://mlr3proba.mlr-org.com/reference/assert_surv_matrix.md).

## Value

a `numeric` vector of the mortality risk scores, one per row of the
input survival matrix.

## References

Sonabend, Raphael, Bender, Andreas, Vollmer, Sebastian (2022). “Avoiding
C-hacking when evaluating survival distribution predictions with
discrimination measures.” *Bioinformatics*. ISSN 1367-4803,
[doi:10.1093/BIOINFORMATICS/BTAC451](https://doi.org/10.1093/BIOINFORMATICS/BTAC451)
,
<https://academic.oup.com/bioinformatics/advance-article/doi/10.1093/bioinformatics/btac451/6640155>.

Ishwaran, Hemant, Kogalur, B U, Blackstone, H E, Lauer, S M, others
(2008). “Random survival forests.” *The Annals of applied statistics*,
**2**(3), 841–860.

## Examples

``` r
n = 10 # number of observations
k = 50 # time points

# Create the matrix with random values between 0 and 1
mat = matrix(runif(n * k, min = 0, max = 1), nrow = n, ncol = k)

# transform it to a survival matrix
surv_mat = t(apply(mat, 1L, function(row) sort(row, decreasing = TRUE)))
colnames(surv_mat) = 1:k # time points

# get mortality scores (the larger, the more risk)
mort = get_mortality(surv_mat)
mort
#>  [1] 62.23396 46.07522 42.49314 61.79209 45.36408 45.04310 59.98554 46.09549
#>  [9] 40.65044 54.34432
```

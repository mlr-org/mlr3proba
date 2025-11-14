# Convert to a Survival Prediction

Convert object to a
[PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md).

## Usage

``` r
as_prediction_surv(x, ...)

# S3 method for class 'PredictionSurv'
as_prediction_surv(x, ...)

# S3 method for class 'data.frame'
as_prediction_surv(x, ...)
```

## Arguments

- x:

  (any)  
  Object to convert.

- ...:

  (any)  
  Additional arguments.

## Value

[PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md).

## Examples

``` r
library(mlr3)
task = tsk("rats")
learner = lrn("surv.coxph")
learner$train(task)
p = learner$predict(task)

# convert to a data.table
tab = as.data.table(p)

# convert back to a Prediction
as_prediction_surv(tab)
#> 
#> ── <PredictionSurv> for 300 observations: ──────────────────────────────────────
#>  row_ids time status      crank         lp     distr
#>        1  101  FALSE  0.3862629  0.3862629 <list[1]>
#>        2   49   TRUE -0.4190332 -0.4190332 <list[1]>
#>        3  104  FALSE -0.4190332 -0.4190332 <list[1]>
#>      ---  ---    ---        ---        ---       ---
#>      298   92  FALSE -1.8607954 -1.8607954 <list[1]>
#>      299  104  FALSE -2.6660915 -2.6660915 <list[1]>
#>      300  102  FALSE -2.6660915 -2.6660915 <list[1]>
```

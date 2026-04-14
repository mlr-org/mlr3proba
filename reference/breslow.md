# Survival probabilities using Breslow's estimator

Helper function to compose a survival distribution (or cumulative
hazard) from the relative risk predictions (linear predictors, `lp`) of
a **proportional hazards** model (e.g. a Cox-type model).

## Usage

``` r
breslow(times, status, lp_train, lp_test, eval_times = NULL, type = "surv")
```

## Arguments

- times:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Vector of times (train set).

- status:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Vector of status indicators (train set). For each observation in the
  train set, this should be 0 (alive/censored) or 1 (dead).

- lp_train:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Vector of linear predictors (train set). These are the relative score
  predictions (\\lp = \hat{\beta}X\_{train}\\) from a proportional
  hazards model on the train set.

- lp_test:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Vector of linear predictors (test set). These are the relative score
  predictions (\\lp = \hat{\beta}X\_{test}\\) from a proportional
  hazards model on the test set.

- eval_times:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Vector of times to compute survival probabilities. If `NULL`
  (default), the unique and sorted `times` from the train set will be
  used, otherwise the unique and sorted `eval_times`.

- type:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Type of prediction estimates. Default is `surv` which returns the
  survival probabilities \\S_i(t)\\ for each test observation \\i\\. If
  `cumhaz`, the function returns the estimated cumulative hazards
  \\H_i(t)\\.

## Value

a `matrix` (obs x times). Number of columns is equal to `eval_times` and
number of rows is equal to the number of test observations (i.e. the
length of the `lp_test` vector). Depending on the `type` argument, the
matrix can have either survival probabilities (0-1) or cumulative hazard
estimates (0-`Inf`).

## Details

We estimate the survival probability of individual \\i\\ (from the test
set), at time point \\t\\ as follows: \$\$S_i(t) = e^{-H_i(t)} =
e^{-\hat{H}\_0(t) \times e^{lp_i}}\$\$

where:

- \\H_i(t)\\ is the cumulative hazard function for individual \\i\\

- \\\hat{H}\_0(t)\\ is Breslow's estimator for the **cumulative baseline
  hazard**. Estimation requires the training set's `times` and `status`
  as well the risk predictions (`lp_train`).

- \\lp_i\\ is the risk prediction (linear predictor) of individual \\i\\
  on the test set.

Breslow's approach uses a non-parametric maximum likelihood estimation
of the cumulative baseline hazard function:

\$\$\hat{H}\_0(t) = \sum\_{i=1}^n{\frac{I(T_i \le t)\delta_i}
{\sum\nolimits\_{j \in R_i}e^{lp_j}}}\$\$

where:

- \\t\\ is the vector of time points (unique and sorted, from the train
  set)

- \\n\\ is number of events (train set)

- \\T\\ is the vector of event times (train set)

- \\\delta\\ is the status indicator (1 = event or 0 = censored)

- \\R_i\\ is the risk set (number of individuals at risk just before
  event \\i\\)

- \\lp_j\\ is the risk prediction (linear predictor) of individual \\j\\
  (who is part of the risk set \\R_i\\) on the train set.

We employ **constant interpolation** to estimate the cumulative baseline
hazards, extending from the observed unique event times to the specified
evaluation times (`eval_times`). Any values falling outside the range of
the estimated times are assigned as follows: \$\$\hat{H}\_0(eval\\times
\< min(t)) = 0\$\$ and \$\$\hat{H}\_0(eval\\times \> max(t)) =
\hat{H}\_0(max(t))\$\$

Note that in the rare event of `lp` predictions being `Inf` or `-Inf`,
the resulting cumulative hazard values become `NaN`, which we substitute
with `Inf` (and corresponding survival probabilities take the value of
\\0\\).

For similar implementations, see `gbm::basehaz.gbm()`,
`C060::basesurv()` and `xgboost.surv::sgb_bhaz()`.

## References

Breslow N (1972). “Discussion of 'Regression Models and Life-Tables' by
D.R. Cox.” *Journal of the Royal Statistical Society: Series B*,
**34**(2), 216-217.

Lin, Y. D (2007). “On the Breslow estimator.” *Lifetime Data Analysis*,
**13**(4), 471-480.
[doi:10.1007/s10985-007-9048-y](https://doi.org/10.1007/s10985-007-9048-y)
.

## Examples

``` r
task = tsk("rats")
part = partition(task, ratio = 0.8)

learner = lrn("surv.coxph")
learner$train(task, part$train)
p_train = learner$predict(task, part$train)
p_test = learner$predict(task, part$test)

surv = breslow(times = task$times(part$train), status = task$status(part$train),
               lp_train = p_train$lp, lp_test = p_test$lp)
head(surv)
#>      23 32        34        39        40        45        49        51
#> [1,]  1  1 0.9934716 0.9869102 0.9803373 0.9737725 0.9671354 0.9671354
#> [2,]  1  1 0.9964608 0.9928931 0.9893081 0.9857165 0.9820740 0.9820740
#> [3,]  1  1 0.9964608 0.9928931 0.9893081 0.9857165 0.9820740 0.9820740
#> [4,]  1  1 0.9963199 0.9926106 0.9888840 0.9851510 0.9813657 0.9813657
#> [5,]  1  1 0.9962474 0.9924652 0.9886657 0.9848599 0.9810011 0.9810011
#> [6,]  1  1 0.9996114 0.9992184 0.9988222 0.9984240 0.9980189 0.9980189
#>             53        54        55        62        63        64        65
#> [1,] 0.9671354 0.9603968 0.9536580 0.9536580 0.9536580 0.9467642 0.9467642
#> [2,] 0.9820740 0.9783641 0.9746422 0.9746422 0.9746422 0.9708221 0.9708221
#> [3,] 0.9820740 0.9783641 0.9746422 0.9746422 0.9746422 0.9708221 0.9708221
#> [4,] 0.9813657 0.9775108 0.9736441 0.9736441 0.9736441 0.9696760 0.9696760
#> [5,] 0.9810011 0.9770718 0.9731306 0.9731306 0.9731306 0.9690864 0.9690864
#> [6,] 0.9980189 0.9976049 0.9971881 0.9971881 0.9971881 0.9967589 0.9967589
#>             66        67        68        69        70        71        72
#> [1,] 0.9398717 0.9329700 0.9260397 0.9260397 0.9190422 0.9120344 0.9050772
#> [2,] 0.9669900 0.9631398 0.9592604 0.9592604 0.9553300 0.9513799 0.9474447
#> [3,] 0.9669900 0.9631398 0.9592604 0.9592604 0.9553300 0.9513799 0.9474447
#> [4,] 0.9656960 0.9616979 0.9576702 0.9576702 0.9535900 0.9494903 0.9454065
#> [5,] 0.9650305 0.9609564 0.9568525 0.9568525 0.9526955 0.9485188 0.9443588
#> [6,] 0.9963268 0.9958911 0.9954505 0.9954505 0.9950026 0.9945507 0.9940989
#>             73        74        75        76        77        78        79
#> [1,] 0.8910119 0.8910119 0.8836515 0.8836515 0.8760279 0.8681794 0.8603226
#> [2,] 0.9394461 0.9394461 0.9352373 0.9352373 0.9308610 0.9263373 0.9217901
#> [3,] 0.9394461 0.9394461 0.9352373 0.9352373 0.9308610 0.9263373 0.9217901
#> [4,] 0.9371081 0.9371081 0.9327427 0.9327427 0.9282044 0.9235142 0.9188004
#> [5,] 0.9359067 0.9359067 0.9314610 0.9314610 0.9268396 0.9220640 0.9172650
#> [6,] 0.9931753 0.9931753 0.9926865 0.9926865 0.9921762 0.9916465 0.9911116
#>             80        81        82        83        84        85        86
#> [1,] 0.8523362 0.8361399 0.8361399 0.8361399 0.8275937 0.8275937 0.8189025
#> [2,] 0.9171483 0.9076730 0.9076730 0.9076730 0.9026393 0.9026393 0.8974958
#> [3,] 0.9171483 0.9076730 0.9076730 0.9076730 0.9026393 0.9026393 0.8974958
#> [4,] 0.9139896 0.9041724 0.9041724 0.9041724 0.8989586 0.8989586 0.8936323
#> [5,] 0.9123676 0.9023753 0.9023753 0.9023753 0.8970694 0.8970694 0.8916497
#> [6,] 0.9905632 0.9894361 0.9894361 0.9894361 0.9888331 0.9888331 0.9882137
#>             87        88        89        90        91        92        93
#> [1,] 0.8189025 0.8098767 0.8005769 0.8005769 0.8005769 0.7901305 0.7901305
#> [2,] 0.8974958 0.8921276 0.8865677 0.8865677 0.8865677 0.8802868 0.8802868
#> [3,] 0.8974958 0.8921276 0.8865677 0.8865677 0.8865677 0.8802868 0.8802868
#> [4,] 0.8936323 0.8880747 0.8823199 0.8823199 0.8823199 0.8758207 0.8758207
#> [5,] 0.8916497 0.8859952 0.8801409 0.8801409 0.8801409 0.8735301 0.8735301
#> [6,] 0.9882137 0.9875640 0.9868874 0.9868874 0.9868874 0.9861185 0.9861185
#>             94        95        96        97        98        99       100
#> [1,] 0.7793025 0.7793025 0.7565837 0.7565837 0.7565837 0.7565837 0.7565837
#> [2,] 0.8737361 0.8737361 0.8598546 0.8598546 0.8598546 0.8598546 0.8598546
#> [3,] 0.8737361 0.8737361 0.8598546 0.8598546 0.8598546 0.8598546 0.8598546
#> [4,] 0.8690443 0.8690443 0.8546912 0.8546912 0.8546912 0.8546912 0.8546912
#> [5,] 0.8666385 0.8666385 0.8520448 0.8520448 0.8520448 0.8520448 0.8520448
#> [6,] 0.9853113 0.9853113 0.9835828 0.9835828 0.9835828 0.9835828 0.9835828
#>            101       102       103       104
#> [1,] 0.7445360 0.7205855 0.6836699 0.6702931
#> [2,] 0.8524158 0.8374616 0.8139581 0.8052983
#> [3,] 0.8524158 0.8374616 0.8139581 0.8052983
#> [4,] 0.8470034 0.8315568 0.8073018 0.7983721
#> [5,] 0.8442301 0.8285329 0.8038960 0.7948293
#> [6,] 0.9826463 0.9807415 0.9776856 0.9765398
```

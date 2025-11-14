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
#>      23 32        39        40        45        49        51        53
#> [1,]  1  1 0.9914209 0.9828248 0.9742625 0.9742625 0.9742625 0.9742625
#> [2,]  1  1 0.9965029 0.9929808 0.9894543 0.9894543 0.9894543 0.9894543
#> [3,]  1  1 0.9964546 0.9928842 0.9893094 0.9893094 0.9893094 0.9893094
#> [4,]  1  1 0.9964546 0.9928842 0.9893094 0.9893094 0.9893094 0.9893094
#> [5,]  1  1 0.9963059 0.9925862 0.9888625 0.9888625 0.9888625 0.9888625
#> [6,]  1  1 0.9993776 0.9987490 0.9981177 0.9981177 0.9981177 0.9981177
#>             54        55        61        62        63        64        65
#> [1,] 0.9654557 0.9566726 0.9566726 0.9566726 0.9566726 0.9477082 0.9477082
#> [2,] 0.9858079 0.9821516 0.9821516 0.9821516 0.9821516 0.9783992 0.9783992
#> [3,] 0.9856132 0.9819072 0.9819072 0.9819072 0.9819072 0.9781040 0.9781040
#> [4,] 0.9856132 0.9819072 0.9819072 0.9819072 0.9819072 0.9781040 0.9781040
#> [5,] 0.9850131 0.9811540 0.9811540 0.9811540 0.9811540 0.9771943 0.9771943
#> [6,] 0.9974630 0.9968046 0.9968046 0.9968046 0.9968046 0.9961267 0.9961267
#>             67        68        69        70        71        72        73
#> [1,] 0.9387716 0.9297896 0.9297896 0.9207989 0.9117966 0.9028786 0.8849133
#> [2,] 0.9746375 0.9708351 0.9708351 0.9670072 0.9631520 0.9593107 0.9515033
#> [3,] 0.9742915 0.9704381 0.9704381 0.9665590 0.9626525 0.9587601 0.9508499
#> [4,] 0.9742915 0.9704381 0.9704381 0.9665590 0.9626525 0.9587601 0.9508499
#> [5,] 0.9732256 0.9692149 0.9692149 0.9651782 0.9611136 0.9570645 0.9488378
#> [6,] 0.9954450 0.9947537 0.9947537 0.9940556 0.9933501 0.9926449 0.9912044
#>             74        75        76        77        78        79        80
#> [1,] 0.8849133 0.8756736 0.8756736 0.8661314 0.8563486 0.8563486 0.8368543
#> [2,] 0.9515033 0.9474512 0.9474512 0.9432398 0.9388934 0.9388934 0.9301438
#> [3,] 0.9508499 0.9467447 0.9467447 0.9424784 0.9380757 0.9380757 0.9292134
#> [4,] 0.9508499 0.9467447 0.9467447 0.9424784 0.9380757 0.9380757 0.9292134
#> [5,] 0.9488378 0.9445696 0.9445696 0.9401345 0.9355587 0.9355587 0.9263505
#> [6,] 0.9912044 0.9904529 0.9904529 0.9896691 0.9888571 0.9888571 0.9872131
#>             81        82        83        84        85        86        87
#> [1,] 0.8268923 0.8268923 0.8268923 0.8062381 0.8062381 0.7959255 0.7959255
#> [2,] 0.9256258 0.9256258 0.9256258 0.9161546 0.9161546 0.9113717 0.9113717
#> [3,] 0.9246378 0.9246378 0.9246378 0.9150467 0.9150467 0.9102037 0.9102037
#> [4,] 0.9246378 0.9246378 0.9246378 0.9150467 0.9150467 0.9102037 0.9102037
#> [5,] 0.9215977 0.9215977 0.9215977 0.9116385 0.9116385 0.9066113 0.9066113
#> [6,] 0.9863592 0.9863592 0.9863592 0.9845580 0.9845580 0.9836426 0.9836426
#>             88        89        90        91        92        93        94
#> [1,] 0.7852553 0.7852553 0.7852553 0.7852553 0.7732870 0.7732870 0.7607858
#> [2,] 0.9063841 0.9063841 0.9063841 0.9063841 0.9007417 0.9007417 0.8947924
#> [3,] 0.9051539 0.9051539 0.9051539 0.9051539 0.8994415 0.8994415 0.8934190
#> [4,] 0.9051539 0.9051539 0.9051539 0.9051539 0.8994415 0.8994415 0.8934190
#> [5,] 0.9013706 0.9013706 0.9013706 0.9013706 0.8954438 0.8954438 0.8891969
#> [6,] 0.9826838 0.9826838 0.9826838 0.9826838 0.9815939 0.9815939 0.9804386
#>             95        96        97        98        99       100       101
#> [1,] 0.7607858 0.7347658 0.7347658 0.7347658 0.7347658 0.7347658 0.7211915
#> [2,] 0.8947924 0.8822208 0.8822208 0.8822208 0.8822208 0.8822208 0.8755573
#> [3,] 0.8934190 0.8806944 0.8806944 0.8806944 0.8806944 0.8806944 0.8739509
#> [4,] 0.8934190 0.8806944 0.8806944 0.8806944 0.8806944 0.8806944 0.8739509
#> [5,] 0.8891969 0.8760042 0.8760042 0.8760042 0.8760042 0.8760042 0.8690157
#> [6,] 0.9804386 0.9779763 0.9779763 0.9779763 0.9779763 0.9779763 0.9766595
#>            102       103       104
#> [1,] 0.7076914 0.6668261 0.6522631
#> [2,] 0.8688560 0.8480960 0.8405158
#> [3,] 0.8671698 0.8461672 0.8385003
#> [4,] 0.8671698 0.8461672 0.8385003
#> [5,] 0.8619907 0.8402472 0.8323155
#> [6,] 0.9753269 0.9711441 0.9695959
```

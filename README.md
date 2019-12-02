
# mlr3proba

Package website: [release](https://mlr3proba.mlr-org.com/) |
[dev](https://mlr3proba.mlr-org.com/dev)

Probabilistic Supervised Learning for
**[mlr3](https://github.com/mlr-org/mlr3)**.

[![Build
Status](https://img.shields.io/travis/mlr-org/mlr3proba/master?label=Linux&logo=travis&style=flat-square)](https://travis-ci.org/mlr-org/mlr3proba)
[![cran
checks](https://cranchecks.info/badges/worst/mlr3proba)](https://cran.r-project.org/web/checks/check_results_mlr3proba.html)

[![CRAN Status
Badge](https://www.r-pkg.org/badges/version-ago/mlr3proba)](https://cran.r-project.org/package=mlr3proba)
[![codecov](https://codecov.io/gh/mlr-org/mlr3proba/branch/master/graph/badge.svg)](https://codecov.io/gh/mlr-org/mlr3proba)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Dependencies](https://tinyverse.netlify.com/badge/mlr3proba)](https://cran.r-project.org/package=mlr3proba)

## What is mlr3proba ?

**mlr3proba** is a probabilistic supervised learning (PSL) toolkit for
machine learning in R utilising the
**[mlr3](https://github.com/mlr-org/mlr3)** package. Probabilistic
supervised learning is a field of supervised machine learning in which
probability distributions are predicted. The most common variant of this
is survival analysis, where the task of interest is to predict an
individual’s survival curve. Other forms of PSL include density
estimation and probabilistic regression. To-date, PSL toolkits in R has
been limited to Bayesian simulation packages, but **mlr3proba** hopes to
change this.

## Installation

Install the last release from CRAN:

``` r
install.packages("mlr3proba")
```

Install the development version from
GitHub:

``` r
remotes::install_github("mlr-org/mlr3proba")
```

## Survival Analysis

### Survival Learners

| ID                                                                                              | Learner                                                | Package                                                                    |
| :---------------------------------------------------------------------------------------------- | :----------------------------------------------------- | :------------------------------------------------------------------------- |
| [surv.blackboost](https://mlr3proba.mlr-org.com/reference/LearnerSurvBlackboost.html)           | Gradient Boosting with Regression Trees                | [mboost](https://cran.r-project.org/web/packages/mboost/)                  |
| [surv.coxph](https://mlr3proba.mlr-org.com/reference/LearnerSurvCoxPH.html)                     | Cox Proportional Hazards                               | [survival](https://cran.r-project.org/web/packages/survival)               |
| [surv.cvglmnet](https://mlr3proba.mlr-org.com/reference/LearnerSurvCVGlmnet.html)               | Cross-Validated GLM with Elastic Net Regularization    | [glmnet](https://cran.r-project.org/web/packages/glmnet)                   |
| [surv.flexible](https://mlr3proba.mlr-org.com/reference/LearnerSurvFlexible.html)               | Flexible Parametric Spline Models                      | [flexsurv](https://cran.r-project.org/web/packages/flexsurv)               |
| [surv.gamboost](https://mlr3proba.mlr-org.com/reference/LearnerSurvGamboost.html)               | Gradient Boosting for Additive Models                  | [mboost](https://cran.r-project.org/web/packages/mboost/)                  |
| [surv.gbm](https://mlr3proba.mlr-org.com/reference/LearnerSurvGBM.html)                         | Generalized Boosting Regression Modeling               | [gbm](https://cran.r-project.org/web/packages/gbm)                         |
| [surv.glmboost](https://mlr3proba.mlr-org.com/reference/LearnerSurvGlmboost.html)               | Gradient Boosting with Component-wise Linear Models    | [mboost](https://cran.r-project.org/web/packages/mboost/)                  |
| [surv.glmnet](https://mlr3proba.mlr-org.com/reference/LearnerSurvGlmnet.html)                   | GLM with Elastic Net Regularization                    | [glmnet](https://cran.r-project.org/web/packages/glmnet)                   |
| [surv.kaplan](https://mlr3proba.mlr-org.com/reference/LearnerSurvKaplan.html)                   | Kaplan-Meier Estimator                                 | [survival](https://cran.r-project.org/web/packages/survival)               |
| [surv.mboost](https://mlr3proba.mlr-org.com/reference/LearnerSurvMboost.html)                   | Gradient Boosting for Generalized Additive Models      | [mboost](https://cran.r-project.org/web/packages/mboost/)                  |
| [surv.nelson](https://mlr3proba.mlr-org.com/reference/LearnerSurvNelson.html)                   | Nelson-Aalen Estimator                                 | [survival](https://cran.r-project.org/web/packages/survival)               |
| [surv.parametric](https://mlr3proba.mlr-org.com/reference/LearnerSurvParametric.html)           | Fully Parametric Survival Models                       | [survival](https://cran.r-project.org/web/packages/survival)               |
| [surv.penalized](https://mlr3proba.mlr-org.com/reference/LearnerSurvPenalized.html)             | L1 and L2 Penalized Estimation in GLMs                 | [penalized](https://cran.r-project.org/web/packages/penalized)             |
| [surv.randomForestSRC](https://mlr3proba.mlr-org.com/reference/LearnerSurvRandomForestSRC.html) | RandomForestSRC Survival Forest                        | [randomForestSRC](https://cran.r-project.org/web/packages/randomForestSRC) |
| [surv.ranger](https://mlr3proba.mlr-org.com/reference/LearnerSurvRanger.html)                   | Ranger Survival Forest                                 | [ranger](https://cran.r-project.org/web/packages/ranger)                   |
| [surv.rpart](https://mlr3proba.mlr-org.com/reference/LearnerSurvRpart.html)                     | Rpart Survival Forest                                  | [rpart](https://cran.r-project.org/web/packages/rpart)                     |
| [surv.svm](https://mlr3proba.mlr-org.com/reference/LearnerSurvSVM.html)                         | Regression, Ranking and Hybrid Support Vector Machines | [survivalsvm](https://cran.r-project.org/web/packages/survivalsvm)         |

### Survival Measures

| ID                                                                                        | Learner                                 | Package                                                        |
| :---------------------------------------------------------------------------------------- | :-------------------------------------- | :------------------------------------------------------------- |
| [surv.beggC](https://mlr3proba.mlr-org.com/reference/MeasureSurvBeggC.html)               | Begg’s C-Index                          | [survAUC](https://cran.r-project.org/web/packages/survAUC)     |
| [surv.chamblessAUC](https://mlr3proba.mlr-org.com/reference/MeasureSurvChamblessAUC.html) | Chambless and Diao’s AUC                | [survAUC](https://cran.r-project.org/web/packages/survAUC)     |
| [surv.gonenC](https://mlr3proba.mlr-org.com/reference/MeasureSurvGonenC.html)             | Gonen and Heller’s AUC                  | [survAUC](https://cran.r-project.org/web/packages/survAUC)     |
| [surv.graf](https://mlr3proba.mlr-org.com/reference/MeasureSurvGraf.html)                 | Integrated Graf Score                   | [mlr3proba](https://cran.r-project.org/web/packages/mlr3proba) |
| [surv.grafSE](https://mlr3proba.mlr-org.com/reference/MeasureSurvGrafSE.html)             | Standard Error of Integrated Graf Score | [mlr3proba](https://cran.r-project.org/web/packages/mlr3proba) |
| [surv.harrellC](https://mlr3proba.mlr-org.com/reference/MeasureSurvHarrellC.html)         | Harrell’s C-Index                       | [mlr3proba](https://cran.r-project.org/web/packages/mlr3proba) |
| [surv.hungAUC](https://mlr3proba.mlr-org.com/reference/MeasureSurvHungAUC.html)           | Hung and Chiang’s AUC                   | [survAUC](https://cran.r-project.org/web/packages/survAUC)     |
| [surv.intlogloss](https://mlr3proba.mlr-org.com/reference/MeasureSurvIntLogloss.html)     | Integrated Log Loss                     | [mlr3proba](https://cran.r-project.org/web/packages/mlr3proba) |
| [surv.intloglossSE](https://mlr3proba.mlr-org.com/reference/MeasureSurvIntLoglossSE.html) | Standard Error of Integrated Log Loss   | [mlr3proba](https://cran.r-project.org/web/packages/mlr3proba) |
| [surv.logloss](https://mlr3proba.mlr-org.com/reference/MeasureSurvLogloss.html)           | Log Loss                                | [mlr3proba](https://cran.r-project.org/web/packages/mlr3proba) |
| [surv.loglossSE](https://mlr3proba.mlr-org.com/reference/MeasureSurvLoglossSE.html)       | Standard Error of Log Loss              | [mlr3proba](https://cran.r-project.org/web/packages/mlr3proba) |
| [surv.nagelkR2](https://mlr3proba.mlr-org.com/reference/MeasureSurvNagelkR2.html)         | Nagelkerke’s R2                         | [survAUC](https://cran.r-project.org/web/packages/survAUC)     |
| [surv.oquigleyR2](https://mlr3proba.mlr-org.com/reference/MeasureSurvOQuigleyR2.html)     | O’Quigley, Xu, and Stare’s R2           | [survAUC](https://cran.r-project.org/web/packages/survAUC)     |
| [surv.songAUC](https://mlr3proba.mlr-org.com/reference/MeasureSurvSongAUC.html)           | Song and Zhou’s AUC                     | [survAUC](https://cran.r-project.org/web/packages/survAUC)     |
| [surv.songTNR](https://mlr3proba.mlr-org.com/reference/MeasureSurvSongTNR.html)           | Song and Zhou’s TNR                     | [survAUC](https://cran.r-project.org/web/packages/survAUC)     |
| [surv.songTPR](https://mlr3proba.mlr-org.com/reference/MeasureSurvSongTPR.html)           | Song and Zhou’s TPR                     | [survAUC](https://cran.r-project.org/web/packages/survAUC)     |
| [surv.unoAUC](https://mlr3proba.mlr-org.com/reference/MeasureSurvUnoAUC.html)             | Uno’s AUC                               | [survAUC](https://cran.r-project.org/web/packages/survAUC)     |
| [surv.unoC](https://mlr3proba.mlr-org.com/reference/MeasureSurvUnoC.html)                 | Uno’s C-Index                           | [survAUC](https://cran.r-project.org/web/packages/survAUC)     |
| [surv.unoTNR](https://mlr3proba.mlr-org.com/reference/MeasureSurvUnoTNR.html)             | Uno’s TNR                               | [survAUC](https://cran.r-project.org/web/packages/survAUC)     |
| [surv.unoTPR](https://mlr3proba.mlr-org.com/reference/MeasureSurvUnoTPR.html)             | Uno’s TPR                               | [survAUC](https://cran.r-project.org/web/packages/survAUC)     |
| [surv.xuR2](https://mlr3proba.mlr-org.com/reference/MeasureSurvXuR2.html)                 | Xu and O’Quigley’s R2                   | [survAUC](https://cran.r-project.org/web/packages/survAUC)     |

## Example

### Train and Predict

``` r
library(mlr3proba)
library(mlr3)
library(survival)
set.seed(1)

# create task and learner

veteran = mlr3misc::load_dataset("veteran", package = "survival")
task_veteran = TaskSurv$new(id = "veteran", backend = veteran, time = "time", event = "status")
learner = lrn("surv.coxph")

# train/test split 

train_set = sample(task_veteran$nrow, 0.8 * task_veteran$nrow)
test_set = setdiff(seq_len(task_veteran$nrow), train_set)

# fit Cox PH and inspect model

learner$train(task_veteran, row_ids = train_set)
learner$model
#> Call:
#> survival::coxph(formula = task$formula(), data = task$data(), 
#>     x = TRUE)
#> 
#>                        coef exp(coef)  se(coef)      z        p
#> age               -0.007685  0.992345  0.011751 -0.654 0.513130
#> celltypesmallcell  0.976473  2.655076  0.317421  3.076 0.002096
#> celltypeadeno      1.173923  3.234657  0.354138  3.315 0.000917
#> celltypelarge      0.460501  1.584868  0.334837  1.375 0.169039
#> diagtime           0.001186  1.001187  0.009848  0.120 0.904155
#> karno             -0.029826  0.970614  0.006497 -4.590 4.42e-06
#> prior              0.010400  1.010454  0.026531  0.392 0.695071
#> trt                0.225305  1.252705  0.247522  0.910 0.362695
#> 
#> Likelihood ratio test=47.36  on 8 df, p=1.307e-07
#> n= 109, number of events= 100

# make predictions for new data

prediction = learner$predict(task_veteran, row_ids = test_set)
prediction
#> <PredictionSurv> for 28 observations:
#>     row_id time status      crank                distr         lp
#>          3  228   TRUE -0.6936862 <VectorDistribution> -0.6936862
#>          4  126   TRUE -0.7746938 <VectorDistribution> -0.7746938
#>          8  110   TRUE -1.4899216 <VectorDistribution> -1.4899216
#> ---                                                              
#>        119    7   TRUE  1.1495552 <VectorDistribution>  1.1495552
#>        128   19   TRUE  0.9844033 <VectorDistribution>  0.9844033
#>        137   49   TRUE  0.8945899 <VectorDistribution>  0.8945899
```

### Evaluate - crank, lp, and distr

Every `PredictionSurv` object can predict one or more of:

  - `lp` - Linear predictor calculated as the fitted coefficients
    multiplied by the test data.
  - `distr` - Predicted survival distribution, either discrete or
    continuous. Implemented in
    **[distr6](https://cran.r-project.org/web/packages/distr6/)**.
  - `crank` - Continuous ranking. This is either the same as `lp` or the
    expectation of `distr`.

`lp` and `crank` can be used with measures of discrimination such as the
concordance index. Whilst `lp` is a specific mathematical prediction,
`crank` is any continuous ranking that identifies who is more or less
likely to experience the event. So far, the only implemented learner
that only returns a continuous ranking is `surv.svm`. If a
`PredictionSurv` returns an `lp` then the `crank` is identical to this.
Otherwise `crank` is calculated as the expectation of the predicted
survival distribution. Note that for linear proportional hazards models,
the ranking (but not necessarily the `crank` score itself) given by `lp`
and the expected `distr`, is
identical.

``` r
# In the previous example, Cox model predicts `lp` so `crank` is identical

all(prediction$lp == prediction$crank)
#> [1] TRUE
prediction$lp[1:10]
#>  [1] -0.69368619 -0.77469379 -1.48992162 -1.02294630  0.10603704  0.35349280
#>  [7]  0.19769846 -0.24070498  1.61950353  0.02121054

# These are evaluated with measures of discrimination and calibration. As all PredictionSurv objects
# will return crank, Harrell's C is the default measure.

prediction$score()
#> surv.harrellC 
#>     0.7526596

# distr is evaluated with probabilistic scoring rules.

measure = lapply(c("surv.graf", "surv.grafSE", "surv.intlogloss", "surv.intloglossSE",
                   "surv.logloss", "surv.loglossSE"), msr)
prediction$score(measure)
#>         surv.graf       surv.grafSE   surv.intlogloss surv.intloglossSE 
#>         0.1249958         0.0129813         0.3917054         0.0326058 
#>      surv.logloss    surv.loglossSE 
#>        22.5096384         2.8787520

# Often measures can be integrated over mutliple time-points, or return predictions for single time-points

measure = msr("surv.graf", times = 60)
prediction$score(measure)
#> surv.graf 
#> 0.1457346
```

## Feature Overview and Lifecycle

The vision of **mlr3proba** is to be the first complete probabilistic
machine learning package in R. This encompasses survival analysis,
probabilistic regression (not just via MCMC\!), and unsupervised density
estimation. The first release of **mlr3proba** is focused entirely on
survival analysis and introduces `TaskSurv`. Later releases will include
`TaskDensity` and will extend `TaskRegr` to have probabilistic predict
types. The lifecycle of the survival task and features are considered
`maturing` and any major changes are unlikely. The density and
probabilistic regression tasks are currently in the early stages of
development. The current main features of **mlr3proba** are:

  - The added `TaskSurv`, `LearnerSurv`, `PredictionSurv` for survival
    analysis
  - 17 survival learners, and 21 survival measures, including efficient
    implementations of censoring-adjusted probabilistic measures, such
    as the Integrated Graf (or Brier) Score.
  - PipeOps integrated with
    **[mlr3pipelines](https://github.com/mlr-org/mlr3pipelines)** for
    composition of probability distributions from linear predictors

## Future Plans

  - Add `TaskDensity`, `PredictionDensity`, `LearnerDensity`, and
    associated learners/measures
  - Add `prob` predict type to `TaskRegr`, and associated
    learners/measures
  - Allow `MeasureSurv` to return measures at multiple time-points
    simultaneously
  - Improve estimation of integrated scores, and re-implement
    **[survAUC](https://cran.r-project.org/web/packages/survAUC)**
    scores in **mlr3proba**
  - Continue to add survival measures and learners

## Bugs, Questions, Feedback

**mlr3proba** is a free and open source software project that encourages
participation and feedback. If you have any issues, questions,
suggestions or feedback, please do not hesitate to open an “issue” about
it on the [GitHub page](https://github.com/mlr-org/mlr3proba/issues)\!

In case of problems / bugs, it is often helpful if you provide a
“minimum working example” that showcases the behaviour (but don’t
worry about this if the bug is obvious).

Please understand that the resources of the project are limited:
response may sometimes be delayed by a few days, and some feature
suggestions may be rejected if they are deemed too tangential to the
vision behind the project.

## Similar Projects

A predecessor to this package is
**[mlr](https://github.com/mlr-org/mlr)**, using the survival task.
Several packages exist for pure Bayesian probabilistic modelling,
including **[jags](http://mcmc-jags.sourceforge.net/)** and
**[stan](https://github.com/stan-dev/rstan)**. For implementation of a
few survival models and measures, the largest package is
**[survival](https://github.com/therneau/survival)**. There does not
appear to be a package that implements many different variants of
density estimation, but see **[this
list](https://vita.had.co.nz/papers/density-estimation.pdf)** for the
biggest density estimation packages in R.

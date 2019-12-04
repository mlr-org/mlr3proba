
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
probability distributions are predicted. Regression and classification
tasks can be shown to be sub-fields of PSL, for example reducing a
predicted probability distribution from probabilistic regression by
taking its mean gives a regression prediction. PSL is therefore a
powerful tool to provide more information about predictions than the
more classical regression or classification. Probably the most known
variant of PSL is survival analysis, where the task of interest is to
predict an individual’s survival curve. Other forms of PSL include
density estimation and probabilistic regression. To-date, PSL toolkits
in R have been limited to Bayesian simulation packages, but
**mlr3proba** hopes to change this by allowing domain-agnostic (Bayesian
or Frequentist) fit/predict and evaluation workflows.

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

| ID                                                                                              | Learner                                                | Package                                                               |
| :---------------------------------------------------------------------------------------------- | :----------------------------------------------------- | :-------------------------------------------------------------------- |
| [surv.blackboost](https://mlr3proba.mlr-org.com/reference/LearnerSurvBlackboost.html)           | Gradient Boosting with Regression Trees                | [mboost](https://CRAN.R-project.org/package=mboost)                   |
| [surv.coxph](https://mlr3proba.mlr-org.com/reference/LearnerSurvCoxPH.html)                     | Cox Proportional Hazards                               | [survival](https://CRAN.R-project.org/package=survival)               |
| [surv.cvglmnet](https://mlr3proba.mlr-org.com/reference/LearnerSurvCVGlmnet.html)               | Cross-Validated GLM with Elastic Net Regularization    | [glmnet](https://CRAN.R-project.org/package=glmnet)                   |
| [surv.flexible](https://mlr3proba.mlr-org.com/reference/LearnerSurvFlexible.html)               | Flexible Parametric Spline Models                      | [flexsurv](https://CRAN.R-project.org/package=flexsurv)               |
| [surv.gamboost](https://mlr3proba.mlr-org.com/reference/LearnerSurvGamboost.html)               | Gradient Boosting for Additive Models                  | [mboost](https://CRAN.R-project.org/package=mboost)                   |
| [surv.gbm](https://mlr3proba.mlr-org.com/reference/LearnerSurvGBM.html)                         | Generalized Boosting Regression Modeling               | [gbm](https://CRAN.R-project.org/package=gbm)                         |
| [surv.glmboost](https://mlr3proba.mlr-org.com/reference/LearnerSurvGlmboost.html)               | Gradient Boosting with Component-wise Linear Models    | [mboost](https://CRAN.R-project.org/package=mboost)                   |
| [surv.glmnet](https://mlr3proba.mlr-org.com/reference/LearnerSurvGlmnet.html)                   | GLM with Elastic Net Regularization                    | [glmnet](https://CRAN.R-project.org/package=glmnet)                   |
| [surv.kaplan](https://mlr3proba.mlr-org.com/reference/LearnerSurvKaplan.html)                   | Kaplan-Meier Estimator                                 | [survival](https://CRAN.R-project.org/package=survival)               |
| [surv.mboost](https://mlr3proba.mlr-org.com/reference/LearnerSurvMboost.html)                   | Gradient Boosting for Generalized Additive Models      | [mboost](https://CRAN.R-project.org/package=mboost)                   |
| [surv.nelson](https://mlr3proba.mlr-org.com/reference/LearnerSurvNelson.html)                   | Nelson-Aalen Estimator                                 | [survival](https://CRAN.R-project.org/package=survival)               |
| [surv.parametric](https://mlr3proba.mlr-org.com/reference/LearnerSurvParametric.html)           | Fully Parametric Survival Models                       | [survival](https://CRAN.R-project.org/package=survival)               |
| [surv.penalized](https://mlr3proba.mlr-org.com/reference/LearnerSurvPenalized.html)             | L1 and L2 Penalized Estimation in GLMs                 | [penalized](https://CRAN.R-project.org/package=penalized)             |
| [surv.randomForestSRC](https://mlr3proba.mlr-org.com/reference/LearnerSurvRandomForestSRC.html) | RandomForestSRC Survival Forest                        | [randomForestSRC](https://CRAN.R-project.org/package=randomForestSRC) |
| [surv.ranger](https://mlr3proba.mlr-org.com/reference/LearnerSurvRanger.html)                   | Ranger Survival Forest                                 | [ranger](https://CRAN.R-project.org/package=ranger)                   |
| [surv.rpart](https://mlr3proba.mlr-org.com/reference/LearnerSurvRpart.html)                     | Rpart Survival Forest                                  | [rpart](https://CRAN.R-project.org/package=rpart)                     |
| [surv.svm](https://mlr3proba.mlr-org.com/reference/LearnerSurvSVM.html)                         | Regression, Ranking and Hybrid Support Vector Machines | [survivalsvm](https://CRAN.R-project.org/package=survivalsvm)         |

### Survival Measures

| ID                                                                                        | Learner                                 | Package                                                   |
| :---------------------------------------------------------------------------------------- | :-------------------------------------- | :-------------------------------------------------------- |
| [surv.beggC](https://mlr3proba.mlr-org.com/reference/MeasureSurvBeggC.html)               | Begg’s C-Index                          | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.chamblessAUC](https://mlr3proba.mlr-org.com/reference/MeasureSurvChamblessAUC.html) | Chambless and Diao’s AUC                | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.gonenC](https://mlr3proba.mlr-org.com/reference/MeasureSurvGonenC.html)             | Gonen and Heller’s C-Index              | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.graf](https://mlr3proba.mlr-org.com/reference/MeasureSurvGraf.html)                 | Integrated Graf Score                   | [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) |
| [surv.grafSE](https://mlr3proba.mlr-org.com/reference/MeasureSurvGrafSE.html)             | Standard Error of Integrated Graf Score | [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) |
| [surv.harrellC](https://mlr3proba.mlr-org.com/reference/MeasureSurvHarrellC.html)         | Harrell’s C-Index                       | [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) |
| [surv.hungAUC](https://mlr3proba.mlr-org.com/reference/MeasureSurvHungAUC.html)           | Hung and Chiang’s AUC                   | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.intlogloss](https://mlr3proba.mlr-org.com/reference/MeasureSurvIntLogloss.html)     | Integrated Log Loss                     | [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) |
| [surv.intloglossSE](https://mlr3proba.mlr-org.com/reference/MeasureSurvIntLoglossSE.html) | Standard Error of Integrated Log Loss   | [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) |
| [surv.logloss](https://mlr3proba.mlr-org.com/reference/MeasureSurvLogloss.html)           | Log Loss                                | [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) |
| [surv.loglossSE](https://mlr3proba.mlr-org.com/reference/MeasureSurvLoglossSE.html)       | Standard Error of Log Loss              | [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) |
| [surv.nagelkR2](https://mlr3proba.mlr-org.com/reference/MeasureSurvNagelkR2.html)         | Nagelkerke’s R2                         | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.oquigleyR2](https://mlr3proba.mlr-org.com/reference/MeasureSurvOQuigleyR2.html)     | O’Quigley, Xu, and Stare’s R2           | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.songAUC](https://mlr3proba.mlr-org.com/reference/MeasureSurvSongAUC.html)           | Song and Zhou’s AUC                     | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.songTNR](https://mlr3proba.mlr-org.com/reference/MeasureSurvSongTNR.html)           | Song and Zhou’s TNR                     | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.songTPR](https://mlr3proba.mlr-org.com/reference/MeasureSurvSongTPR.html)           | Song and Zhou’s TPR                     | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.unoAUC](https://mlr3proba.mlr-org.com/reference/MeasureSurvUnoAUC.html)             | Uno’s AUC                               | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.unoC](https://mlr3proba.mlr-org.com/reference/MeasureSurvUnoC.html)                 | Uno’s C-Index                           | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.unoTNR](https://mlr3proba.mlr-org.com/reference/MeasureSurvUnoTNR.html)             | Uno’s TNR                               | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.unoTPR](https://mlr3proba.mlr-org.com/reference/MeasureSurvUnoTPR.html)             | Uno’s TPR                               | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.xuR2](https://mlr3proba.mlr-org.com/reference/MeasureSurvXuR2.html)                 | Xu and O’Quigley’s R2                   | [survAUC](https://CRAN.R-project.org/package=survAUC)     |

## Feature Overview and Lifecycle

The vision of **mlr3proba** is to be the first complete probabilistic
machine learning package in R. This encompasses survival analysis,
probabilistic regression, and unsupervised density estimation. The first
release of **mlr3proba** is focused entirely on survival analysis and
introduces `TaskSurv`. Later releases will include `TaskDensity` and
will extend `TaskRegr` to have probabilistic predict types. The
lifecycle of the survival task and features are considered `maturing`
and any major changes are unlikely. The density and probabilistic
regression tasks are currently in the early stages of development. The
current main features of **mlr3proba** are:

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
    **[survAUC](https://CRAN.R-project.org/package=survAUC)** scores in
    **mlr3proba**
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

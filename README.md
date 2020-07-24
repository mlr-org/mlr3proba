
# mlr3proba

Package website: [release](https://mlr3proba.mlr-org.com/) |
[dev](https://mlr3proba.mlr-org.com/dev)

Probabilistic Supervised Learning for
**[mlr3](https://github.com/mlr-org/mlr3)**.

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3proba/workflows/tic/badge.svg?branch=master)](https://github.com/mlr-org/mlr3proba/actions)
[![cran
checks](https://cranchecks.info/badges/worst/mlr3proba)](https://cran.r-project.org/web/checks/check_results_mlr3proba.html)

[![CRAN Status
Badge](https://www.r-pkg.org/badges/version-ago/mlr3proba)](https://cran.r-project.org/package=mlr3proba)
[![codecov](https://codecov.io/gh/mlr-org/mlr3proba/branch/master/graph/badge.svg)](https://codecov.io/gh/mlr-org/mlr3proba)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Dependencies](https://tinyverse.netlify.com/badge/mlr3proba)](https://cran.r-project.org/package=mlr3proba)
<!-- badges: end -->

## What is mlr3proba ?

**mlr3proba** is a machine learning toolkit for making probabilistic
predictions within the **[mlr3](https://github.com/mlr-org/mlr3)**
ecosystem. It currently supports the following tasks:

  - Probabilistic supervised regression - Supervised regression with a
    predictive distribution as the return type.
  - Predictive survival analysis - Survival analysis where individual
    predictive hazards can be queried. This is equivalent to
    probabilistic supervised regression with censored observations.
  - Unconditional distribution estimation, where the distribution is
    returned. Sub-cases are density estimation and unconditional
    survival estimation.

Key features of **mlr3proba** are

  - A unified fit/predict model interface to any probabilistic
    predictive model (frequentist, Bayesian, or other)
  - Pipeline/model composition
  - Task reduction strategies
  - Domain-agnostic evaluation workflows using task specific algorithmic
    performance measures.

**mlr3proba** makes use of the
**[distr6](https://github.com/alan-turing-institute/distr6)**
probability distribution interface as its probabilistic predictive
return type.

## Feature Overview

The current **mlr3proba** release focuses on survival analysis, and
contains:

  - Task frameworks for survival analysis (`TaskSurv`)
  - A comprehensive selection of 17 predictive survival learners
  - A comprehensive selection of 21 performance measures for predictive
    survival learners, with respect to prognostic index (continuous
    rank) prediction, and probabilistic (distribution) prediction
  - PipeOps integrated with
    **[mlr3pipelines](https://github.com/mlr-org/mlr3pipelines)**, for
    basic pipeline building, and reduction/composition strategies using
    linear predictors and baseline hazards.

## Roadmap

The vision of **mlr3proba** is to provide comprehensive machine learning
functionality to the mlr3 ecosystem for continuous probabilistic return
types.

The lifecycle of the survival task and features are considered
`maturing` and any major changes are unlikely.

The density and probabilistic supervised regression tasks are currently
in the early stages of development. Task frameworks have been drawn up,
but may not be stable; learners need to be interfaced, and contributions
are very welcome (see
[issues](https://github.com/mlr-org/mlr3proba/issues)).

## Installation

Install the last release from CRAN:

``` r
install.packages("mlr3proba")
```

Install the development version from GitHub:

``` r
remotes::install_github("mlr-org/mlr3proba")
```

## Survival Analysis

### Survival Learners

Learners are located either in
[mlr3proba](https://github.com/mlr-org/mlr3proba), the [mlr3learners
repository](https://github.com/mlr-org/mlr3learners), or the
[mlr3learners organisation](https://github.com/mlr3learners). See
[here](https://github.com/mlr3learners/mlr3learners.drat) for
instructions in how to install learners from the [mlr3learners
organisation](https://github.com/mlr3learners).

| ID                                                                                   | Learner                                                   | Package                                                                  |
| :----------------------------------------------------------------------------------- | :-------------------------------------------------------- | :----------------------------------------------------------------------- |
| [surv.akritas](https://github.com/mlr3learners/mlr3learners.proba)                   | Akritas Conditional Non-Parametric Estimator              | [mlr3learners.proba](https://github.com/mlr3learners/mlr3learners.proba) |
| [surv.blackboost](https://github.com/mlr3learners/mlr3learners.mboost)               | Gradient Boosting with Regression Trees                   | [mboost](https://CRAN.R-project.org/package=mboost)                      |
| [surv.coxboost](https://github.com/mlr3learners/mlr3learners.coxboost)               | Cox Model with Likelihood Based Boosting                  | [CoxBoost](https://CRAN.R-project.org/package=CoxBoost)                  |
| [surv.coxph](https://mlr3proba.mlr-org.com/reference/mlr_learners_surv.coxph.html)   | Cox Proportional Hazards                                  | [survival](https://CRAN.R-project.org/package=survival)                  |
| [surv.cvcoxboost](https://github.com/mlr3learners/mlr3learners.coxboost)             | Cox Model with Cross-Validation Likelihood Based Boosting | [CoxBoost](https://CRAN.R-project.org/package=CoxBoost)                  |
| [surv.cvglmnet](https://mlr3learners.mlr-org.com/)                                   | Cross-Validated GLM with Elastic Net Regularization       | [glmnet](https://CRAN.R-project.org/package=glmnet)                      |
| [surv.flexible](https://github.com/mlr3learners/mlr3learners.flexsurv)               | Flexible Parametric Spline Models                         | [flexsurv](https://CRAN.R-project.org/package=flexsurv)                  |
| [surv.gamboost](https://github.com/mlr3learners/mlr3learners.mboost)                 | Gradient Boosting for Additive Models                     | [mboost](https://CRAN.R-project.org/package=mboost)                      |
| [surv.gbm](https://github.com/mlr3learners/mlr3learners.gbm)                         | Generalized Boosting Regression Modeling                  | [gbm](https://CRAN.R-project.org/package=gbm)                            |
| [surv.glmboost](https://github.com/mlr3learners/mlr3learners.mboost)                 | Gradient Boosting with Component-wise Linear Models       | [mboost](https://CRAN.R-project.org/package=mboost)                      |
| [surv.glmnet](https://mlr3learners.mlr-org.com/)                                     | GLM with Elastic Net Regularization                       | [glmnet](https://CRAN.R-project.org/package=glmnet)                      |
| [surv.kaplan](https://mlr3proba.mlr-org.com/reference/mlr_learners_surv.kaplan.html) | Kaplan-Meier Estimator                                    | [survival](https://CRAN.R-project.org/package=survival)                  |
| [surv.mboost](https://github.com/mlr3learners/mlr3learners.mboost)                   | Gradient Boosting for Generalized Additive Models         | [mboost](https://CRAN.R-project.org/package=mboost)                      |
| [surv.nelson](https://github.com/mlr3learners/mlr3learners.survival)                 | Nelson-Aalen Estimator                                    | [survival](https://CRAN.R-project.org/package=survival)                  |
| [surv.parametric](https://github.com/mlr3learners/mlr3learners.survival)             | Fully Parametric Survival Models                          | [survival](https://CRAN.R-project.org/package=survival)                  |
| [surv.penalized](https://github.com/mlr3learners/mlr3learners.penalized)             | L1 and L2 Penalized Estimation in GLMs                    | [penalized](https://CRAN.R-project.org/package=penalized)                |
| [surv.randomForestSRC](https://github.com/mlr3learners/mlr3learners.randomforestsrc) | RandomForestSRC Survival Forest                           | [randomForestSRC](https://CRAN.R-project.org/package=randomForestSRC)    |
| [surv.ranger](https://github.com/mlr-org/mlr3learners/)                              | Ranger Survival Forest                                    | [ranger](https://CRAN.R-project.org/package=ranger)                      |
| [surv.rpart](https://mlr3proba.mlr-org.com/reference/mlr_learners_surv.rpart.html)   | Rpart Survival Forest                                     | [rpart](https://CRAN.R-project.org/package=rpart)                        |
| [surv.svm](https://github.com/mlr3learners/mlr3learners.survivalsvm)                 | Regression, Ranking and Hybrid Support Vector Machines    | [survivalsvm](https://CRAN.R-project.org/package=survivalsvm)            |
| [surv.xgboost](https://github.com/mlr-org/mlr3learners/)                             | Cox Model with Gradient Boosting Trees                    | [xgboost](https://CRAN.R-project.org/package=xgboost)                    |

### Survival Measures

| ID                                                                                                  | Measure                             | Package                                                   |
| :-------------------------------------------------------------------------------------------------- | :---------------------------------- | :-------------------------------------------------------- |
| [surv.calib\_alpha](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_alpha.html)     | van Houwelingen’s Alpha Calibration | [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) |
| [surv.calib\_beta](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_beta.html)       | van Houwelingen’s Beta Calibration  | [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) |
| [surv.chambless\_auc](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.chambless_auc.html) | Chambless and Diao’s AUC            | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.graf](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.graf.html)                    | Integrated Graf Score               | [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) |
| [surv.hungAUC](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.hung_auc.html)             | Hung and Chiang’s AUC               | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.intlogloss](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.intlogloss.html)        | Integrated Log Loss                 | [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) |
| [surv.logloss](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.logloss.html)              | Log Loss                            | [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) |
| [surv.nagelk\_r2](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.nagelk_r2.html)         | Nagelkerke’s R2                     | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.oquigley\_r2](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.oquigley_r2.html)     | O’Quigley, Xu, and Stare’s R2       | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.song\_auc](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.song_auc.html)           | Song and Zhou’s AUC                 | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.song\_tnr](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.song_tnr.html)           | Song and Zhou’s TNR                 | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.song\_tpr](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.song_tpr.html)           | Song and Zhou’s TPR                 | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.uno\_auc](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_auc.html)             | Uno’s AUC                           | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.uno\_tnr](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_tnr.html)             | Uno’s TNR                           | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.uno\_tpr](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_tpr.html)             | Uno’s TPR                           | [survAUC](https://CRAN.R-project.org/package=survAUC)     |
| [surv.xu\_r2](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.xu_r2.html)                 | Xu and O’Quigley’s R2               | [survAUC](https://CRAN.R-project.org/package=survAUC)     |

## Density Estimation

### Density Learners

Learners are located either in
[mlr3proba](https://github.com/mlr-org/mlr3proba), the [mlr3learners
repository](https://github.com/mlr-org/mlr3learners), or the
[mlr3learners organisation](https://github.com/mlr3learners). See
[here](https://github.com/mlr3learners/mlr3learners.drat) for
instructions in how to install learners from the [mlr3learners
organisation](https://github.com/mlr3learners).

| ID                                                                               | Learner                                                       | Package                                                       |
| :------------------------------------------------------------------------------- | :------------------------------------------------------------ | :------------------------------------------------------------ |
| [dens.hist](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.hist.html) | Univariate Histogram Density Estimator                        | graphics                                                      |
| [dens.kde](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.kde.html)   | Univariate KDE for Different Kernels                          | [distr6](https://CRAN.R-project.org/package=distr6)           |
| [dens.kdeKD](https://github.com/mlr3learners/mlr3learners.kerdiest)              | Nonparametric KDE Using Plug-in Method of Polansky and Baker  | [kerdiest](https://CRAN.R-project.org/package=kerdiest)       |
| [dens.kdeKS](https://github.com/mlr3learners/mlr3learners.ks)                    | Nonparametric Gaussian KDE                                    | [ks](https://CRAN.R-project.org/package=ks)                   |
| [dens.locfit](https://github.com/mlr3learners/mlr3learners.locfit)               | Nonparametric KDE Using Gaussian kernel                       | [locfit](https://CRAN.R-project.org/package=locfit)           |
| [dens.logspline](https://github.com/mlr3learners/mlr3learners.logspline)         | Logspline Method for Density Estimation                       | [logspline](https://CRAN.R-project.org/package=logspline)     |
| [dens.mixed](https://github.com/mlr3learners/mlr3learners.np)                    | KDE Using Li and Racine Bandwidth Specification               | [np](https://CRAN.R-project.org/package=np)                   |
| [dens.nonpar](https://github.com/mlr3learners/mlr3learners.sm)                   | Nonparametric KDE Using Normal Optimal Smoothing Parameter    | [sm](https://CRAN.R-project.org/package=sm)                   |
| [dens.pen](https://github.com/mlr3learners/mlr3learners.pendensity)              | Density Estimation with a Penalized Mixture                   | [pendensity](https://CRAN.R-project.org/package=pendensity)   |
| [dens.plug](https://github.com/mlr3learners/mlr3learners.plugdensity)            | Density Estimation with Iterative Plug-in Bandwidth Selection | [plugdensity](https://CRAN.R-project.org/package=plugdensity) |
| [dens.spline](https://github.com/mlr3learners/mlr3learners.gss)                  | Density Estimation Using Smoothing Spline ANOVA               | [gss](https://CRAN.R-project.org/package=gss)                 |

### Density Measures

| ID                                                                                     | Measure  | Package                                                   |
| :------------------------------------------------------------------------------------- | :------- | :-------------------------------------------------------- |
| [dens.logloss](https://mlr3proba.mlr-org.com/reference/mlr_measures_dens.logloss.html) | Log Loss | [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) |

## Near-Future Plans

  - Add `prob` predict type to `TaskRegr`, and associated
    learners/measures
  - Allow `MeasureSurv` to return measures at multiple time-points
    simultaneously
  - Continue to add survival measures and learners

## Bugs, Questions, Feedback

**mlr3proba** is a free and open source software project that encourages
participation and feedback. If you have any issues, questions,
suggestions or feedback, please do not hesitate to open an “issue” about
it on the [GitHub page](https://github.com/mlr-org/mlr3proba/issues)\!

In case of problems / bugs, it is often helpful if you provide a
“minimum working example” that showcases the behaviour (but don’t
worry about this if the bug is obvious).

## Similar Projects

Predecessors to this package are previous instances of survival
modelling in **[mlr](https://github.com/mlr-org/mlr)**. The
**[skpro](https://github.com/alan-turing-institute/skpro)** package in
the python/scikit-learn ecosystem follows a similar interface for
probabilistic supervised learning and is an architectural predecessor.
Several packages exist which allow probabilistic predictive modelling
with a Bayesian model specific general interface, such as
**[jags](http://mcmc-jags.sourceforge.net/)** and
**[stan](https://github.com/stan-dev/rstan)**. For implementation of a
few survival models and measures, a central package is
**[survival](https://github.com/therneau/survival)**. There does not
appear to be a package that provides an architectural framework for
distribution/density estimation, see **[this
list](https://vita.had.co.nz/papers/density-estimation.pdf)** for a
review of density estimation packages in R.

## Acknowledgements

Several people contributed to the building of `mlr3proba`. Firstly,
thanks to Michel Lang for writing `mlr3survival`. Several learners and
measures implemented in `mlr3proba`, as well as the prediction, task,
and measure surv objects, were written initially in `mlr3survival`
before being absorbed into `mlr3proba`. Secondly thanks to Franz Kiraly
for major contributions towards the design of the proba-specific parts
of the package, including compositors and predict types. Also for
mathematical contributions towards the scoring rules implemented in the
package. Finally thanks to Bernd Bischl and the rest of the mlr core
team for building `mlr3` and for many conversations about the design of
`mlr3proba`.

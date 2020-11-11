
# mlr3proba

Package website: [release](https://mlr3proba.mlr-org.com/) |
[dev](https://mlr3proba.mlr-org.com/dev/)

Probabilistic Supervised Learning for
**[mlr3](https://github.com/mlr-org/mlr3/)**.

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3proba/workflows/tic/badge.svg?branch=master)](https://github.com/mlr-org/mlr3proba/actions)
[![CodeFactor](https://www.codefactor.io/repository/github/mlr-org/mlr3proba/badge)](https://www.codefactor.io/repository/github/mlr-org/mlr3proba)
[![cran
checks](https://cranchecks.info/badges/worst/mlr3proba)](https://cran.r-project.org/web/checks/check_results_mlr3proba.html)
[![arXiv](https://img.shields.io/badge/arXiv-2008.08080-brightgreen.svg)](https://arxiv.org/abs/2008.08080)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
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

## Learners

Core learners are implemented in
[mlr3proba](https://github.com/mlr-org/mlr3proba), recommended common
learners are implemented in
[mlr3learners](https://github.com/mlr-org/mlr3learners), and many more
are implemented in
[mlr3extralearners](https://github.com/mlr-org/mlr3extralearners). Use
the [interactive search
table](https://mlr3extralearners.mlr-org.com/articles/learners/list_learners.html)
to search for available learners and see the [learner status
page](https://mlr3extralearners.mlr-org.com/articles/learners/learner_status.html)
for their live status.

## Measures

For density estimation only the log-loss is currently implemented, for
survival analysis, the following measures are implemented:

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
**[rjags](https://CRAN.R-project.org/package=rjags)** and
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

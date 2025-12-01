# mlr3proba

![](reference/figures/warning.png) This package **will be archived**
somewhere in 2026. It should work with other mlr3-packages on the
following versions: `mlr3` (1.2.0/1.3.0), `mlr3pipelines` (0.10.0),
`mlr3tuning` (1.5.0) and `mlr3extralearners` (1.3.0). Development
efforts for `mlr3`-compatible survival analysis continues in packages
`survdistr`, `mlr3survival`, `mlr3cmprsk` (CRAN releases hopefully
within 2026).

Probabilistic Learning for **[mlr3](https://github.com/mlr-org/mlr3/)**
([website](https://mlr3proba.mlr-org.com/)).

## What is mlr3proba?

`mlr3proba` is a machine learning toolkit for probabilistic learning
within the [mlr3](https://github.com/mlr-org/mlr3) ecosystem. It
supports:

1.  **Predictive survival analysis** for single-event tasks with
    right-censoring.
2.  **Unsupervised density estimation** for single variable tasks.
3.  **Probabilistic supervised regression**: Supervised regression with
    a predictive distribution as the return type.

## Main Features

Key features of `mlr3proba` focus on **survival analysis** and are:

- A task framework for survival analysis (`TaskSurv`)
- A unified `$train()`/`$predict()` model interface to any probabilistic
  predictive model (frequentist, Bayesian, Deep Learning, or other)
- Use of the [distr6](https://github.com/xoopR/distr6) interface for the
  survival probability distribution prediction
- A comprehensive selection of **measures** for evaluating model
  performance, with respect to prognostic index (continuous rank)
  prediction, and probabilistic (distribution) prediction
- Basic **ML pipeline building** integrated with
  [mlr3pipelines](https://github.com/mlr-org/mlr3pipelines) (e.g. to
  transform prediction types)
- **Reduction strategies** to transform survival to
  classification/regression tasks

## Installation

`mlr3proba` will not be on CRAN. Please follow one of the two following
methods to install it:

### R-universe

Install the latest released version:

``` r
install.packages("mlr3proba", repos = "https://mlr-org.r-universe.dev")
```

### GitHub

Install the latest development version:

``` r
# install.packages("pak")
pak::pak("mlr-org/mlr3proba")
```

## Learners

### Survival Analysis

- [Core survival
  learners](https://mlr3proba.mlr-org.com/reference/index.html#survival-learners)
  are implemented in `mlr3proba` and include the Kaplan-Meier Estimator,
  the Cox Proportional Hazards model and the Survival Tree learner.
- In [mlr3extralearners](https://github.com/mlr-org/mlr3extralearners)
  we have interfaced several more advanced ML learners suited for
  survival tasks. Use the [interactive search
  table](https://mlr-org.com/learners.html) to search for the available
  survival learners and see the [learner status
  page](https://mlr3extralearners.mlr-org.com/articles/learner_status.html)
  for their live status.

### Density Estimation

See list of available density learners
[here](https://mlr3proba.mlr-org.com/reference/index.html#density-learners).

### Probabilistic Regression

Probabilistic regression is enabled via
[mlr3pipelines](https://github.com/mlr-org/mlr3pipelines). See the
available
[pipeline](https://mlr3proba.mlr-org.com/reference/mlr_graphs_probregr.html)
and associated distribution composition
[PipeOp](http://mlr3proba.mlr-org.com/reference/mlr_pipeops_compose_probregr.md).

## Measures

For density estimation and probabilistic regression only the
**log-loss** is implemented. For survival analysis, see list
[here](https://mlr3proba.mlr-org.com/reference/index.html#survival-measures).

Some commonly used measures for right-censored single-event tasks are
the following:

| ID                                                                                             | Measure                   | Package     | Category       | Prediction Type |
|:-----------------------------------------------------------------------------------------------|:--------------------------|:------------|:---------------|:----------------|
| [surv.dcalib](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.dcalib.html)           | D-Calibration             | `mlr3proba` | Calibration    | `distr`         |
| [surv.calib_index](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_index.html) | One-point Calibration     | `mlr3proba` | Calibration    | `distr`         |
| [surv.cindex](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.cindex.html)           | Concordance Index         | `mlr3proba` | Discrimination | `crank`         |
| [surv.uno_auc](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_auc.html)         | Uno’s AUC                 | `survAUC`   | Discrimination | `lp`            |
| [surv.graf](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.graf.html)               | Integrated Brier Score    | `mlr3proba` | Scoring Rule   | `distr`         |
| [surv.rcll](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.rcll.html)               | Right-Censored Log loss   | `mlr3proba` | Scoring Rule   | `distr`         |
| [surv.intlogloss](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.intlogloss.html)   | Integrated Log-Likelihood | `mlr3proba` | Scoring Rule   | `distr`         |

## Bugs, Questions, Feedback

**mlr3proba** is a free and open source software project that encourages
participation and feedback. If you have any issues, questions,
suggestions or feedback, please do not hesitate to open an “issue” about
it on the [GitHub page](https://github.com/mlr-org/mlr3proba/issues)!

In case of problems / bugs, it is often helpful if you provide a
“minimum working example” using [reprex](https://reprex.tidyverse.org/)
that showcases the behavior.

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
review of density estimation packages in `R`.

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

## Citing mlr3proba

If you use mlr3proba, please cite our [Bioinformatics
article](https://doi.org/10.1093/bioinformatics/btab039):

``` R
@Article{,
  title = {mlr3proba: An R Package for Machine Learning in Survival Analysis},
  author = {Raphael Sonabend and Franz J Király and Andreas Bender and Bernd Bischl and Michel Lang},
  journal = {Bioinformatics},
  month = {02},
  year = {2021},
  doi = {10.1093/bioinformatics/btab039},
  issn = {1367-4803},
}
```

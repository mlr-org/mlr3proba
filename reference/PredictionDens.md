# Prediction Object for Density

This object stores the predictions returned by a learner of class
[LearnerDens](https://mlr3proba.mlr-org.com/reference/LearnerDens.md).

The `task_type` is set to `"dens"`.

## See also

Other Prediction:
[`PredictionSurv`](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md)

## Super class

[`mlr3::Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html)
-\> `PredictionDens`

## Active bindings

- `pdf`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Access the stored predicted probability density function.

- `cdf`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Access the stored predicted cumulative distribution function.

- `distr`:

  ([Distribution](https://xoopr.github.io/distr6/reference/Distribution.html))  
  Access the stored estimated distribution.

## Methods

### Public methods

- [`PredictionDens$new()`](#method-PredictionDens-new)

- [`PredictionDens$clone()`](#method-PredictionDens-clone)

Inherited methods

- [`mlr3::Prediction$filter()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-filter)
- [`mlr3::Prediction$format()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-format)
- [`mlr3::Prediction$help()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-help)
- [`mlr3::Prediction$obs_loss()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-obs_loss)
- [`mlr3::Prediction$print()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-print)
- [`mlr3::Prediction$score()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-score)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://rdrr.io/pkg/R6/man/R6Class.html) class.

#### Usage

    PredictionDens$new(
      task = NULL,
      row_ids = task$row_ids,
      pdf = NULL,
      cdf = NULL,
      distr = NULL,
      check = TRUE
    )

#### Arguments

- `task`:

  ([TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md))  
  Task, used to extract defaults for `row_ids`.

- `row_ids`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Row ids of the predicted observations, i.e. the row ids of the test
  set.

- `pdf`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Numeric vector of estimated probability density function, evaluated at
  values in test set. One element for each observation in the test set.

- `cdf`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Numeric vector of estimated cumulative distribution function,
  evaluated at values in test set. One element for each observation in
  the test set.

- `distr`:

  ([Distribution](https://xoopr.github.io/distr6/reference/Distribution.html))  
  [Distribution](https://xoopr.github.io/distr6/reference/Distribution.html)
  from
  [distr6](https://xoopr.github.io/distr6/reference/distr6-package.html).
  The distribution from which `pdf` and `cdf` are derived.

- `check`:

  (`logical(1)`)  
  If `TRUE`, performs argument checks and predict type conversions.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PredictionDens$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = mlr_tasks$get("precip")
learner = mlr_learners$get("dens.hist")
p = learner$train(task)$predict(task)
head(as.data.table(p))
#>    row_ids         pdf       cdf
#>      <int>       <num>     <num>
#> 1:       1 0.001428571 0.9957143
#> 2:       2 0.007142857 0.9478571
#> 3:       3 0.005714286 0.0400000
#> 4:       4 0.030000000 0.8692857
#> 5:       5 0.012857143 0.1085714
#> 6:       6 0.012857143 0.1497143
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        distr
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <list>
#> 1: <Distribution>\n  Public:\n    alias: \n    cdf: function (..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, \n    clone: function (deep = FALSE) \n    confidence: function (alpha = 0.95, sides = "both", median = FALSE) \n    correlation: function () \n    decorators: active binding\n    description: NULL\n    dmax: active binding\n    dmin: active binding\n    getParameterValue: function (id, error = "warn") \n    inf: active binding\n    initialize: function (name = NULL, short_name = NULL, type, support = NULL, \n    iqr: function () \n    kurtosisType: active binding\n    liesInSupport: function (x, all = TRUE, bound = FALSE) \n    liesInType: function (x, all = TRUE, bound = FALSE) \n    median: function (na.rm = NULL, ...) \n    name: Histogram Estimator\n    parameters: function (id = NULL) \n    pdf: function (..., log = FALSE, simplify = TRUE, data = NULL) \n    prec: function () \n    print: function (n = 2, ...) \n    properties: active binding\n    quantile: function (..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, \n    rand: function (n, simplify = TRUE) \n    setParameterValue: function (..., lst = list(...), error = "warn", resolveConflicts = FALSE) \n    short_name: Histogram\n    skewnessType: active binding\n    stdev: function () \n    strprint: function (n = 2) \n    summary: function (full = TRUE, ...) \n    sup: active binding\n    support: active binding\n    symmetry: active binding\n    traits: active binding\n    type: active binding\n    valueSupport: active binding\n    variateForm: active binding\n    workingSupport: function () \n  Private:\n    .cdf: function (x, self = <environment>, ...) \n    .decorators: NULL\n    .isCdf: 1\n    .isPdf: 1\n    .isQuantile: 0\n    .isRand: 0\n    .log: FALSE\n    .parameters: NULL\n    .pdf: function (x, self = <environment>, ...) \n    .properties: list\n    .traits: list\n    .updateDecorators: function (decs) \n    .variates: 1
#> 2: <Distribution>\n  Public:\n    alias: \n    cdf: function (..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, \n    clone: function (deep = FALSE) \n    confidence: function (alpha = 0.95, sides = "both", median = FALSE) \n    correlation: function () \n    decorators: active binding\n    description: NULL\n    dmax: active binding\n    dmin: active binding\n    getParameterValue: function (id, error = "warn") \n    inf: active binding\n    initialize: function (name = NULL, short_name = NULL, type, support = NULL, \n    iqr: function () \n    kurtosisType: active binding\n    liesInSupport: function (x, all = TRUE, bound = FALSE) \n    liesInType: function (x, all = TRUE, bound = FALSE) \n    median: function (na.rm = NULL, ...) \n    name: Histogram Estimator\n    parameters: function (id = NULL) \n    pdf: function (..., log = FALSE, simplify = TRUE, data = NULL) \n    prec: function () \n    print: function (n = 2, ...) \n    properties: active binding\n    quantile: function (..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, \n    rand: function (n, simplify = TRUE) \n    setParameterValue: function (..., lst = list(...), error = "warn", resolveConflicts = FALSE) \n    short_name: Histogram\n    skewnessType: active binding\n    stdev: function () \n    strprint: function (n = 2) \n    summary: function (full = TRUE, ...) \n    sup: active binding\n    support: active binding\n    symmetry: active binding\n    traits: active binding\n    type: active binding\n    valueSupport: active binding\n    variateForm: active binding\n    workingSupport: function () \n  Private:\n    .cdf: function (x, self = <environment>, ...) \n    .decorators: NULL\n    .isCdf: 1\n    .isPdf: 1\n    .isQuantile: 0\n    .isRand: 0\n    .log: FALSE\n    .parameters: NULL\n    .pdf: function (x, self = <environment>, ...) \n    .properties: list\n    .traits: list\n    .updateDecorators: function (decs) \n    .variates: 1
#> 3: <Distribution>\n  Public:\n    alias: \n    cdf: function (..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, \n    clone: function (deep = FALSE) \n    confidence: function (alpha = 0.95, sides = "both", median = FALSE) \n    correlation: function () \n    decorators: active binding\n    description: NULL\n    dmax: active binding\n    dmin: active binding\n    getParameterValue: function (id, error = "warn") \n    inf: active binding\n    initialize: function (name = NULL, short_name = NULL, type, support = NULL, \n    iqr: function () \n    kurtosisType: active binding\n    liesInSupport: function (x, all = TRUE, bound = FALSE) \n    liesInType: function (x, all = TRUE, bound = FALSE) \n    median: function (na.rm = NULL, ...) \n    name: Histogram Estimator\n    parameters: function (id = NULL) \n    pdf: function (..., log = FALSE, simplify = TRUE, data = NULL) \n    prec: function () \n    print: function (n = 2, ...) \n    properties: active binding\n    quantile: function (..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, \n    rand: function (n, simplify = TRUE) \n    setParameterValue: function (..., lst = list(...), error = "warn", resolveConflicts = FALSE) \n    short_name: Histogram\n    skewnessType: active binding\n    stdev: function () \n    strprint: function (n = 2) \n    summary: function (full = TRUE, ...) \n    sup: active binding\n    support: active binding\n    symmetry: active binding\n    traits: active binding\n    type: active binding\n    valueSupport: active binding\n    variateForm: active binding\n    workingSupport: function () \n  Private:\n    .cdf: function (x, self = <environment>, ...) \n    .decorators: NULL\n    .isCdf: 1\n    .isPdf: 1\n    .isQuantile: 0\n    .isRand: 0\n    .log: FALSE\n    .parameters: NULL\n    .pdf: function (x, self = <environment>, ...) \n    .properties: list\n    .traits: list\n    .updateDecorators: function (decs) \n    .variates: 1
#> 4: <Distribution>\n  Public:\n    alias: \n    cdf: function (..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, \n    clone: function (deep = FALSE) \n    confidence: function (alpha = 0.95, sides = "both", median = FALSE) \n    correlation: function () \n    decorators: active binding\n    description: NULL\n    dmax: active binding\n    dmin: active binding\n    getParameterValue: function (id, error = "warn") \n    inf: active binding\n    initialize: function (name = NULL, short_name = NULL, type, support = NULL, \n    iqr: function () \n    kurtosisType: active binding\n    liesInSupport: function (x, all = TRUE, bound = FALSE) \n    liesInType: function (x, all = TRUE, bound = FALSE) \n    median: function (na.rm = NULL, ...) \n    name: Histogram Estimator\n    parameters: function (id = NULL) \n    pdf: function (..., log = FALSE, simplify = TRUE, data = NULL) \n    prec: function () \n    print: function (n = 2, ...) \n    properties: active binding\n    quantile: function (..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, \n    rand: function (n, simplify = TRUE) \n    setParameterValue: function (..., lst = list(...), error = "warn", resolveConflicts = FALSE) \n    short_name: Histogram\n    skewnessType: active binding\n    stdev: function () \n    strprint: function (n = 2) \n    summary: function (full = TRUE, ...) \n    sup: active binding\n    support: active binding\n    symmetry: active binding\n    traits: active binding\n    type: active binding\n    valueSupport: active binding\n    variateForm: active binding\n    workingSupport: function () \n  Private:\n    .cdf: function (x, self = <environment>, ...) \n    .decorators: NULL\n    .isCdf: 1\n    .isPdf: 1\n    .isQuantile: 0\n    .isRand: 0\n    .log: FALSE\n    .parameters: NULL\n    .pdf: function (x, self = <environment>, ...) \n    .properties: list\n    .traits: list\n    .updateDecorators: function (decs) \n    .variates: 1
#> 5: <Distribution>\n  Public:\n    alias: \n    cdf: function (..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, \n    clone: function (deep = FALSE) \n    confidence: function (alpha = 0.95, sides = "both", median = FALSE) \n    correlation: function () \n    decorators: active binding\n    description: NULL\n    dmax: active binding\n    dmin: active binding\n    getParameterValue: function (id, error = "warn") \n    inf: active binding\n    initialize: function (name = NULL, short_name = NULL, type, support = NULL, \n    iqr: function () \n    kurtosisType: active binding\n    liesInSupport: function (x, all = TRUE, bound = FALSE) \n    liesInType: function (x, all = TRUE, bound = FALSE) \n    median: function (na.rm = NULL, ...) \n    name: Histogram Estimator\n    parameters: function (id = NULL) \n    pdf: function (..., log = FALSE, simplify = TRUE, data = NULL) \n    prec: function () \n    print: function (n = 2, ...) \n    properties: active binding\n    quantile: function (..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, \n    rand: function (n, simplify = TRUE) \n    setParameterValue: function (..., lst = list(...), error = "warn", resolveConflicts = FALSE) \n    short_name: Histogram\n    skewnessType: active binding\n    stdev: function () \n    strprint: function (n = 2) \n    summary: function (full = TRUE, ...) \n    sup: active binding\n    support: active binding\n    symmetry: active binding\n    traits: active binding\n    type: active binding\n    valueSupport: active binding\n    variateForm: active binding\n    workingSupport: function () \n  Private:\n    .cdf: function (x, self = <environment>, ...) \n    .decorators: NULL\n    .isCdf: 1\n    .isPdf: 1\n    .isQuantile: 0\n    .isRand: 0\n    .log: FALSE\n    .parameters: NULL\n    .pdf: function (x, self = <environment>, ...) \n    .properties: list\n    .traits: list\n    .updateDecorators: function (decs) \n    .variates: 1
#> 6: <Distribution>\n  Public:\n    alias: \n    cdf: function (..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, \n    clone: function (deep = FALSE) \n    confidence: function (alpha = 0.95, sides = "both", median = FALSE) \n    correlation: function () \n    decorators: active binding\n    description: NULL\n    dmax: active binding\n    dmin: active binding\n    getParameterValue: function (id, error = "warn") \n    inf: active binding\n    initialize: function (name = NULL, short_name = NULL, type, support = NULL, \n    iqr: function () \n    kurtosisType: active binding\n    liesInSupport: function (x, all = TRUE, bound = FALSE) \n    liesInType: function (x, all = TRUE, bound = FALSE) \n    median: function (na.rm = NULL, ...) \n    name: Histogram Estimator\n    parameters: function (id = NULL) \n    pdf: function (..., log = FALSE, simplify = TRUE, data = NULL) \n    prec: function () \n    print: function (n = 2, ...) \n    properties: active binding\n    quantile: function (..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, \n    rand: function (n, simplify = TRUE) \n    setParameterValue: function (..., lst = list(...), error = "warn", resolveConflicts = FALSE) \n    short_name: Histogram\n    skewnessType: active binding\n    stdev: function () \n    strprint: function (n = 2) \n    summary: function (full = TRUE, ...) \n    sup: active binding\n    support: active binding\n    symmetry: active binding\n    traits: active binding\n    type: active binding\n    valueSupport: active binding\n    variateForm: active binding\n    workingSupport: function () \n  Private:\n    .cdf: function (x, self = <environment>, ...) \n    .decorators: NULL\n    .isCdf: 1\n    .isPdf: 1\n    .isQuantile: 0\n    .isRand: 0\n    .log: FALSE\n    .parameters: NULL\n    .pdf: function (x, self = <environment>, ...) \n    .properties: list\n    .traits: list\n    .updateDecorators: function (decs) \n    .variates: 1
```

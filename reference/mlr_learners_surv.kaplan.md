# Kaplan-Meier Estimator Survival Learner

Calls
[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).

- distr is predicted by estimating the survival function with
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)

- `crank` is predicted as the sum of the cumulative hazard function
  (expected mortality) derived from the survival distribution, `distr`

## Dictionary

This [Learner](https://mlr3.mlr-org.com/reference/Learner.html) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_learners](https://mlr3.mlr-org.com/reference/mlr_learners.html) or
with the associated sugar function
[lrn()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    LearnerSurvKaplan$new()
    mlr_learners$get("surv.kaplan")
    lrn("surv.kaplan")

## Meta Information

- Task type: “surv”

- Predict Types: “crank”, “distr”

- Feature Types: “logical”, “integer”, “numeric”, “character”, “factor”,
  “ordered”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3),
  [mlr3proba](https://CRAN.R-project.org/package=mlr3proba),
  [survival](https://CRAN.R-project.org/package=survival),
  [distr6](https://CRAN.R-project.org/package=distr6)

## Parameters

Empty ParamSet

## References

Kaplan EL, Meier P (1958). “Nonparametric Estimation from Incomplete
Observations.” *Journal of the American Statistical Association*,
**53**(282), 457–481.
[doi:10.1080/01621459.1958.10501452](https://doi.org/10.1080/01621459.1958.10501452)
.

## See also

Other survival learners:
[`mlr_learners_surv.coxph`](https://mlr3proba.mlr-org.com/reference/mlr_learners_surv.coxph.md),
[`mlr_learners_surv.rpart`](https://mlr3proba.mlr-org.com/reference/mlr_learners_surv.rpart.md)

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`mlr3proba::LearnerSurv`](https://mlr3proba.mlr-org.com/reference/LearnerSurv.md)
-\> `LearnerSurvKaplan`

## Methods

### Public methods

- [`LearnerSurvKaplan$new()`](#method-LearnerSurvKaplan-new)

- [`LearnerSurvKaplan$importance()`](#method-LearnerSurvKaplan-importance)

- [`LearnerSurvKaplan$selected_features()`](#method-LearnerSurvKaplan-selected_features)

- [`LearnerSurvKaplan$clone()`](#method-LearnerSurvKaplan-clone)

Inherited methods

- [`mlr3::Learner$base_learner()`](https://mlr3.mlr-org.com/reference/Learner.html#method-base_learner)
- [`mlr3::Learner$configure()`](https://mlr3.mlr-org.com/reference/Learner.html#method-configure)
- [`mlr3::Learner$encapsulate()`](https://mlr3.mlr-org.com/reference/Learner.html#method-encapsulate)
- [`mlr3::Learner$format()`](https://mlr3.mlr-org.com/reference/Learner.html#method-format)
- [`mlr3::Learner$help()`](https://mlr3.mlr-org.com/reference/Learner.html#method-help)
- [`mlr3::Learner$predict()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict)
- [`mlr3::Learner$predict_newdata()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict_newdata)
- [`mlr3::Learner$print()`](https://mlr3.mlr-org.com/reference/Learner.html#method-print)
- [`mlr3::Learner$reset()`](https://mlr3.mlr-org.com/reference/Learner.html#method-reset)
- [`mlr3::Learner$train()`](https://mlr3.mlr-org.com/reference/Learner.html#method-train)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://rdrr.io/pkg/R6/man/R6Class.html) class.

#### Usage

    LearnerSurvKaplan$new()

------------------------------------------------------------------------

### Method `importance()`

All features have a score of `0` for this learner. This method exists
solely for compatibility with the `mlr3` ecosystem, as this learner is
used as a fallback for other survival learners that require an
`importance()` method.

#### Usage

    LearnerSurvKaplan$importance()

#### Returns

Named [`numeric()`](https://rdrr.io/r/base/numeric.html).

------------------------------------------------------------------------

### Method `selected_features()`

Selected features are always the empty set for this learner. This method
is implemented only for compatibility with the `mlr3` API, as this
learner does not perform feature selection.

#### Usage

    LearnerSurvKaplan$selected_features()

#### Returns

`character(0)`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerSurvKaplan$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

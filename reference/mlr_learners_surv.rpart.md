# Rpart Survival Trees Survival Learner

Calls [`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html).

- crank is predicted using
  [`rpart::predict.rpart()`](https://rdrr.io/pkg/rpart/man/predict.rpart.html)

## Dictionary

This [Learner](https://mlr3.mlr-org.com/reference/Learner.html) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_learners](https://mlr3.mlr-org.com/reference/mlr_learners.html) or
with the associated sugar function
[lrn()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    LearnerSurvRpart$new()
    mlr_learners$get("surv.rpart")
    lrn("surv.rpart")

## Meta Information

- Task type: “surv”

- Predict Types: “crank”

- Feature Types: “logical”, “integer”, “numeric”, “character”, “factor”,
  “ordered”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3),
  [mlr3proba](https://CRAN.R-project.org/package=mlr3proba),
  [rpart](https://CRAN.R-project.org/package=rpart),
  [distr6](https://CRAN.R-project.org/package=distr6),
  [survival](https://CRAN.R-project.org/package=survival)

## Parameters

|                |         |         |             |                       |
|----------------|---------|---------|-------------|-----------------------|
| Id             | Type    | Default | Levels      | Range                 |
| parms          | numeric | 1       |             | \\(-\infty, \infty)\\ |
| minbucket      | integer | \-      |             | \\\[1, \infty)\\      |
| minsplit       | integer | 20      |             | \\\[1, \infty)\\      |
| cp             | numeric | 0.01    |             | \\\[0, 1\]\\          |
| maxcompete     | integer | 4       |             | \\\[0, \infty)\\      |
| maxsurrogate   | integer | 5       |             | \\\[0, \infty)\\      |
| maxdepth       | integer | 30      |             | \\\[1, 30\]\\         |
| usesurrogate   | integer | 2       |             | \\\[0, 2\]\\          |
| surrogatestyle | integer | 0       |             | \\\[0, 1\]\\          |
| xval           | integer | 10      |             | \\\[0, \infty)\\      |
| cost           | untyped | \-      |             | \-                    |
| keep_model     | logical | FALSE   | TRUE, FALSE | \-                    |

## Initial parameter values

- `xval` is set to 0 in order to save some computation time.

- `model` has been renamed to `keep_model`.

## References

Breiman L, Friedman JH, Olshen RA, Stone CJ (1984). *Classification And
Regression Trees*. Routledge.
[doi:10.1201/9781315139470](https://doi.org/10.1201/9781315139470) .

## See also

Other survival learners:
[`mlr_learners_surv.coxph`](https://mlr3proba.mlr-org.com/reference/mlr_learners_surv.coxph.md),
[`mlr_learners_surv.kaplan`](https://mlr3proba.mlr-org.com/reference/mlr_learners_surv.kaplan.md)

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`mlr3proba::LearnerSurv`](https://mlr3proba.mlr-org.com/reference/LearnerSurv.md)
-\> `LearnerSurvRpart`

## Methods

### Public methods

- [`LearnerSurvRpart$new()`](#method-LearnerSurvRpart-new)

- [`LearnerSurvRpart$importance()`](#method-LearnerSurvRpart-importance)

- [`LearnerSurvRpart$selected_features()`](#method-LearnerSurvRpart-selected_features)

- [`LearnerSurvRpart$clone()`](#method-LearnerSurvRpart-clone)

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
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerSurvRpart$new()

------------------------------------------------------------------------

### Method `importance()`

The importance scores are extracted from the model slot
`variable.importance`.

#### Usage

    LearnerSurvRpart$importance()

#### Returns

Named [`numeric()`](https://rdrr.io/r/base/numeric.html).

------------------------------------------------------------------------

### Method `selected_features()`

Selected features are extracted from the model slot `frame$var`.

#### Usage

    LearnerSurvRpart$selected_features()

#### Returns

[`character()`](https://rdrr.io/r/base/character.html).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerSurvRpart$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

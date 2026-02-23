# Survival Learner

This Learner specializes
[Learner](https://mlr3.mlr-org.com/reference/Learner.html) for survival
problems:

- `task_type` is set to `"surv"`

- Creates
  [Prediction](https://mlr3.mlr-org.com/reference/Prediction.html)s of
  class
  [PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md).

- Possible values for `predict_types` are:

  - `"distr"`: Predicts a probability distribution for each observation
    in the test set, uses
    [distr6](https://xoopr.github.io/distr6/reference/distr6-package.html).

  - `"lp"`: Predicts a linear predictor for each observation in the test
    set.

  - `"crank"`: Predicts a continuous ranking for each observation in the
    test set.

  - `"response"`: Predicts a survival time for each observation in the
    test set.

## See also

Other Learner:
[`LearnerDens`](https://mlr3proba.mlr-org.com/reference/LearnerDens.md)

## Super class

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
`LearnerSurv`

## Methods

### Public methods

- [`LearnerSurv$new()`](#method-LearnerSurv-new)

- [`LearnerSurv$clone()`](#method-LearnerSurv-clone)

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
- [`mlr3::Learner$selected_features()`](https://mlr3.mlr-org.com/reference/Learner.html#method-selected_features)
- [`mlr3::Learner$train()`](https://mlr3.mlr-org.com/reference/Learner.html#method-train)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://rdrr.io/pkg/R6/man/R6Class.html) class.

#### Usage

    LearnerSurv$new(
      id,
      param_set = ps(),
      predict_types = "distr",
      feature_types = character(),
      properties = character(),
      packages = character(),
      label = NA_character_,
      man = NA_character_
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of hyperparameters.

- `predict_types`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Supported predict types. Must be a subset of
  [`mlr_reflections$learner_predict_types`](https://mlr3.mlr-org.com/reference/mlr_reflections.html).

- `feature_types`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Feature types the learner operates on. Must be a subset of
  [`mlr_reflections$task_feature_types`](https://mlr3.mlr-org.com/reference/mlr_reflections.html).

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of properties of the
  [Learner](https://mlr3.mlr-org.com/reference/Learner.html) (see
  initialization method `$new()`. Must be a subset of
  [`mlr_reflections$learner_properties`](https://mlr3.mlr-org.com/reference/mlr_reflections.html).

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of required packages. A warning is signaled by the constructor if
  at least one of the packages is not installed, but loaded (not
  attached) later on-demand via
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html).

- `label`:

  (`character(1)`)  
  Label for the new instance.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. The referenced help package can be opened via method
  `$help()`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerSurv$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
# get all survival learners from mlr_learners:
lrns = mlr_learners$mget(mlr_learners$keys("^surv"))
names(lrns)
#> [1] "surv.coxph"  "surv.kaplan" "surv.rpart" 

# get a specific learner from mlr_learners:
mlr_learners$get("surv.coxph")
#> 
#> ── <LearnerSurvCoxPH> (surv.coxph): Cox Proportional Hazards ───────────────────
#> • Model: -
#> • Parameters: list()
#> • Packages: mlr3, mlr3proba, survival, and distr6
#> • Predict Types: [crank], distr, and lp
#> • Feature Types: logical, integer, numeric, and factor
#> • Encapsulation: none (fallback: -)
#> • Properties: weights
#> • Other settings: use_weights = 'use'
lrn("surv.coxph")
#> 
#> ── <LearnerSurvCoxPH> (surv.coxph): Cox Proportional Hazards ───────────────────
#> • Model: -
#> • Parameters: list()
#> • Packages: mlr3, mlr3proba, survival, and distr6
#> • Predict Types: [crank], distr, and lp
#> • Feature Types: logical, integer, numeric, and factor
#> • Encapsulation: none (fallback: -)
#> • Properties: weights
#> • Other settings: use_weights = 'use'
```

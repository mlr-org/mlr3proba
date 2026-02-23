# Density Learner

This Learner specializes
[Learner](https://mlr3.mlr-org.com/reference/Learner.html) for density
estimation problems:

- `task_type` is set to `"dens"`

- Creates
  [Prediction](https://mlr3.mlr-org.com/reference/Prediction.html)s of
  class
  [PredictionDens](https://mlr3proba.mlr-org.com/reference/PredictionDens.md).

- Possible values for `predict_types` are:

  - `"pdf"`: Evaluates estimated probability density function for each
    value in the test set.

  - `"cdf"`: Evaluates estimated cumulative distribution function for
    each value in the test set.

## See also

Other Learner:
[`LearnerSurv`](https://mlr3proba.mlr-org.com/reference/LearnerSurv.md)

## Super class

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
`LearnerDens`

## Methods

### Public methods

- [`LearnerDens$new()`](#method-LearnerDens-new)

- [`LearnerDens$clone()`](#method-LearnerDens-clone)

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

    LearnerDens$new(
      id,
      param_set = ps(),
      predict_types = "cdf",
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

    LearnerDens$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
# get all density learners from mlr_learners:
lrns = mlr_learners$mget(mlr_learners$keys("^dens"))
names(lrns)
#>  [1] "dens.hist"      "dens.kde"       "dens.kde_ks"    "dens.locfit"   
#>  [5] "dens.logspline" "dens.mixed"     "dens.nonpar"    "dens.pen"      
#>  [9] "dens.plug"      "dens.spline"   

# get a specific learner from mlr_learners:
mlr_learners$get("dens.hist")
#> 
#> ── <LearnerDensHistogram> (dens.hist): Histogram Density Estimator ─────────────
#> • Model: -
#> • Parameters: list()
#> • Packages: mlr3, mlr3proba, and distr6
#> • Predict Types: [pdf], cdf, and distr
#> • Feature Types: integer and numeric
#> • Encapsulation: none (fallback: -)
#> • Properties:
#> • Other settings: use_weights = 'error'
lrn("dens.hist")
#> 
#> ── <LearnerDensHistogram> (dens.hist): Histogram Density Estimator ─────────────
#> • Model: -
#> • Parameters: list()
#> • Packages: mlr3, mlr3proba, and distr6
#> • Predict Types: [pdf], cdf, and distr
#> • Feature Types: integer and numeric
#> • Encapsulation: none (fallback: -)
#> • Properties:
#> • Other settings: use_weights = 'error'
```

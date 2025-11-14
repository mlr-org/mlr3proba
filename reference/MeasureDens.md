# Density Measure

This measure specializes
[Measure](https://mlr3.mlr-org.com/reference/Measure.html) for survival
problems.

- `task_type` is set to `"dens"`.

- Possible values for `predict_type` are `"pdf"` and `"cdf"`.

Predefined measures can be found in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr3::mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.html).

## See also

Default density measures:
[`dens.logloss`](https://mlr3proba.mlr-org.com/reference/mlr_measures_dens.logloss.md)

Other Measure:
[`MeasureSurv`](https://mlr3proba.mlr-org.com/reference/MeasureSurv.md)

## Super class

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.html) -\>
`MeasureDens`

## Methods

### Public methods

- [`MeasureDens$new()`](#method-MeasureDens-new)

- [`MeasureDens$clone()`](#method-MeasureDens-clone)

Inherited methods

- [`mlr3::Measure$aggregate()`](https://mlr3.mlr-org.com/reference/Measure.html#method-aggregate)
- [`mlr3::Measure$format()`](https://mlr3.mlr-org.com/reference/Measure.html#method-format)
- [`mlr3::Measure$help()`](https://mlr3.mlr-org.com/reference/Measure.html#method-help)
- [`mlr3::Measure$print()`](https://mlr3.mlr-org.com/reference/Measure.html#method-print)
- [`mlr3::Measure$score()`](https://mlr3.mlr-org.com/reference/Measure.html#method-score)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    MeasureDens$new(
      id,
      param_set = ps(),
      range,
      minimize = NA,
      aggregator = NULL,
      properties = character(),
      predict_type = "pdf",
      task_properties = character(),
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

- `range`:

  (`numeric(2)`)  
  Feasible range for this measure as `c(lower_bound, upper_bound)`. Both
  bounds may be infinite.

- `minimize`:

  (`logical(1)`)  
  Set to `TRUE` if good predictions correspond to small values, and to
  `FALSE` if good predictions correspond to large values. If set to `NA`
  (default), tuning this measure is not possible.

- `aggregator`:

  (`function(x)`)  
  Function to aggregate individual performance scores `x` where `x` is a
  numeric vector. If `NULL`, defaults to
  [`mean()`](https://rdrr.io/r/base/mean.html).

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Properties of the measure. Must be a subset of
  [mlr_reflections\$measure_properties](https://mlr3.mlr-org.com/reference/mlr_reflections.html).
  Supported by `mlr3`:

  - `"requires_task"` (requires the complete
    [Task](https://mlr3.mlr-org.com/reference/Task.html)),

  - `"requires_learner"` (requires the trained
    [Learner](https://mlr3.mlr-org.com/reference/Learner.html)),

  - `"requires_train_set"` (requires the training indices from the
    [Resampling](https://mlr3.mlr-org.com/reference/Resampling.html)),
    and

  - `"na_score"` (the measure is expected to occasionally return `NA` or
    `NaN`).

- `predict_type`:

  (`character(1)`)  
  Required predict type of the
  [Learner](https://mlr3.mlr-org.com/reference/Learner.html). Possible
  values are stored in
  [mlr_reflections\$learner_predict_types](https://mlr3.mlr-org.com/reference/mlr_reflections.html).

- `task_properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Required task properties, see
  [Task](https://mlr3.mlr-org.com/reference/Task.html).

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

    MeasureDens$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

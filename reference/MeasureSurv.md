# Survival Measure

This measure specializes
[Measure](https://mlr3.mlr-org.com/reference/Measure.html) for survival
problems.

- `task_type` is set to `"surv"`.

- Possible values for `predict_type` are `"distr"`, `"lp"`, `"crank"`,
  and `"response"`.

Predefined measures can be found in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr3::mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.html).

## See also

Default survival measure:
[`surv.cindex`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.cindex.md)

Other Measure:
[`MeasureDens`](https://mlr3proba.mlr-org.com/reference/MeasureDens.md)

## Super class

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.html) -\>
`MeasureSurv`

## Methods

### Public methods

- [`MeasureSurv$new()`](#method-MeasureSurv-new)

- [`MeasureSurv$clone()`](#method-MeasureSurv-clone)

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

    MeasureSurv$new(
      id,
      param_set = ps(),
      range,
      minimize = NA,
      average = "macro",
      aggregator = NULL,
      obs_loss = NULL,
      properties = character(),
      predict_type = "distr",
      predict_sets = "test",
      task_properties = character(),
      packages = character(),
      label = NA_character_,
      man = NA_character_,
      trafo = NULL
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

- `average`:

  (`character(1)`)  
  How to average multiple
  [Prediction](https://mlr3.mlr-org.com/reference/Prediction.html)s from
  a
  [ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html).

  The default, `"macro"`, calculates the individual performances scores
  for each
  [Prediction](https://mlr3.mlr-org.com/reference/Prediction.html) and
  then uses the function defined in `$aggregator` to average them to a
  single number.

  If set to `"micro"`, the individual
  [Prediction](https://mlr3.mlr-org.com/reference/Prediction.html)
  objects are first combined into a single new
  [Prediction](https://mlr3.mlr-org.com/reference/Prediction.html)
  object which is then used to assess the performance. The function in
  `$aggregator` is not used in this case.

- `aggregator`:

  (`function(x)`)  
  Function to aggregate individual performance scores `x` where `x` is a
  numeric vector. If `NULL`, defaults to
  [`mean()`](https://rdrr.io/r/base/mean.html).

- `obs_loss`:

  (`function` or `NULL`)  
  The observation-wise loss function, e.g.
  [zero-one](https://mlr3measures.mlr-org.com/reference/zero_one.html)
  for classification error.

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

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Prediction sets to operate on, used in
  [`aggregate()`](https://rdrr.io/r/stats/aggregate.html) to extract the
  matching `predict_sets` from the
  [ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html).
  Multiple predict sets are calculated by the respective
  [Learner](https://mlr3.mlr-org.com/reference/Learner.html) during
  [resample()](https://mlr3.mlr-org.com/reference/resample.html)/[benchmark()](https://mlr3.mlr-org.com/reference/benchmark.html).
  Must be a non-empty subset of `{"train", "test"}`. If multiple sets
  are provided, these are first combined to a single prediction object.
  Default is `"test"`.

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

- `trafo`:

  ([`list()`](https://rdrr.io/r/base/list.html) or `NULL`)  
  An optional list with two elements, containing the transformation
  `"fn"` and its derivative `"deriv"`. The transformation function is
  the function that is applied after aggregating the pointwise losses,
  i.e. this requires an `$obs_loss` to be present. An example is `sqrt`
  for RMSE (regression).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureSurv$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

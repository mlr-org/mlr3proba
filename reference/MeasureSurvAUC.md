# Abstract Class for survAUC Measures

This is an abstract class that should not be constructed directly.

## Parameter details

- `integrated` (`logical(1)`)  
  If `TRUE` (default), returns the integrated score (eg across time
  points); otherwise, not integrated (eg at a single time point).

&nbsp;

- `times` ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  If `integrated == TRUE` then a vector of time-points over which to
  integrate the score. If `integrated == FALSE` then a single time point
  at which to return the score.

## Super classes

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.html) -\>
[`mlr3proba::MeasureSurv`](https://mlr3proba.mlr-org.com/reference/MeasureSurv.md)
-\> `MeasureSurvAUC`

## Methods

### Public methods

- [`MeasureSurvAUC$new()`](#method-MeasureSurvAUC-new)

- [`MeasureSurvAUC$clone()`](#method-MeasureSurvAUC-clone)

Inherited methods

- [`mlr3::Measure$aggregate()`](https://mlr3.mlr-org.com/reference/Measure.html#method-aggregate)
- [`mlr3::Measure$format()`](https://mlr3.mlr-org.com/reference/Measure.html#method-format)
- [`mlr3::Measure$help()`](https://mlr3.mlr-org.com/reference/Measure.html#method-help)
- [`mlr3::Measure$print()`](https://mlr3.mlr-org.com/reference/Measure.html#method-print)
- [`mlr3::Measure$score()`](https://mlr3.mlr-org.com/reference/Measure.html#method-score)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://rdrr.io/pkg/R6/man/R6Class.html) class.

#### Usage

    MeasureSurvAUC$new(
      id,
      properties = character(),
      label = NA_character_,
      man = NA_character_,
      param_set = ps()
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

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

- `label`:

  (`character(1)`)  
  Label for the new instance.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. The referenced help package can be opened via method
  `$help()`.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of hyperparameters.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureSurvAUC$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

#' @param properties (`character()`)\cr
#'   Properties of the measure.
#'   Must be a subset of [mlr_reflections$measure_properties][mlr3::mlr_reflections].
#'   Supported by `mlr3`:
#'   * `"requires_task"` (requires the complete [Task][mlr3::Task]),
#'   * `"requires_learner"` (requires the trained [Learner][mlr3::Learner]),
#'   * `"requires_train_set"` (requires the training indices from the [Resampling][mlr3::Resampling]), and
#'   * `"na_score"` (the measure is expected to occasionally return `NA` or `NaN`).

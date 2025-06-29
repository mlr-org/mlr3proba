#' @title Density Learner
#'
#' @description
#' This Learner specializes [Learner][mlr3::Learner] for density estimation problems:
#'
#' * `task_type` is set to `"dens"`
#' * Creates [Prediction][mlr3::Prediction]s of class [PredictionDens].
#' * Possible values for `predict_types` are:
#'   - `"pdf"`: Evaluates estimated probability density function for each value in the test set.
#'   - `"cdf"`: Evaluates estimated cumulative distribution function for each value in the test set.
#'
#' @template param_id
#' @template param_param_set
#' @template param_predict_types
#' @template param_feature_types
#' @template param_learner_properties
#' @template param_packages
#' @template param_label
#' @template param_man
#'
#' @family Learner
#' @export
#' @examples
#' library(mlr3)
#' # get all density learners from mlr_learners:
#' lrns = mlr_learners$mget(mlr_learners$keys("^dens"))
#' names(lrns)
#'
#' # get a specific learner from mlr_learners:
#' mlr_learners$get("dens.hist")
#' lrn("dens.hist")
LearnerDens = R6Class("LearnerDens",
  inherit = Learner,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, param_set = ps(),
      predict_types = "cdf", feature_types = character(),
      properties = character(),
      packages = character(),
      label = NA_character_,
      man = NA_character_) {
      super$initialize(
        id = id, task_type = "dens", param_set = param_set,
        predict_types = predict_types, feature_types = feature_types, properties = properties,
        packages = c("mlr3proba", packages), label = label, man = man)
    }
  )
)

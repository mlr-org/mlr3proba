#' @title Survival Learner
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3::Learner].
#'
#' @description
#' This Learner specializes [mlr3::Learner] for survival problems.
#' Predefined learners can be found in the [mlr3misc::Dictionary] [mlr3::mlr_learners].
#'
#' @section Construction:
#' ```
#' LearnerSurv$new(id, param_set = ParamSet$new(),
#'      predict_types = character(),
#'      feature_types = character(), properties = character(),
#'      packages = character())
#' ```
#' For a description of the arguments, see [mlr3::Learner].
#' `task_type` is set to `"surv"`.
#' Possible values for `predict_type` are `"distr"`, `"lp"`, `"crank"`, and `"response"`.
#'
#' @section Fields:
#' See [mlr3::Learner].
#'
#' @section Methods:
#' See [mlr3::Learner].
#'
#' @family Learner
#' @export
#' @examples
#' library(mlr3)
#' ids = mlr_learners$keys("^surv")
#' ids
#'
#' # get a specific learner from mlr_learners:
#' lrn = mlr_learners$get("surv.rpart")
#' print(lrn)
LearnerSurv = R6Class("LearnerSurv", inherit = Learner,
                      public = list(
                        initialize = function(id, param_set = ParamSet$new(), predict_types = "distr", feature_types = character(), properties = character(), packages = character()) {
                          super$initialize(id = id, task_type = "surv", param_set = param_set, predict_types = predict_types,
                                           feature_types = feature_types, properties = properties, packages = packages)
                        }
                      )
)

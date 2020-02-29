#' @title Density Learner
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Learner].
#'
#' @description
#' This Learner specializes [Learner] for density estimation problems.
#'
#' @section Construction:
#' ```
#' l = LearnerDens$new(id, param_set = ParamSet$new(), predict_types = "cdf",
#' feature_types = character(), properties = character(), data_formats = "data.table",
#' packages = character())
#' ```
#' For a description of the arguments, see [Learner].
#' `task_type` is set to `"dens"`.
#'
#' Possible values for `predict_types` are passed to and converted by [PredictionDens]:
#' * `"pdf"`: Evaluates the estimated probability density function for each value in the test set.
#' * `"cdf"`: Evaluates the estimated cumulative distribution function for each value in the test set.
#'
#' @section Fields:
#' See [Learner].
#'
#' @section Methods:
#' See [Learner].
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
LearnerDens = R6::R6Class("LearnerDens", inherit = Learner)
LearnerDens$set("public","initialize", function(id, param_set = ParamSet$new(),
                                                   predict_types = "cdf", feature_types = character(),
                                                   properties = character(), data_formats = "data.table",
                                                   packages = character()){
                             super$initialize(id = id, task_type = "dens", param_set = param_set,
                                              predict_types = predict_types, feature_types = feature_types, properties = properties,
                                              data_formats = data_formats, packages = packages)
                           })

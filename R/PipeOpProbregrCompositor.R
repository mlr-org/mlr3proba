#' @title PipeOpProbregrCompositor
#' @name mlr_pipeops_compose_probregr
#' @template param_pipelines
#'
#' @description
#' Combines a predicted `reponse` and `se` from [PredictionRegr] with a specified probability
#' distribution to estimate (or 'compose') a `distr` prediction.
#'
#' @section Dictionary:
#' This [PipeOp][mlr3pipelines::PipeOp] can be instantiated via the
#' [dictionary][mlr3misc::Dictionary] [mlr3pipelines::mlr_pipeops] or with the associated sugar
#' function [mlr3pipelines::po()]:
#' ```
#' PipeOpProbregrCompositor$new()
#' mlr_pipeops$get("compose_probregr")
#' po("compose_probregr")
#' ```
#'
#' @section Input and Output Channels:
#' [PipeOpProbregrCompositor] has two input channels named `"input_response"` and `"input_se"`,
#' which take `NULL` during training and two [PredictionRegr]s during prediction, these should
#' respectively contain the `response` and `se` return type, the same object can be passed twice.
#'
#' The output during prediction is a [PredictionRegr] with the "response" from `input_response`,
#' the "se" from `input_se` and a "distr" created from combining the two.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' * `dist` :: `character(1)` \cr
#'    Location-scale distribution to use for composition. Current choices are `"Normal"` (default),
#'     `"Cauchy"`, `"Gumbel"`, `"Laplace"`, `"Logistic"`. All implemented via \CRANpkg{distr6}.
#'
#' @section Internals:
#' The composition is created by substituting the `response` and `se` predictions into the
#' distribution location and scale parameters respectively.
#'
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE) &&
#' requireNamespace("rpart", quietly = TRUE)) {
#' library(mlr3)
#' library(mlr3pipelines)
#' set.seed(1)
#' task = tsk("boston_housing")
#'
#' # Option 1: Use a learner that can predict se
#' learn = lrn("regr.featureless", predict_type = "se")
#' pred = learn$train(task)$predict(task)
#' poc = po("compose_probregr")
#' poc$predict(list(pred, pred))[[1]]
#'
#' # Option 2: Use two learners, one for response and the other for se
#' learn_response = lrn("regr.rpart")
#' learn_se = lrn("regr.featureless", predict_type = "se")
#' pred_response = learn_response$train(task)$predict(task)
#' pred_se = learn_se$train(task)$predict(task)
#' poc = po("compose_probregr")
#' poc$predict(list(pred_response, pred_se))[[1]]
#' }
#' }
PipeOpProbregrCompositor = R6Class("PipeOpProbregrCompositor",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "compose_probregr", param_vals = list(dist = "Normal")) {
      ps = ParamSet$new(params = list(
        ParamFct$new("dist", default = "Normal",
                     levels = distr6::listDistributions(filter = list(Tags = "locscale"),
                                                        simplify = TRUE),
                     tags = "predict")
      ))

      super$initialize(
        id = id,
        param_set = ps,
        param_vals = param_vals,
        input = data.table(name = c("input_response", "input_se"), train = "NULL",
                           predict = c("PredictionRegr", "PredictionRegr")),
        output = data.table(name = "output", train = "NULL", predict = "PredictionRegr"),
        packages = "distr6"
      )
    }
  ),

  private =  list(
    .train = function(inputs) {
      self$state = list()
      list(NULL)
    },

    .predict = function(inputs) {
      pred_response = inputs$input_response
      pred_se = inputs$input_se

      if ("se" %nin% pred_se$predict_types) {
        stopf("'se' is not a predict_type in %s.", pred_se$id)
      }

      response = pred_response$response
      se = pred_se$se

      assert(all(pred_response$truth == pred_se$truth))
      assert(all(pred_response$row_ids == pred_se$row_ids))

      pv = self$param_set$values
      dist = pv$dist

      if (is.null(dist) || dist %in% c("Normal")) {
        params = data.table(mean = response, sd = se)
      } else if (dist %in% c("Cauchy", "Gumbel")) {
        params = data.table(location = response, scale = se)
      } else if (dist %in% c("Laplace", "Logistic")) {
        params = data.table(mean = response, scale = se)
      }


      list(PredictionRegr$new(row_ids = pred_response$row_ids,
                              truth = pred_response$truth,
                              response = response,
                              se = se,
                              distr = distr6::VectorDistribution$new(distribution = dist,
                                                                     params = params)))
    }
  )
)

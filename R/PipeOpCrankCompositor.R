#' @title PipeOpCrankCompositor
#' @name mlr_pipeops_crankcompose
#' @template param_pipelines
#'
#' @description
#' Uses a predicted `distr` in a [PredictionSurv] to estimate (or 'compose') a `crank` prediction.
#'
#' @section Dictionary:
#' This [PipeOp][mlr3pipelines::PipeOp] can be instantiated via the
#' [dictionary][mlr3misc::Dictionary] [mlr3pipelines::mlr_pipeops] or with the associated sugar
#' function [mlr3pipelines::po()]:
#' ```
#' PipeOpCrankCompositor$new()
#' mlr_pipeops$get("crankcompose")
#' po("crankcompose")
#' ```
#'
#' @section Input and Output Channels:
#' [PipeOpCrankCompositor] has one input channel named `"input"`, which takes `NULL` during training and [PredictionSurv] during prediction.
#'
#' [PipeOpCrankCompositor] has one output channel named `"output"`, producing `NULL` during training and a [PredictionSurv] during prediction.
#'
#' The output during prediction is the [PredictionSurv] from the input but with the `crank` predict type overwritten by the given estimation method.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' * `method` :: `character(1)` \cr
#'    Determines what method should be used to produce a continuous ranking from the distribution.
#'    Currently only `mort` is supported, which is the sum of the cumulative hazard, also called *expected/ensemble mortality*, see Ishwaran et al. (2008).
#'    For more details, see [get_mortality()].
#' * `overwrite` :: `logical(1)` \cr
#'    If `FALSE` (default) and the prediction already has a `crank` prediction, then the compositor returns the input prediction unchanged.
#'    If `TRUE`, then the `crank` will be overwritten.
#'
#' @seealso [pipeline_crankcompositor]
#' @family survival compositors
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#'   library(mlr3pipelines)
#'   task = tsk("rats")
#'
#'   # change the crank prediction type of a Cox's model predictions
#'   pred = lrn("surv.coxph")$train(task)$predict(task)
#'   poc = po("crankcompose", param_vals = list(overwrite = TRUE))
#'   poc$predict(list(pred))[[1L]]
#' }
#' }
#' @export
PipeOpCrankCompositor = R6Class("PipeOpCrankCompositor",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "crankcompose", param_vals = list()) {
      param_set = ps(
        method = p_fct(default = "mort", levels = c("mort"), tags = "predict"),
        overwrite = p_lgl(default = FALSE, tags = "predict")
      )
      param_set$set_values(method = "mort", overwrite = FALSE)

      super$initialize(
        id = id,
        param_set = param_set,
        param_vals = param_vals,
        input = data.table(name = "input", train = "NULL", predict = "PredictionSurv"),
        output = data.table(name = "output", train = "NULL", predict = "PredictionSurv"),
        packages = c("mlr3proba")
      )
    }
  ),

  private = list(
    .train = function(inputs) {
      self$state = list()
      list(NULL)
    },

    .predict = function(inputs) {
      pred = inputs[[1L]]
      overwrite = self$param_set$values$overwrite
      # it's impossible for a learner not to predict crank in mlr3proba,
      # but let's check either way:
      has_crank = !all(is.na(pred$crank))

      if (!overwrite & has_crank) {
        # return prediction as is
        return(list(pred))
      } else {
        # compose crank from distr prediction
        assert("distr" %in% pred$predict_types)

        # get survival matrix
        if (inherits(pred$data$distr, "array")) {
          surv = pred$data$distr
          if (length(dim(surv)) == 3L) {
            # survival 3d array, extract median
            surv = .ext_surv_mat(arr = surv, which.curve = 0.5)
          }
        } else {
          stop("Distribution prediction does not have a survival matrix or array
               in the $data$distr slot")
        }

        method = self$param_set$values$method
        if (method == "mort") {
          crank = get_mortality(surv)
        }

        p = PredictionSurv$new(
          row_ids = pred$row_ids,
          truth = pred$truth,
          crank = crank, # update only `crank`
          distr = pred$distr,
          lp = pred$lp,
          response = pred$response
        )

        return(list(p))
      }
    }
  )
)

register_pipeop("crankcompose", PipeOpCrankCompositor)

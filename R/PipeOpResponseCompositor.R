#' @title PipeOpResponseCompositor
#' @name mlr_pipeops_responsecompose
#' @template param_pipelines
#'
#' @description
#' Uses a predicted survival distribution (`distr`) in a [PredictionSurv] to estimate (or 'compose') an expected survival time (`response`) prediction.
#' Practically, this `PipeOp` summarizes an observation's survival curve/distribution to a single number which can be either the restricted mean survival time or the median survival time.
#'
#' @section Dictionary:
#' This [PipeOp][mlr3pipelines::PipeOp] can be instantiated via the
#' [dictionary][mlr3misc::Dictionary] [mlr3pipelines::mlr_pipeops] or with the associated sugar
#' function [mlr3pipelines::po()]:
#' ```
#' PipeOpResponseCompositor$new()
#' mlr_pipeops$get("responsecompose")
#' po("responsecompose")
#' ```
#'
#' @section Input and Output Channels:
#' [PipeOpResponseCompositor] has one input channel named `"input"`, which takes
#' `NULL` during training and [PredictionSurv] during prediction.
#'
#' [PipeOpResponseCompositor] has one output channel named `"output"`, producing `NULL` during training
#' and a [PredictionSurv] during prediction.
#'
#' The output during prediction is the [PredictionSurv] from the input but with the `response`
#' predict type overwritten by the given method.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' - `method` :: `character(1)` \cr
#'    Determines what method should be used to produce a survival time (response) from the survival distribution.
#'    Available methods are `"rmst"` and `"median"`, corresponding to the *restricted mean survival time* and the *median survival time* respectively.
#' - `tau` :: `numeric(1)` \cr
#'    Determines the time point up to which we calculate the restricted mean survival time (works only for the `"rmst"` method).
#'    If `NULL` (default), all the available time points in the predicted survival distribution will be used.
#' * `add_crank` :: `logical(1)` \cr
#'    If `TRUE` then `crank` predict type will be set as `-response` (as higher survival times correspond to lower risk).
#'    Works only if `overwrite` is `TRUE`.
#' * `overwrite` :: `logical(1)` \cr
#'    If `FALSE` (default) and the prediction already has a `response` prediction, then the compositor returns the input prediction unchanged.
#'    If `TRUE`, then the `response` (and the `crank`, if `add_crank` is `TRUE`) will be overwritten.
#'
#' @section Internals:
#' The restricted mean survival time is the default/preferred method and is calculated as follows:
#' \deqn{T_{i,rmst} \approx \sum_{t_j \in [0,\tau]} (t_j - t_{j-1}) S_i(t_j)}
#'
#' where \eqn{T} is the expected survival time, \eqn{\tau} is the time cutoff/horizon and \eqn{S_i(t_j)} are the predicted survival probabilities of observation \eqn{i} for all the \eqn{t_j} time points.
#'
#' The \eqn{T_{i,median}} survival time is just the first time point for which the survival probability is less than \eqn{0.5}.
#' If no such time point exists (e.g. when the survival distribution is not proper due to high censoring) we return the last time point.
#' This is **not a good estimate to use in general**, only a reasonable substitute in such cases.
#'
#' @references
#' `r format_bib("zhao_2016")`
#'
#' @seealso [pipeline_responsecompositor]
#' @family survival compositors
#' @examplesIf mlr3misc::require_namespaces(c("mlr3pipelines"), quietly = TRUE)
#' \dontrun{
#'   library(mlr3pipelines)
#'   task = tsk("rats")
#'
#'   # add survival time prediction type to the predictions of a Cox model
#'   # Median survival time as response
#'   pred = lrn("surv.coxph")$train(task)$predict(task)
#'   por = po("responsecompose", param_vals = list(method = "median", overwrite = TRUE))
#'   por$train(list(NULL)) # need to train first, even if nothing happens
#'   por$predict(list(pred))[[1L]]
#'   # mostly improper survival distributions, "median" sets the survival time
#'   # to the last time point
#'
#'   # RMST (default) as response, while also changing the `crank` to `-response`
#'   por = po("responsecompose", param_vals = list(overwrite = TRUE, add_crank = TRUE))
#'   por$train(list(NULL))
#'   por$predict(list(pred))[[1L]]
#' }
#' @export
PipeOpResponseCompositor = R6Class("PipeOpResponseCompositor",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "responsecompose", param_vals = list()) {
      param_set = ps(
        method = p_fct(default = "rmst", levels = c("rmst", "median"), tags = "predict"),
        tau = p_dbl(0, default = NULL, special_vals = list(NULL), tags = "predict"),
        add_crank = p_lgl(default = FALSE, tags = "predict"),
        overwrite = p_lgl(default = FALSE, tags = "predict")
      )

      param_set$set_values(method = "rmst", add_crank = FALSE, overwrite = FALSE)

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
      has_response = !is.null(pred$response)

      if (!overwrite & has_response) {
        # return prediction as is
        return(list(pred))
      } else {
        # compose response (survival time) from distr prediction
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

        # time points
        times = as.numeric(colnames(surv))

        method = self$param_set$values$method
        if (method == "rmst") {
          tau = self$param_set$values$tau
          within_range = !is.null(tau) && tau <= max(times) && tau >= min(times)
          if (within_range) {
            # subset survival matrix and times
            surv = surv[, times <= tau, drop = FALSE]
            times = times[times <= tau]
          }

          # calculate the restricted mean survival time
          weights = c(times[1], diff(times)) # time intervals as weights
          response = apply(surv, 1, function(surv_probs) {
            sum(weights * surv_probs)
          })
        } else { # median
          t_max = tail(times, 1) # last time point
          response = apply(surv, 1, function(surv_probs) {
            # first survival probability that is < 0.5
            median_time_index = match(TRUE, surv_probs < 0.5)
            if (!is.na(median_time_index)) times[median_time_index] else t_max
          })
        }

        add_crank = self$param_set$values$add_crank
        if (add_crank) {
          # higher risk, lower survival time
          crank = -response
        } else {
          # otherwise, leave crank unchanged
          crank = pred$crank
        }

        # update only `response` (and maybe `crank`)
        p = PredictionSurv$new(
          row_ids = pred$row_ids,
          truth = pred$truth,
          crank = crank,
          distr = pred$distr,
          lp = pred$lp,
          response = response
        )

        return(list(p))
      }
    }
  )
)

register_pipeop("responsecompose", PipeOpResponseCompositor)

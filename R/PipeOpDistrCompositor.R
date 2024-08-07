#' @title PipeOpDistrCompositor
#' @name mlr_pipeops_distrcompose
#' @template param_pipelines
#'
#' @description
#' Estimates (or 'composes') a survival distribution from a predicted baseline `distr` and a
#' `crank` or `lp` from two [PredictionSurv]s.
#'
#' Compositor Assumptions:
#' * The baseline `distr` is a discrete estimator, e.g. [surv.kaplan][LearnerSurvKaplan].
#' * The composed `distr` is of a linear form
#' * If `lp` is missing then `crank` is equivalent
#'
#' These assumptions are strong and may not be reasonable. Future updates will upgrade this
#' compositor to be more flexible.
#'
#' @section Dictionary:
#' This [PipeOp][mlr3pipelines::PipeOp] can be instantiated via the
#' [dictionary][mlr3misc::Dictionary] [mlr3pipelines::mlr_pipeops] or with the associated sugar
#' function [mlr3pipelines::po()]:
#' ```
#' PipeOpDistrCompositor$new()
#' mlr_pipeops$get("distrcompose")
#' po("distrcompose")
#' ```
#'
#' @section Input and Output Channels:
#' [PipeOpDistrCompositor] has two input channels, "base" and "pred". Both input channels take
#' `NULL` during training and [PredictionSurv] during prediction.
#'
#' [PipeOpDistrCompositor] has one output channel named "output", producing `NULL` during training
#' and a [PredictionSurv] during prediction.
#'
#' The output during prediction is the [PredictionSurv] from the "pred" input but with an extra
#' (or overwritten) column for `distr` predict type; which is composed from the `distr` of "base"
#' and `lp` or `crank` of "pred".
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' The parameters are:
#' * `form` :: `character(1)` \cr
#'    Determines the form that the predicted linear survival model should take. This is either,
#'    accelerated-failure time, `aft`, proportional hazards, `ph`, or proportional odds, `po`.
#'    Default `aft`.
#' * `overwrite` :: `logical(1)` \cr
#'    If `FALSE` (default) then if the "pred" input already has a `distr`, the compositor does
#'    nothing and returns the given [PredictionSurv]. If `TRUE` then the `distr` is overwritten
#'    with the `distr` composed from `lp`/`crank` - this is useful for changing the prediction
#'    `distr` from one model form to another.
#'
#' @section Internals:
#' The respective `form`s above have respective survival distributions:
#'    \deqn{aft: S(t) = S_0(\frac{t}{exp(lp)})}{aft: S(t) = S0(t/exp(lp))}
#'    \deqn{ph: S(t) = S_0(t)^{exp(lp)}}{ph: S(t) = S0(t)^exp(lp)}
#'    \deqn{po: S(t) = \frac{S_0(t)}{exp(-lp) + (1-exp(-lp)) S_0(t)}}{po: S(t) = S0(t) / [exp(-lp) + S0(t) (1-exp(-lp))]} # nolint
#' where \eqn{S_0}{S0} is the estimated baseline survival distribution, and \eqn{lp} is the
#' predicted linear predictor. If the input model does not predict a linear predictor then `crank`
#' is assumed to be the `lp` - **this may be a strong and unreasonable assumption.**
#'
#' @seealso [pipeline_distrcompositor]
#' @export
#' @family survival compositors
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3pipelines)
#'   task = tsk("rats")
#'
#'   base = lrn("surv.kaplan")$train(task)$predict(task)
#'   pred = lrn("surv.coxph")$train(task)$predict(task)
#'   pod = po("distrcompose", param_vals = list(form = "aft", overwrite = TRUE))
#'   pod$predict(list(base = base, pred = pred))[[1]]
#' }
#' }
PipeOpDistrCompositor = R6Class("PipeOpDistrCompositor",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "distrcompose", param_vals = list()) {
      param_set = ps(
        form = p_fct(default = "aft", levels = c("aft", "ph", "po"), tags = "predict"),
        overwrite = p_lgl(default = FALSE, tags = "predict")
      )
      param_set$set_values(form = "aft", overwrite = FALSE)

      super$initialize(
        id = id,
        param_set = param_set,
        param_vals = param_vals,
        input = data.table(name = c("base", "pred"), train = "NULL", predict = "PredictionSurv"),
        output = data.table(name = "output", train = "NULL", predict = "PredictionSurv"),
        packages = c("mlr3proba", "distr6")
      )
    }
  ),

  private = list(
    .train = function(inputs) {
      self$state = list()
      list(NULL)
    },

    .predict = function(inputs) {
      base = inputs$base
      inpred = inputs$pred

      overwrite = self$param_set$values$overwrite
      if (!length(overwrite)) overwrite = FALSE

      if ("distr" %in% inpred$predict_types & !overwrite) {
        return(list(inpred))
      } else {
        assert("distr" %in% base$predict_types)

        row_ids = inpred$row_ids
        truth = inpred$truth

        walk(inputs, function(x) assert_true(identical(truth, x$truth)))

        form = self$param_set$values$form
        if (length(form) == 0L) form = "aft"
        nr = length(inpred$data$row_ids)

        # assumes PH-style lp where high value = high risk
        if (anyMissing(inpred$lp)) {
          lp = inpred$crank
        } else {
          lp = inpred$lp
        }

        if (inherits(base$data$distr, "Distribution")) {
          base = distr6::as.MixtureDistribution(base$distr)
          times = unlist(base[1L]$properties$support$elements)
          nc = length(times)
          survmat = matrix(1 - base$cdf(times), nrow = nr, ncol = nc, byrow = TRUE)
        } else {
          base = colMeans(base$data$distr)
          times = as.numeric(names(base))
          nc = length(times)
          survmat = matrix(base, nrow = nr, ncol = nc, byrow = TRUE)
        }

        timesmat = matrix(times, nrow = nr, ncol = nc, byrow = TRUE)
        lpmat = matrix(lp, nrow = nr, ncol = nc)

        if (form == "ph") {
          cdf = 1 - (survmat^exp(lpmat))
        } else if (form == "aft") {
          mtc = findInterval(timesmat / exp(lpmat), times)
          mtc[mtc == 0] = NA
          cdf = 1 - matrix(survmat[1L, mtc], nr, nc, FALSE)
          cdf[is.na(cdf)] = 0
        } else if (form == "po") {
          cdf = 1 - (survmat * ((exp(-lpmat) + ((1 - exp(-lpmat)) * survmat))^-1))
          cdf[survmat == 1] = 0
        }

        distr = .surv_return(times, 1 - cdf)$distr

        if (anyMissing(inpred$lp)) {
          lp = NULL
        } else {
          lp = inpred$lp
        }

        return(list(PredictionSurv$new(
          row_ids = row_ids, truth = truth,
          crank = inpred$crank, distr = distr, lp = lp)))
      }
    }
  )
)

register_pipeop("distrcompose", PipeOpDistrCompositor)

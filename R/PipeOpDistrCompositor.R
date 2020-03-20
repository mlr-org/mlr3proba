#' @title PipeOpDistrCompositor
#' @aliases mlr_pipeops_distrcompose
#'
#' @description
#' Estimates (or 'composes') a survival distribution from a predicted baseline `distr` and a
#' `crank` or `lp` from two [PredictionSurv]s.
#'
#' Compositor Assumptions:
#' * The baseline `distr` is a discrete estimator, i.e. [LearnerSurvKaplan] or [LearnerSurvNelson]
#' * The composed `distr` is of a linear form
#' * If `lp` is missing then `crank` is equivalent
#'
#' These assumptions are strong and may not be reasonable. Future updates will upgrade this
#' compositor to be more flexible.
#'
#' @section Dictionary:
#' This [PipeOp][mlr3pipelines::PipeOp] can be instantiated via the [dictionary][mlr3misc::Dictionary]
#' [mlr3pipelines::mlr_pipeops] or with the associated sugar function [mlr3pipelines::po()]:
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
#' The output during prediction is the [PredictionSurv] from the "pred" input but with an extra (or overwritten)
#' column for `distr` predict type; which is composed from the `distr` of "base" and `lp` or `crank`
#' of "pred".
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
#'    If `FALSE` (default) then if the "pred" input already has a `distr`, the compositor does nothing
#'    and returns the given [PredictionSurv]. If `TRUE` then the `distr` is overwritten with the `distr`
#'    composed from `lp`/`crank` - this is useful for changing the prediction `distr` from one model
#'    form to another.
#'
#' @section Internals:
#' The respective `form`s above have respective survival distributions:
#'    \deqn{aft: S(t) = S_0(\frac{t}{exp(lp)})}{aft: S(t) = S0(t/exp(lp))}
#'    \deqn{ph: S(t) = S_0(t)^{exp(lp)}}{ph: S(t) = S0(t)^exp(lp)}
#'    \deqn{po: S(t) = \frac{S_0(t)}{exp(-lp) + (1-exp(-lp)) S_0(t)}}{po: S(t) = S0(t) / [exp(-lp) + S0(t) (1-exp(-lp))]}
#' where \eqn{S_0}{S0} is the estimated baseline survival distribution, and \eqn{lp} is the predicted
#' linear predictor. If the input model does not predict a linear predictor then `crank` is
#' assumed to be the `lp` - **this may be a strong and unreasonable assumption.**
#'
#' @section Fields:
#' Only fields inherited from [PipeOp][mlr3pipelines::PipeOp].
#'
#' @section Methods:
#' Only methods inherited from [PipeOp][mlr3pipelines::PipeOp].
#'
#' @seealso [mlr3pipelines::PipeOp] and [distrcompositor]
#' @export
#' @family survival compositors
#' @examples
#' library(mlr3)
#' library(mlr3pipelines)
#' set.seed(42)
#'
#' # Three methods to transform the cox ph predicted `distr` to an
#' #  accelerated failure time model
#' task = tgen("simsurv")$generate(30)
#'
#' # Method 1 - Train and predict separately then compose
#' base = lrn("surv.kaplan")$train(task)$predict(task)
#' pred = lrn("surv.coxph")$train(task)$predict(task)
#' pod = po("distrcompose", param_vals = list(form = "aft", overwrite = TRUE))
#' pod$predict(list(base = base, pred = pred))
#'
#' # Examples not run to save run-time.
#' \dontrun{
#' # Method 2 - Create a graph manually
#' gr = Graph$new()$
#'   add_pipeop(po("learner", lrn("surv.kaplan")))$
#'   add_pipeop(po("learner", lrn("surv.glmnet")))$
#'   add_pipeop(po("distrcompose"))$
#'   add_edge("surv.kaplan", "distrcompose", dst_channel = "base")$
#'   add_edge("surv.glmnet", "distrcompose", dst_channel = "pred")
#' gr$train(task)$gr$predict(task)
#'
#' # Method 3 - Syntactic sugar: Wrap the learner in a graph.
#' cvglm.distr = distrcompositor(learner = lrn("surv.cvglmnet"),
#'                             estimator = "kaplan",
#'                             form = "aft")
#' cvglm.distr$fit(task)$predict(task)
#' }
PipeOpDistrCompositor = R6Class("PipeOpDistrCompositor",
  inherit = PipeOp,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier of the resulting  object.
    #' @param param_vals (`list()`)\cr
    #'   List of hyperparameter settings, overwriting the hyperparameter settings that would
    #'   otherwise be set during construction.
    initialize = function(id = "distrcompose", param_vals = list(form = "aft", overwrite = FALSE)) {
      super$initialize(id = id,
                       param_set = ParamSet$new(params = list(
                         ParamFct$new("form", default = "aft", levels = c("aft","ph","po"), tags = c("predict")),
                         ParamLgl$new("overwrite", default = FALSE, tags = c("predict"))
                       )),
                       param_vals = param_vals,
                       input = data.table(name = c("base","pred"), train = "NULL", predict = "PredictionSurv"),
                       output = data.table(name = "output", train = "NULL", predict = "PredictionSurv"),
                       packages = "distr6")
      },

    #' @description train_internal
    #' Internal `train` function, will be moved to `private` in a near-future update, should be ignored.
    #' @param inputs
    #' Ignore.
    train_internal = function(inputs) {
      self$state = list()
      list(NULL)
    },

    #' @description predict_internal
    #' Internal `predict` function, will be moved to `private` in a near-future update, should be ignored.
    #' @param inputs
    #' Ignore.
    predict_internal = function(inputs) {
      base = inputs$base
      inpred = inputs$pred

      overwrite = self$param_set$values$overwrite
      if(length(overwrite) == 0) overwrite = FALSE

      if ("distr" %in% inpred$predict_types & !overwrite) {
        return(list(inpred))
      } else {
        assert("distr" %in% base$predict_types)

        row_ids = inpred$row_ids
        truth = inpred$truth
        map(inputs, function(x) assert_true(identical(row_ids, x$row_ids)))
        map(inputs, function(x) assert_true(identical(truth, x$truth)))

        form = self$param_set$values$form
        if(length(form) == 0) form = "aft"

        base = base$distr[1]
        times = unlist(base$support$elements)

        nr = nrow(inpred$data$tab)
        nc = length(times)

        if(is.null(inpred$lp) | length(inpred$lp) == 0)
          lp = inpred$crank
        else
          lp = inpred$lp

        timesmat = matrix(times, nrow = nr, ncol = nc, byrow = T)
        survmat = matrix(base$survival(times), nrow = nr, ncol = nc, byrow = T)
        lpmat = matrix(lp, nrow = nr, ncol = nc)

        if(form == "ph")
          cdf = 1 - (survmat ^ exp(lpmat))
        else if (form == "aft")
          cdf = t(apply(timesmat / exp(lpmat), 1, function(x) base$cdf(x)))
        else if (form == "po")
          cdf = 1 - (survmat * ({exp(-lpmat) + ((1 - exp(-lpmat)) * survmat)}^-1))

        x = rep(list(data = data.frame(x = times, cdf = 0)), nr)

        for(i in seq_along(times))
          x[[i]]$cdf = cdf[i,]

        distr = distr6::VectorDistribution$new(distribution = "WeightedDiscrete", params = x,
                                               decorators = c("CoreStatistics", "ExoticStatistics"))

        if(is.null(inpred$lp) | length(inpred$lp) == 0)
          lp = NULL
        else
          lp = inpred$lp

        return(list(PredictionSurv$new(row_ids = row_ids, truth = truth,
                                       crank = inpred$crank, distr = distr, lp = lp)))
      }
    }
  )

  # private = list(
  #   .train = function(inputs) {
  #     self$state = list()
  #     list(NULL)
  #   },
  #
  #   .predict = function(inputs) {
  #     base = inputs$base
  #     inpred = inputs$pred
  #
  #     overwrite = self$param_set$values$overwrite
  #     if(length(overwrite) == 0) overwrite = FALSE
  #
  #     if ("distr" %in% inpred$predict_types & !overwrite) {
  #       return(list(inpred))
  #     } else {
  #       assert("distr" %in% base$predict_types)
  #
  #       row_ids = inpred$row_ids
  #       truth = inpred$truth
  #       map(inputs, function(x) assert_true(identical(row_ids, x$row_ids)))
  #       map(inputs, function(x) assert_true(identical(truth, x$truth)))
  #
  #       form = self$param_set$values$form
  #       if(length(form) == 0) form = "aft"
  #
  #       base = base$distr[1]
  #       times = unlist(base$support()$elements)
  #
  #       nr = nrow(inpred$data$tab)
  #       nc = length(times)
  #
  #       if(is.null(inpred$lp) | length(inpred$lp) == 0)
  #         lp = inpred$crank
  #       else
  #         lp = inpred$lp
  #
  #       timesmat = matrix(times, nrow = nr, ncol = nc, byrow = T)
  #       survmat = matrix(base$survival(times), nrow = nr, ncol = nc, byrow = T)
  #       lpmat = matrix(lp, nrow = nr, ncol = nc)
  #
  #       if(form == "ph")
  #         cdf = 1 - (survmat ^ exp(lpmat))
  #       else if (form == "aft")
  #         cdf = t(apply(timesmat / exp(lpmat), 1, function(x) base$cdf(x)))
  #       else if (form == "po")
  #         cdf = 1 - (survmat * ({exp(-lpmat) + ((1 - exp(-lpmat)) * survmat)}^-1))
  #
  #       x = rep(list(data = data.frame(x = times, cdf = 0)), nr)
  #
  #       for(i in seq_along(times))
  #         x[[i]]$cdf = cdf[i,]
  #
  #       distr = distr6::VectorDistribution$new(distribution = "WeightedDiscrete", params = x,
  #                                              decorators = c("CoreStatistics", "ExoticStatistics"))
  #
  #       if(is.null(inpred$lp) | length(inpred$lp) == 0)
  #         lp = NULL
  #       else
  #         lp = inpred$lp
  #
  #       return(list(PredictionSurv$new(row_ids = row_ids, truth = truth,
  #                                      crank = inpred$crank, distr = distr, lp = lp)))
  #     }
  #   }
  # )
)

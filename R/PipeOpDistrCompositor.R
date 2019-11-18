#' @title PipeOpDistrCompositor
#'
#' @usage NULL
#' @name mlr_pipeops_distrcompose
#' @format [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Predict a survival distribution from a survival learner [`Prediction`][mlr3::Prediction], which
#' predicts `lp` or `crank`.
#'
#' Note:
#' * This compositor is only sensible if assuming a linear model form, which may not always be the case.
#' * Currently only discrete estimators, Kaplan-Meier and Nelson-Aalen, are implemented. Resulting in a
#' predicted `[distr6::WeightedDiscrete]` distribution for each individual, in the future we plan to
#' extend this to allow continuous estimators.
#'
#' @section Construction:
#' ```
#' PipeOpDistrCompositor$new(id = "distrcompose", param_vals = list())
#' ```
#' * `id` :: `character(1)` \cr
#'   Identifier of the resulting  object, default `"distrcompose"`.
#' * `param_vals` :: named `list` \cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
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
#'
#' @section Internals:
#' The respective `form`s above have respective survival distributions:
#'    \deqn{aft: S(t) = S0(t/exp(lp))}
#'    \deqn{ph: S(t) = S0(t)^exp(lp)}
#'    \deqn{po: S(t) = S0 * [exp(-lp) + (1-exp(-lp))*S0(t)]^-1}
#' where \eqn{S0} is the estimated baseline survival distribution, and `lp` is the predicted
#' linear predictor. If the input model does not predict a linear predictor then `crank` is
#' assumed to be the `lp` - **this may be a strong and unreasonable assumption.**
#'
#' @section Fields:
#' Only fields inherited from [PipeOp].
#'
#' @section Methods:
#' Only methods inherited from [PipeOp].
#'
#' @seealso [mlr3pipelines::PipeOp]
#' @export
#' @examples
#' library("mlr3")
#' library("mlr3pipelines")
#'
#' # Three methods to transform the cox ph predicted `distr` to an
#' #  accelerated failure time model
#' task = tsk("rats")
#'
#' # Method 1 - Train and predict separately then compose
#' base = lrn("surv.kaplan")$train(task)$predict(task)
#' pred = lrn("surv.coxph")$train(task)$predict(task)
#' pod = po("distrcompose", param_vals = list(form = "aft"))
#' pod$predict(list(base = base, pred = pred))
#'
#' # Method 2 - Create a graph manually
#' gr = Graph$new()$
#'   add_pipeop(po("learner", lrn("surv.kaplan")))$
#'   add_pipeop(po("learner", lrn("surv.coxph")))$
#'   add_pipeop(po("distrcompose"))$
#'   add_edge("surv.kaplan", "distrcompose", dst_channel = "base")$
#'   add_edge("surv.coxph", "distrcompose", dst_channel = "pred")
#' gr$train(task)
#' gr$predict(task)
#'
#' # Method 3 - Syntactic sugar: Wrap the learner in a graph
#' cox.distr = distrcompositor(learner = lrn("surv.coxph"),
#'                             estimator = "kaplan",
#'                             form = "aft")
#' cox.distr$plot()
#' resample(task, cox.distr, rsmp("cv", folds = 2))
PipeOpDistrCompositor = R6Class("PipeOpDistrCompositor",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "distrcompose", param_vals = list()) {
      super$initialize(id = id,
                       param_set = ParamSet$new(params = list(
                         ParamFct$new("form", default = "aft", levels = c("aft","ph","po"), tags = c("predict"))
                       )),
                       param_vals = param_vals,
                       input = data.table(name = c("base","pred"), train = "NULL", predict = "PredictionSurv"),
                       output = data.table(name = "output", train = "NULL", predict = "PredictionSurv"),
                       packages = "distr6")
      },

    train_internal = function(inputs) {
      self$state = list()
      list(NULL)
      },

    predict_internal = function(inputs) {
      base = inputs$base
      inpred = inputs$pred

      assert("distr" %in% base$predict_types)
      assert(any(c("crank", "lp") %in% inpred$predict_types))

      row_ids = inpred$row_ids
      map(inputs, function(x) assert_true(identical(row_ids, x$row_ids)))
      truth = inpred$truth

      # get form, set default if missing
      form = self$param_set$values$form
      if(length(form) == 0) form = "aft"

      times = base$distr[1]$support()$elements()
      base = base$distr[1]

      nr = nrow(inpred$data$tab)
      nc = length(times)

      if(is.null(inpred$lp))
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

      for(i in 1:nc)
        x[[i]]$cdf = cdf[i,]

      distr = distr6::VectorDistribution$new(distribution = "WeightedDiscrete", params = x,
                                             decorators = c("CoreStatistics", "ExoticStatistics"))


      truth = inputs[[1]]$truth

      list(PredictionSurv$new(row_ids = row_ids, truth = truth, crank = inpred$crank, distr = distr, lp = inpred$lp))
    }
  )
)

#' @title Survival Ranger Learner
#'
#' @usage NULL
#' @aliases mlr_learners_surv.ranger
#' @format [R6::R6Class()] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @section Construction:
#' ```
#' LearnerSurvRanger$new()
#' mlr_learners$get("surv.ranger")
#' lrn("surv.ranger")
#' ```
#'
#' @description
#' A [LearnerSurv] for a survival random forest implemented in [ranger::ranger()] in package \CRANpkg{ranger}.
#'
#' @references
#' Marvin N. Wright and Andreas Ziegler (2017).
#' ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R.
#' Journal of Statistical Software, 77(1), 1-17.
#' \doi{10.18637/jss.v077.i01}.
#'
#' Breiman, L. (2001).
#' Random Forests.
#' Machine Learning 45(1).
#' \doi{10.1023/A:1010933404324}.
#'
#' @template seealso_learner
#' @export
LearnerSurvRanger = R6Class("LearnerSurvRanger", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.ranger",
        param_set = ParamSet$new(
          params = list(
            ParamInt$new(id = "num.trees", default = 500L, lower = 1L, tags = c("train", "predict")),
            ParamInt$new(id = "mtry", lower = 1L, tags = "train"),
            ParamFct$new(id = "importance", levels = c("none", "impurity", "impurity_corrected", "permutation"), tags = "train"),
            ParamLgl$new(id = "write.forest", default = TRUE, tags = "train"),
            ParamInt$new(id = "min.node.size", default = 5L, lower = 1L, tags = "train"), # for probability == TRUE, def = 10
            ParamLgl$new(id = "replace", default = TRUE, tags = "train"),
            ParamDbl$new(id = "sample.fraction", lower = 0L, upper = 1L, tags = "train"), # for replace == FALSE, def = 0.632
            # ParamDbl$new(id = "class.weights", defaul = NULL, tags = "train"), #
            ParamFct$new(id = "splitrule", levels = c("variance", "extratrees", "maxstat"), default = "variance", tags = "train"),
            ParamInt$new(id = "num.random.splits", lower = 1L, default = 1L, tags = "train"), # requires = quote(splitrule == "extratrees")
            ParamDbl$new(id = "split.select.weights", lower = 0, upper = 1, tags = "train"),
            ParamUty$new(id = "always.split.variables", tags = "train"),
            ParamFct$new(id = "respect.unordered.factors", levels = c("ignore", "order", "partition"), default = "ignore", tags = "train"), # for splitrule == "extratrees", def = partition
            ParamLgl$new(id = "scale.permutation.importance", default = FALSE, tags = "train"), # requires = quote(importance == "permutation")
            ParamLgl$new(id = "keep.inbag", default = FALSE, tags = "train"),
            ParamLgl$new(id = "holdout", default = FALSE, tags = "train"), # FIXME: do we need this?
            ParamInt$new(id = "num.threads", lower = 1L, tags = c("train", "predict")),
            ParamLgl$new(id = "save.memory", default = FALSE, tags = "train"),
            ParamLgl$new(id = "verbose", default = TRUE, tags = c("train", "predict")),
            ParamLgl$new(id = "oob.error", default = TRUE, tags = "train")
          )
        ),
        predict_types = c("risk","distr"),
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = c("weights", "importance", "oob_error"),
        packages = c("ranger", "distr6")
      )
    },

    train_internal = function(task) {
      pv = self$param_set$get_values(tags = "train")
      targets = task$target_names

      invoke(ranger::ranger,
        formula = NULL,
        dependent.variable.name = targets[1L],
        status.variable.name = targets[2L],
        data = task$data(),
        case.weights = task$weights$weight,
        .args = pv
      )
    },

    predict_internal = function(task) {
      newdata = task$data(cols = task$feature_names)
      fit = predict(object = self$model, data = newdata)

      # Bottleneck. Avoidable or not? 4s time diff when returning risk only.
      distr = suppressAll(apply(fit$survival, 1, function(x)
        distr6::WeightedDiscrete$new(data.frame(x = fit$unique.death.times, cdf = 1 - x),
                             decorators = c(distr6::CoreStatistics, distr6::ExoticStatistics))))

      # Is it correct that the mean over time of the CHF is an estimate for risk?
      PredictionSurv$new(task = task, distr = distr, risk = rowMeans(fit$chf))
    },

    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      if (self$model$importance.mode == "none") {
        stopf("No importance stored")
      }

      sort(self$model$variable.importance, decreasing = TRUE)
    },

    oob_error = function() {
      self$model$prediction.error
    }
  )
)

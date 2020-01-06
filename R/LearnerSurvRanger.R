#' @template surv_learner
#' @templateVar title Ranger Survival Forest
#' @templateVar fullname LearnerSurvRanger
#' @templateVar caller [ranger::ranger()]
#' @templateVar distr using [ranger::predict.ranger()]
#'
#' @references
#' \cite{mlr3proba}{wright_2017}
#'
#' \cite{mlr3proba}{breiman_2001}
#'
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
            ParamFct$new(id = "splitrule", levels = c("logrank","extratrees","C","maxstat"), default = "logrank", tags = "train"),
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
        predict_types = c("distr","crank"),
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

      # define WeightedDiscrete distr6 object from predicted survival function
      x = rep(list(data = data.frame(x = fit$unique.death.times, cdf = 0)), task$nrow)
      for(i in 1:task$nrow)
        x[[i]]$cdf = 1 - fit$survival[i, ]

      distr = distr6::VectorDistribution$new(distribution = "WeightedDiscrete", params = x,
                                             decorators = c("CoreStatistics", "ExoticStatistics"))

      crank = as.numeric(sapply(x, function(y) sum(y[,1] * c(y[,2][1], diff(y[,2])))))

      PredictionSurv$new(task = task, distr = distr, crank = crank)
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

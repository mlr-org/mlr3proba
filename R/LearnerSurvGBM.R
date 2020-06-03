#' @template surv_learner
#' @templateVar title Generalized Boosting Regression Modeling
#' @templateVar fullname LearnerSurvGBM
#' @templateVar caller [gbm::gbm()]
#' @templateVar lp by [gbm::predict.gbm()]
#'
#' @description
#' Parameter `distribution` is set to `coxph` as this is the only distribution implemented
#' in [gbm::gbm()] for survival analysis; parameter `keep.data` is set to `FALSE` for efficiency.
#'
#' @references
#' \cite{mlr3proba}{freund_1997}
#'
#' \cite{mlr3proba}{ridgeway_1999}
#'
#' \cite{mlr3proba}{friedman_2000}
#'
#' \cite{mlr3proba}{friedman_2001}
#'
#' \cite{mlr3proba}{friedman_2001}
#'
#' \cite{mlr3proba}{friedman_2002}
#'
#' \cite{mlr3proba}{kriegler_2007}
#'
#' \cite{mlr3proba}{burges_2010}
#'
#' @export
LearnerSurvGBM = R6Class("LearnerSurvGBM",
  inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamFct$new(id = "distribution", levels = c("gaussian", "laplace", "tdist", "bernoulli", "huberized", "adaboost", "poisson", "coxph", "quantile", "pairwise"), default = "bernoulli", tags = "train"),
          ParamInt$new(id = "n.trees", default = 100L, lower = 1L, tags = c("train", "predict")),
          ParamInt$new(id = "cv.folds", default = 0L, lower = 0L, tags = "train"),
          ParamInt$new(id = "interaction.depth", default = 1L, lower = 1L, tags = "train"),
          ParamInt$new(id = "n.minobsinnode", default = 10L, lower = 1L, tags = "train"),
          ParamDbl$new(id = "shrinkage", default = 0.001, lower = 0, tags = "train"),
          ParamDbl$new(id = "bag.fraction", default = 0.5, lower = 0, upper = 1, tags = "train"),
          ParamDbl$new(id = "train.fraction", default = 1, lower = 0, upper = 1, tags = "train"),
          ParamLgl$new(id = "keep.data", default = TRUE, tags = "train"),
          ParamLgl$new(id = "verbose", default = FALSE, tags = "train"),
          ParamLgl$new(id = "single.tree", default = FALSE, tags = "predict")
        )
      )
      ps$values = insert_named(ps$values, list(distribution = "coxph", keep.data = FALSE))

      super$initialize(
        id = "surv.gbm",
        param_set = ps,
        predict_types = c("crank", "lp"),
        feature_types = c("integer", "numeric", "factor", "ordered"),
        properties = c("missings", "weights", "importance"),
        packages = c("gbm")
      )
    },

    #' @description
    #' The importance scores are extracted from the model slot `variable.importance`.
    #' @return Named `numeric()`.
    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      sum = summary(self$model, plotit = FALSE)
      relinf = sum$rel.inf
      names(relinf) = sum$var

      relinf
    }
  ),

  private = list(
    .train = function(task) {

      # hacky formula construction as gbm fails when "type" argument specified in Surv()

      tn = task$target_names
      lhs = sprintf("Surv(%s, %s)", tn[1L], tn[2L])
      f = formulate(lhs, task$feature_names, env = getNamespace("survival"))

      # collect arguments for predict
      pars = self$param_set$get_values(tags = "train")
      pars = c(pars, list(weights = task$weights$weight))

      invoke(
        gbm::gbm,
        formula = f,
        data = task$data(),
        .args = pars
      )
    },

    .predict = function(task) {

      pv = self$param_set$get_values(tags = "predict")
      newdata = task$data()

      # predict linear predictor
      lp = mlr3misc::invoke(predict, self$model, newdata = newdata, .args = pv)

      # define WeightedDiscrete distr6 object from predicted survival function
      # x = rep(list(data = data.frame(x = fit$unique.death.times, cdf = 0)), task$nrow)
      # for(i in 1:task$nrow)
      #   x[[i]]$cdf = 1 - fit$survival[i, ]

      # distr = distr6::VectorDistribution$new(
      #   distribution = "WeightedDiscrete",
      #   params = x,
      #   decorators = c("CoreStatistics", "ExoticStatistics"))

      # crank = as.numeric(sapply(x, function(y) sum(y[,1] * c(y[,2][1], diff(y[,2])))))

      PredictionSurv$new(task = task, crank = lp, lp = lp)

    }
  )
)

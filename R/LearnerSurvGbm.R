#' @title Survival Gbm Learner
#'
#' @usage NULL
#' @aliases mlr_learners_surv.gbm
#' @format [R6::R6Class()] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @section Construction:
#' ```
#' LearnerSurvGbm$new()
#' mlr_learners$get("surv.gbm")
#' lrn("surv.gbm")-
#' ```
#'
#' @description
#' A gradient boosting machine based [LearnerSurv] implemented in [gbm::gbm()] in package \CRANpkg{gbm}.
#' Only Cox PH type models are supported for survival analysis.
#'
#' @details
#' The \code{lp} return type is given natively by predicting the linear predictor in [gbm::predict.gbm()].\cr
#'
#'
#' @references
#' Brandon Greenwell, Bradley Boehmke, Jay Cunningham and GBM Developers (2019).
#'   gbm: Generalized Boosted Regression Models. R package version 2.1.5.
#'   \url{https://CRAN.R-project.org/package=gbm}
#'
#'
#' @template seealso_learner
#' @export
LearnerSurvGbm = R6Class("LearnerSurvGbm", inherit = LearnerSurv,
  public = list(
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
        packages = c("gbm", "distr6")
      )
    },

    train_internal = function(task) {

      # hacky formula construction as gbm fails when "type" argument specified in Surv()
      tn = task$target_names
      lhs = sprintf("Surv(%s, %s)", tn[1L], tn[2L])
      f = formulate(lhs, task$feature_names, env = getNamespace("survival"))

      # collect arguments for predict
      pars = self$param_set$get_values(tags = "train")
      pars = c(pars, list(weights = task$weights$weight))
      dat <- task$data()

      invoke(
        gbm::gbm,
        formula = f,
        data = task$data(),
        .args = pars
      )
    },

predict_internal = function(task) {

      pv = self$param_set$get_values(tags = "predict")
      newdata = task$data()

      # predict linear predictor
      lp = invoke(predict, self$model, newdata = newdata, .args = c(pv, type = "link"))

      # predict survival (type="response" returns lp)
      # surv = invoke(predict, self$model, newdata = newdata, .args = c(pv, type = "response"))

      # define WeightedDiscrete distr6 object from predicted survival function
      # x = rep(list(data = data.frame(x = fit$unique.death.times, cdf = 0)), task$nrow)
      # for(i in 1:task$nrow)
      #   x[[i]]$cdf = 1 - fit$survival[i, ]

      # distr = distr6::VectorDistribution$new(
      #   distribution = "WeightedDiscrete",
      #   params = x,
      #   decorators = c("CoreStatistics", "ExoticStatistics"))

      # crank = as.numeric(sapply(x, function(y) sum(y[,1] * c(y[,2][1], diff(y[,2])))))

      # note the ranking of lp and crank is identical
      PredictionSurv$new(task = task, crank = lp, lp = lp)

    },

    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      summary(self$model)$rel.inf
    }
  )
)

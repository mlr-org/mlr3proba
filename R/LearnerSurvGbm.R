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
#' Y. Freund and R.E. Schapire (1997)
#' A decision-theoretic generalization of on-line learning and an application to boosting.
#' Journal of Computer and System Sciences, 55(1):119-139.
#'
#' G. Ridgeway (1999). The state of boosting. Computing Science and Statistics 31:172-181.
#'
#' J.H. Friedman, T. Hastie, R. Tibshirani (2000).
#' Additive Logistic Regression: a Statistical View of Boosting. Annals of Statistics 28(2):337-374.
#'
#' J.H. Friedman (2001).
#' Greedy Function Approximation: A Gradient Boosting Machine. Annals of Statistics 29(5):1189-1232.
#'
#' J.H. Friedman (2002).
#' Stochastic Gradient Boosting. Computational Statistics and Data Analysis 38(4):367-378.
#'
#' B. Kriegler (2007).
#' Cost-Sensitive Stochastic Gradient Boosting Within a Quantitative Regression Framework.
#' Ph.D. Dissertation. University of California at Los Angeles, Los Angeles, CA, USA. Advisor(s) Richard A. Berk.
#' \url{https://dl.acm.org/citation.cfm?id=1354603}
#'
#' C. Burges (2010).
#' From RankNet to LambdaRank to LambdaMART: An Overview.
#' Microsoft Research Technical Report MSR-TR-2010-82.
#'
#' @export
LearnerSurvGBM = R6Class("LearnerSurvGBM", inherit = LearnerSurv,
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
        packages = c("gbm")
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

    },

    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      sum = summary(self$model, plotit = FALSE)
      relinf = sum$rel.inf
      names(relinf) = sum$var

      relinf
    }
  )
)

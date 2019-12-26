#' @template surv_learner
#' @templateVar title RandomForestSRC Survival Forest
#' @templateVar fullname LearnerSurvRandomForestSRC
#' @templateVar caller [randomForestSRC::rfsrc()]
#' @templateVar distr using [randomForestSRC::predict.rfsrc()]
#'
#' @description
#' [randomForestSRC::predict.rfsrc()] returns both cumulative hazard function (chf) and survival function (surv)
#' but uses different estimators to derive these. `chf` uses a bootstrapped Nelson-Aalen estimator,
#' (Ishwaran, 2008) whereas `surv` uses a bootstrapped Kaplan-Meier estimator [https://kogalur.github.io/randomForestSRC/theory.html](https://kogalur.github.io/randomForestSRC/theory.html).
#' The choice of which estimator to use is given by the extra `estimator` hyper-parameter,
#' default is `nelson`.
#'
#'
#' @references
#' Ishwaran H. and Kogalur U.B. (2019). Fast Unified Random Forests for Survival,
#' Regression, and Classification (RF-SRC), R package version 2.9.1.
#'
#' \cite{mlr3proba}{ishwaran_2008}
#'
#' \cite{mlr3proba}{breiman_2001}
#'
#' @export
LearnerSurvRandomForestSRC = R6Class("LearnerSurvRandomForestSRC", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamInt$new(id = "ntree", default = 1000, lower = 1L, tags = c("train", "predict")),
          ParamInt$new(id = "mtry", lower = 1L, tags = "train"),
          ParamInt$new(id = "nodesize", default = 15L, lower = 1L, tags = "train"),
          ParamInt$new(id = "nodedepth", lower = 1L, tags = "train"),
          ParamFct$new(id = "splitrule", levels = c("logrank", "bs.gradient", "logrankscore"), default = "logrank", tags = "train"),
          ParamInt$new(id = "nsplit", lower = 0, default = 10, tags = "train"),
          ParamFct$new(id = "importance", default = "FALSE", levels = c("FALSE", "TRUE", "none", "permute", "random", "anti"), tags = c("train","predict")),
          ParamInt$new(id = "block.size", default = 10L, lower = 1L, tags = c("train","predict")),
          ParamFct$new(id = "ensemble", default = "all", levels = c("all", "oob", "inbag"), tags = c("train","predict")),
          ParamFct$new(id = "bootstrap", default = "by.root", levels = c("by.root", "by.node", "none","by.user"), tags = "train"),
          ParamFct$new(id = "samptype", default = "swor", levels = c("swor", "swr"), tags = "train"),
          ParamUty$new(id = "samp", tags = "train"),
          ParamLgl$new(id = "membership", default = FALSE, tags = c("train","predict")),
          ParamUty$new(id = "sampsize", tags = "train"),
          ParamFct$new(id = "na.action", default = "na.omit", levels = c("na.omit", "na.impute"), tags = c("train","predict")),
          ParamInt$new(id = "na.impute", default = 1L, lower = 1L, tags = "train"),
          ParamInt$new(id = "ntime", lower = 1L, tags = "train"),
          ParamInt$new(id = "cause", lower = 1L, tags = "train"),
          ParamFct$new(id = "proximity", default = "FALSE", levels = c("FALSE", "TRUE", "inbag", "oob", "all"), tags = c("train","predict")),
          ParamFct$new(id = "distance", default = "FALSE", levels = c("FALSE", "TRUE", "inbag", "oob", "all"), tags = c("train","predict")),
          ParamFct$new(id = "forest.wt", default = "FALSE", levels = c("FALSE", "TRUE", "inbag", "oob", "all"), tags = c("train","predict")),
          ParamUty$new(id = "xvar.wt", tags = "train"),
          ParamUty$new(id = "split.wt", tags = "train"),
          ParamLgl$new(id = "forest", default = TRUE, tags = "train"),
          ParamFct$new(id = "var.used", default = "FALSE", levels = c("FALSE", "all.trees", "by.tree"), tags = c("train","predict")),
          ParamFct$new(id = "split.depth", default = "FALSE", levels = c("FALSE", "all.trees", "by.tree"), tags = c("train","predict")),
          ParamInt$new(id = "seed", upper = -1L, tags = c("train","predict")),
          ParamLgl$new(id = "do.trace", default = FALSE, tags = c("train","predict")),
          ParamLgl$new(id = "statistics", default = FALSE, tags = c("train","predict")),
          ParamUty$new(id = "get.tree", tags = "predict"),
          ParamFct$new(id = "outcome", default = "train", levels = c("train","test"), tags = "predict"),
          ParamInt$new(id = "ptn.count", default = 0L, lower = 0L, tags = "predict"),
          ParamFct$new(id = "estimator", default = "nelson", levels = c("nelson","kaplan"), tags = "predict")
        )
      )

      ps$values = insert_named(ps$values, list(estimator = "nelson"))

      super$initialize(
        id = "surv.randomForestSRC",
        param_set = ps,
        predict_types = c("crank","distr"),
        feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
        properties = c("weights", "missings", "importance"),
        packages = c("randomForestSRC", "distr6")
      )
    },

    train_internal = function(task) {
      pv = self$param_set$get_values(tags = "train")

      invoke(randomForestSRC::rfsrc, formula = task$formula(), data = task$data(),
        case.wt = task$weights$weight, .args = pv)
    },

    predict_internal = function(task) {
      newdata = task$data(cols = task$feature_names)
      pars = self$param_set$get_values(tags = "predict")
      # estimator parameter is used internally for composition (i.e. outside of rfsrc) and is
      # thus ignored for now
      pars$estimator = NULL

      p = invoke(predict, object = self$model, newdata = newdata, .args = pars)

      # rfsrc uses Nelson-Aalen in chf and Kaplan-Meier for survival, as these
      # don't give equivalent results one must be chosen and the relevant functions are transformed
      # as required.
      if(self$param_set$values$estimator == "nelson")
        cdf = 1 - exp(-p$chf)
      else
        cdf = 1 - p$survival

      # define WeightedDiscrete distr6 object from predicted survival function
      x = rep(list(data = data.frame(x = self$model$time.interest, cdf = 0)), task$nrow)
      for(i in 1:task$nrow)
        x[[i]]$cdf = cdf[i, ]

      distr = distr6::VectorDistribution$new(distribution = "WeightedDiscrete", params = x,
                                             decorators = c("CoreStatistics", "ExoticStatistics"))

      crank = as.numeric(sapply(x, function(y) sum(y[,1] * c(y[,2][1], diff(y[,2])))))

      PredictionSurv$new(task = task, crank = crank, distr = distr)

    },

    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      if (is.null(self$model$importance)) {
        stopf("Importance not stored. Set 'importance' parameter to one of {'TRUE', 'permute', 'random', 'anti'}.")
      }

      sort(self$model$importance, decreasing = TRUE)
    },

    selected_features = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      if (is.null(self$model$var.used)) {
        stopf("Variables used not stored. Set var.used to one of {'all.trees', 'by.tree'}.")
      }

      self$model$var.used
    }

    # Note that we could return prediction error but it would first have to be evaluated using Harrel's C
    # to be in line with other learners such as rpart.
    #
    # oob_error = function() {
    #   self$model$prediction.error
    # }
  )
)

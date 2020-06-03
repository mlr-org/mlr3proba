#' @template surv_learner
#' @templateVar title Gradient Boosting with Component-wise Linear Models
#' @templateVar fullname LearnerSurvGlmboost
#' @templateVar caller [mboost::glmboost()]
#' @templateVar distr by [mboost::survFit()] which assumes a PH fit with a Breslow estimator
#' @templateVar lp by [mboost::predict.mboost()]
#'
#' @template learner_boost
#'
#' @references
#' \cite{mlr3proba}{buehlmann_2003}
#'
#' \cite{mlr3proba}{buehlmann_2006}
#'
#' \cite{mlr3proba}{buehlmann_2007}
#'
#' \cite{mlr3proba}{hothorn_2010}
#'
#' \cite{mlr3proba}{hofner_2012}
#'
#' @export
#' @examples
#' library(mlr3)
#' task = tgen("simsurv")$generate(20)
#' learner = lrn("surv.glmboost")
#' resampling = rsmp("cv", folds = 3)
#' resample(task, learner, resampling)
LearnerSurvGlmboost = R6Class("LearnerSurvGlmboost",
  inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamFct$new(
            id = "family", default = "coxph",
            levels = c(
              "coxph", "weibull", "loglog", "lognormal", "gehan", "cindex",
              "custom"), tags = c("train", "family")),
          ParamUty$new(id = "custom.family", tags = c("train", "family")),
          ParamUty$new(id = "nuirange", default = c(0, 100), tags = c("train", "aft")),
          ParamLgl$new(id = "center", default = TRUE, tags = "train"),
          ParamInt$new(id = "mstop", default = 100L, lower = 0L, tags = "train"),
          ParamDbl$new(id = "nu", default = 0.1, lower = 0, upper = 1, tags = "train"),
          ParamFct$new(id = "risk", levels = c("inbag", "oobag", "none"), tags = "train"),
          ParamLgl$new(id = "stopintern", default = FALSE, tags = "train"),
          ParamLgl$new(id = "trace", default = FALSE, tags = "train"),
          ParamDbl$new(
            id = "sigma", default = 0.1, lower = 0, upper = 1,
            tags = c("train", "cindex")),
          ParamUty$new(id = "ipcw", default = 1, tags = c("train", "cindex"))
        )
      )

      ps$values = list(family = "coxph")
      ps$add_dep("sigma", "family", CondEqual$new("cindex"))
      ps$add_dep("ipcw", "family", CondEqual$new("cindex"))

      super$initialize(
        id = "surv.glmboost",
        param_set = ps,
        feature_types = c("integer", "numeric", "factor", "logical"),
        predict_types = c("distr", "crank", "lp", "response"),
        properties = c("weights"),
        packages = c("mboost", "distr6", "survival")
      )
    }

    #' Importance is supported but fails tests as internally data is coerced to model
    #' matrix and original names can't be recovered.
    #'
    # importance = function() {
    #   if (is.null(self$model)) {
    #     stopf("No model stored")
    #   }
    #
    #   sort(mboost::varimp(self$model)[-1], decreasing = TRUE)
    # },

    #' Importance is supported but fails tests as internally data is coerced to model
    #' matrix and original names can't be recovered.
    #'
    #' description
    #' Selected features are extracted with the function [mboost::variable.names.mboost()], with
    #' `used.only = TRUE`.
    #' return `character()`.
    # selected_features = function() {
    #   if (is.null(self$model)) {
    #     stopf("No model stored")
    #   }
    #
    #   sel = unique(names(self$model$model.frame())[self$model$xselect()])
    #   sel = sel[!(sel %in% "(Intercept)")]
    #
    #   return(sel)
    # }
  ),

  private = list(
    .train = function(task) {

      pars = self$param_set$get_values(tags = "train")

      if ("weights" %in% task$properties) {
        pars$weights = task$weights$weight
      }

      family = switch(pars$family,
        coxph = mboost::CoxPH(),
        weibull = mlr3misc::invoke(mboost::Weibull,
          .args = self$param_set$get_values(tags = "aft")),
        loglog = mlr3misc::invoke(mboost::Loglog,
          .args = self$param_set$get_values(tags = "aft")),
        lognormal = mlr3misc::invoke(mboost::Lognormal,
          .args = self$param_set$get_values(tags = "aft")),
        gehan = mboost::Gehan(),
        cindex = mlr3misc::invoke(mboost::Cindex,
          .args = self$param_set$get_values(tags = "cindex")),
        custom = pars$custom.family
      )

      # FIXME - until issue closes
      pars = pars[!(pars %in% self$param_set$get_values(tags = c("aft")))]
      pars = pars[!(pars %in% self$param_set$get_values(tags = c("cindex")))]
      pars = pars[!(pars %in% self$param_set$get_values(tags = c("family")))]

      mlr3misc::invoke(mboost::glmboost, task$formula(task$feature_names),
        data = task$data(), family = family, .args = pars)
    },

    .predict = function(task) {

      newdata = task$data(cols = task$feature_names)

      # predict linear predictor
      lp = as.numeric(mlr3misc::invoke(predict, self$model, newdata = newdata, type = "link"))

      # predict survival
      surv = mlr3misc::invoke(mboost::survFit, self$model, newdata = newdata)
      surv$cdf = 1 - surv$surv

      # define WeightedDiscrete distr6 object from predicted survival function
      x = rep(list(data = data.frame(x = surv$time, cdf = 0)), task$nrow)
      for (i in 1:task$nrow) {
        x[[i]]$cdf = surv$cdf[, i]
      }

      distr = distr6::VectorDistribution$new(
        distribution = "WeightedDiscrete", params = x,
        decorators = c("CoreStatistics", "ExoticStatistics"))

      response = NULL
      if (!is.null(self$param_set$values$family)) {
        if (self$param_set$values$family %in% c("weibull", "loglog", "lognormal", "gehan")) {
          response = exp(lp)
        }
      }

      PredictionSurv$new(task = task, crank = lp, distr = distr, lp = lp, response = response)
    }
  )
)

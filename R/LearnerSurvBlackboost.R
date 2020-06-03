#' @template surv_learner
#' @templateVar title Gradient Boosting with Regression Trees
#' @templateVar fullname LearnerSurvBlackboost
#' @templateVar caller [mboost::blackboost()]
#' @templateVar distr by [mboost::survFit()] which assumes a PH fit with a Breslow estimator
#' @templateVar lp by [mboost::predict.mboost()]
#'
#' @template learner_boost
#'
#' @references
#' \cite{mlr3proba}{buehlmann_2007}
#'
#' \cite{mlr3proba}{hothorn_2006}
#'
#' \cite{mlr3proba}{freund_1996}
#'
#' \cite{mlr3proba}{friedman_2001}
#'
#' \cite{mlr3proba}{ridgeway_1999}
#'
#' @export
#' @examples
#' library(mlr3)
#' task = tgen("simsurv")$generate(20)
#' learner = lrn("surv.blackboost")
#' resampling = rsmp("cv", folds = 2)
#' resample(task, learner, resampling)
LearnerSurvBlackboost = R6Class("LearnerSurvBlackboost",
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
          ParamUty$new(id = "offset", tags = "train"),
          ParamLgl$new(id = "center", default = TRUE, tags = "train"),
          ParamInt$new(id = "mstop", default = 100L, lower = 0L, tags = "train"),
          ParamDbl$new(id = "nu", default = 0.1, lower = 0, upper = 1, tags = "train"),
          ParamFct$new(id = "risk", levels = c("inbag", "oobag", "none"), tags = "train"),
          ParamLgl$new(id = "stopintern", default = FALSE, tags = "train"),
          ParamLgl$new(id = "trace", default = FALSE, tags = "train"),
          ParamUty$new(id = "oobweights", tags = "train"),
          ParamFct$new(id = "teststat", default = "quadratic", levels = c("quadratic", "maximum"), tags = "train"),
          ParamFct$new(id = "splitstat", default = "quadratic", levels = c("quadratic", "maximum"), tags = "train"),
          ParamLgl$new(id = "splittest", default = FALSE, tags = "train"),
          ParamFct$new(
            id = "testtype", default = "Bonferroni",
            levels = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic"), tags = "train"),
          ParamInt$new(id = "maxpts", default = 25000L, lower = 1L, tags = "train"),
          ParamDbl$new(id = "abseps", default = 0.001, tags = "train"),
          ParamDbl$new(id = "releps", default = 0, tags = "train"),
          ParamUty$new(id = "nmax", tags = "train"),
          ParamDbl$new(id = "alpha", default = 0.05, lower = 0, upper = 1, tags = "train"),
          ParamDbl$new(id = "mincriterion", default = 0.95, lower = 0, upper = 1, tags = "train"),
          ParamDbl$new(id = "logmincriterion", default = log(0.95), upper = 0, tags = "train"),
          ParamInt$new(id = "minsplit", default = 20L, lower = 0L, tags = "train"),
          ParamInt$new(id = "minbucket", default = 7L, lower = 0L, tags = "train"),
          ParamDbl$new(id = "minprob", default = 0.01, lower = 0, upper = 1, tags = "train"),
          ParamLgl$new(id = "stump", default = FALSE, tags = "train"),
          ParamLgl$new(id = "lookahead", default = FALSE, tags = "train"),
          ParamLgl$new(id = "MIA", default = FALSE, tags = "train"),
          ParamInt$new(id = "nresample", default = 9999L, lower = 1L, tags = "train"),
          ParamDbl$new(id = "tol", default = sqrt(.Machine$double.eps), lower = 0, tags = "train"),
          ParamInt$new(id = "maxsurrogate", default = 0L, lower = 0L, tags = "train"),
          ParamInt$new(id = "mtry", lower = 0L, tags = "train"),
          ParamInt$new(id = "maxdepth", lower = 0L, tags = "train"),
          ParamLgl$new(id = "multiway", default = FALSE, tags = "train"),
          ParamInt$new(id = "splittry", default = 2L, lower = 1L, tags = "train"),
          ParamLgl$new(id = "intersplit", default = FALSE, tags = "train"),
          ParamLgl$new(id = "majority", default = FALSE, tags = "train"),
          ParamLgl$new(id = "caseweights", default = TRUE, tags = "train"),
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
        id = "surv.blackboost",
        param_set = ps,
        feature_types = c("integer", "numeric", "factor"),
        predict_types = c("distr", "crank", "lp", "response"),
        properties = c("weights"),
        packages = c("mboost", "distr6", "survival", "partykit", "mvtnorm")
      )
    }
  ),

  private = list(
    .train = function(task) {

      pars = self$param_set$get_values(tags = "train")

      if ("weights" %in% task$properties) {
        pars$weights = task$weights$weight
      }

      # mboost control
      # Save control settings and return on exit
      saved_ctrl = mboost::boost_control()
      is_ctrl_pars = (names(pars) %in% names(saved_ctrl))
      # ensure only relevant pars passed to fitted model
      if (any(is_ctrl_pars)) {
        pars$control = do.call(mboost::boost_control, pars[is_ctrl_pars])
        pars = pars[!is_ctrl_pars]
      }

      # GenzBretz control
      # Save control settings and return on exit
      saved_ctrl = mvtnorm::GenzBretz()
      is_ctrl_pars = (names(pars) %in% names(saved_ctrl))
      # ensure only relevant pars passed to fitted model
      if (any(is_ctrl_pars)) {
        pars$pargs = do.call(mvtnorm::GenzBretz, pars[is_ctrl_pars])
        pars = pars[!is_ctrl_pars]
      }

      # ctree control
      # Save control settings and return on exit
      saved_ctrl = partykit::ctree_control()
      is_ctrl_pars = (names(pars) %in% names(saved_ctrl))
      # ensure only relevant pars passed to fitted model
      if (any(is_ctrl_pars)) {
        pars$tree_controls = do.call(partykit::ctree_control, pars[is_ctrl_pars])
        pars = pars[!is_ctrl_pars]
      }

      # if ("weights" %in% task$properties)
      #   pars$weights = task$weights$weight

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

      mlr3misc::invoke(mboost::blackboost,
        formula = task$formula(task$feature_names),
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

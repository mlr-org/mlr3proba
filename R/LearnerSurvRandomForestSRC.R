#' @title Survival RandomForestSRC Learner
#'
#' @usage NULL
#' @aliases mlr_learners_surv.randomForestSRC
#' @format [R6::R6Class()] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @section Construction:
#' ```
#' LearnerSurvRandomForestSRC$new()
#' mlr_learners$get("surv.randomForestSRC")
#' lrn("surv.randomForestSRC")
#' ```
#'
#' @description
#' A [LearnerSurv] for a survival random forest implemented in [randomForestSRC::rfsrc()] in package \CRANpkg{randomForestSRC}.
#'
#' @details
#' \code{\link[randomForestSRC]{rfsrc}} has three prediction outcomes, from the fitted model these are
#' respectively:
#' 1. predicted - This is ensemble mortality defined in Ishwaran et al. (2008), the sum
#' of an individuals cumulative hazard function over all time-points
#' 2. chf - Cumulative hazard function, estimated via a bootstrapped Nelson-Aalen estimator (Ishwaran, 2008)
#' 3. surv - Survival function, estimated via a bootrstrapped Kaplan-Meier estimate (https://kogalur.github.io/randomForestSRC/theory.html)
#'
#' Only the second two of these are returned in the \code{distr} predict.type, as Nelson-Aalen and Kaplan-Meier
#' will give different results, the estimator can be chosen in the parameter set under "estimator".
#'
#' @references
#' Ishwaran H. and Kogalur U.B. (2019). Fast Unified Random Forests for Survival,
#' Regression, and Classification (RF-SRC), R package version 2.9.1.
#'
#' Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S. (2008). Random survival
#' forests. Ann. Appl. Statist. 2(3), 841--860.
#'
#' Breiman, L. (2001).
#' Random Forests.
#' Machine Learning 45(1).
#' \doi{10.1023/A:1010933404324}.
#'
#' @template seealso_learner
#' @export
LearnerSurvRandomForestSRC = R6Class("LearnerSurvRandomForestSRC", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.randomForestSRC",
        param_set = ParamSet$new(
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
        ),
        predict_types = c("crank","distr"),
        feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
        properties = c("weights", "missings", "importance"),
        packages = c("randomForestSRC", "distr6")
      )
    },

    train_internal = function(task) {
      pv = self$param_set$get_values(tags = "train")

      fit = invoke(randomForestSRC::rfsrc,
        formula = task$formula(),
        data = task$data(),
        case.wt = task$weights$weight,
        .args = pv
      )

      set_class(list(fit = fit,
                     times = sort(unique(fit$yvar[,1][fit$yvar[,2]==1]))),
                "surv.randomForestSRC")
    },

    predict_internal = function(task) {
      newdata = task$data(cols = task$feature_names)
      pars = self$param_set$get_values(tags = "predict")
      # estimator parameter is used internally for composition (i.e. outside of rfsrc) and is
      # thus ignored for now
      pars$estimator = NULL

      p = invoke(predict, object = self$model$fit, newdata = newdata, .args = pars)

      # Default estimator is set to Kaplan-Meier
      estimator = self$param_set$values$estimator
      if(length(estimator) == 0) estimator = "kaplan"
      # rfsrc uses Nelson-Aalen in chf and Kaplan-Meier for survival, as these
      # don't give equivalent results one must be chosen and the relevant functions are transformed
      # as required.
      if(estimator == "nelson")
        cdf = 1 - exp(-p$chf)
      else
        cdf = 1 - p$survival

      # define WeightedDiscrete distr6 object
      distr = suppressAll(apply(cdf, 1, function(x)
        distr6::WeightedDiscrete$new(data.frame(x = self$model$times, cdf = x),
                             decorators = c(distr6::CoreStatistics, distr6::ExoticStatistics))))

      # crank defined as mean of survival distribution.
      crank = as.numeric(unlist(lapply(distr, mean)))

      PredictionSurv$new(task = task, distr = distr, crank = crank)
    },

    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      if (is.null(self$model$fit$importance)) {
        stopf("Importance not stored. Set 'importance' parameter to one of {'TRUE', 'permute', 'random', 'anti'}.")
      }

      sort(self$model$fit$importance, decreasing = TRUE)
    },

    selected_features = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      if (is.null(self$model$fit$var.used)) {
        stopf("Variables used not stored. Set var.used to one of {'all.trees', 'by.tree'}.")
      }

      self$model$fit$var.used
    }

    # Note that we could return prediction error but it would first have to be evaluated using Harrel's C
    # to be in line with other learners such as rpart.
    #
    # oob_error = function() {
    #   self$model$prediction.error
    # }
  )
)

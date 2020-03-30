#' @template surv_learner
#' @templateVar title Cox Model with Likelihood Based Boosting
#' @templateVar fullname LearnerSurvCVCoxboost
#' @templateVar caller [CoxBoost::cv.CoxBoost]
#' @templateVar distr by [CoxBoost::predict.CoxBoost]
#' @templateVar lp by [CoxBoost::predict.CoxBoost]
#'
#' @description
#' Use [LearnerSurvCoxboost] and [LearnerSurvCVCoxboost] for Cox boosting without and with internal
#' cross-validation of boosting step number, respectively. Tuning using the internal optimizer in
#' [LearnerSurvCVCoxboost] may be more efficient when tuning `stepno` only. However, for tuning
#' multiple hyperparameters, \CRANpkg{mlr3tuning} and [LearnerSurvCoxboost] will likely give better
#' results.
#'
#' If `penalty == "optimCoxBoostPenalty"` then [CoxBoost::optimCoxBoostPenalty] is used to determine
#' the penalty value to be used in [CoxBoost::cv.CoxBoost].
#'
#'
#' @references
#' \cite{mlr3proba}{binder_2009}
#'
#' @export
LearnerSurvCVCoxboost = R6Class("LearnerSurvCVCoxboost", inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "surv.cvcoxboost",
        param_set = ParamSet$new(
          params = list(
            ParamInt$new(id = "maxstepno", default = 100, lower = 0, tags = "train"),
            ParamInt$new(id = "K", default = 10, lower = 2, tags = "train"),
            ParamFct$new(id = "type", default = "verweij", levels = c("verweij", "naive"), tags = "train"),
            ParamUty$new(id = "folds", default = NULL, tags = "train"),
            ParamInt$new(id = "minstepno", default = 50, lower = 0, tags = "train"),
            ParamDbl$new(id = "start.penalty", tags = "train"),
            ParamInt$new(id = "iter.max", default = 10, lower = 1, tags = "train"),
            ParamDbl$new(id = "upper.margin", default = 0.05, lower = 0, upper = 1, tags = "train"),
            ParamUty$new(id = "unpen.index", tags = "train"),
            ParamLgl$new(id = "standardize", default = TRUE, tags = "train"),
            ParamDbl$new(id = "penalty", special_vals = list("optimCoxBoostPenalty"), tags = "train"),
            ParamFct$new(id = "criterion", default = "pscore", levels = c("pscore", "score", "hpscore", "hscore"), tags = "train"),
            ParamDbl$new(id = "stepsize.factor", default = 1, tags = "train"),
            ParamFct$new(id = "sf.scheme", default = "sigmoid", levels = c("sigmoid", "linear"), tags = "train"),
            ParamUty$new(id = "pendistmat", tags = "train"),
            ParamUty$new(id = "connected.index", tags = "train"),
            ParamLgl$new(id = "x.is.01", default = FALSE, tags = "train"),
            ParamLgl$new(id = "return.score", default = TRUE, tags = "train"),
            ParamUty$new(id = "at.step", tags = "predict")
          )),
        feature_types = c("integer", "numeric", "factor", "logical"),
        predict_types = c("distr","crank","lp"),
        packages = c("CoxBoost","Matrix","distr6","survival"),
        properties = "weights"
      )
    }
  ),

  private = list(
    .train = function(task) {

      pars = self$param_set$get_values(tags = "train")

      if ("weights" %in% task$properties) {
        pars$weights = as.numeric(task$weights$weight)
      }

      pen.optim = if(is.null(pars$penalty)) FALSE else pars$penalty == "optimCoxBoostPenalty"

      with_package("CoxBoost", {
        if(pen.optim){
          opt.pars = c("minstepno", "start.penalty", "iter.max", "upper.margin","penalty")
          pars = pars[names(pars) %nin% opt.pars]

          optim = invoke(
            CoxBoost::optimCoxBoostPenalty,
            time = task$truth()[, 1],
            status = task$truth()[, 2],
            x = model.matrix( ~ ., as.data.frame(task$data(cols = task$feature_names)))[,-1,drop = FALSE],
            .args = pars
          )

          pars = pars[names(pars) %nin% c("maxstepno", "K", "type")]

          return(invoke(
            CoxBoost::CoxBoost,
            time = task$truth()[, 1],
            status = task$truth()[, 2],
            x = model.matrix( ~ ., as.data.frame(task$data(cols = task$feature_names)))[,-1,drop = FALSE],
            stepno = optim$cv.res$optimal.step,
            penalty = optim$penalty,
            .args = pars
          ))
        } else {
          optimal.step = invoke(
            CoxBoost::cv.CoxBoost,
            time = task$truth()[, 1],
            status = task$truth()[, 2],
            x = model.matrix( ~ ., as.data.frame(task$data(cols = task$feature_names)))[,-1,drop = FALSE],
            .args = pars
          )$optimal.step

          pars = pars[names(pars) %nin% c("maxstepno", "K", "type")]

          return(invoke(
            CoxBoost::CoxBoost,
            time = task$truth()[, 1],
            status = task$truth()[, 2],
            x = model.matrix( ~ ., as.data.frame(task$data(cols = task$feature_names)))[,-1,drop = FALSE],
            stepno = optimal.step,
            .args = pars
          ))
        }
      })
    },

    .predict = function(task) {

      lp = as.numeric(invoke(predict,
                  self$model,
                  newdata = model.matrix(~., as.data.frame(task$data(cols = task$feature_names)))[,-1,drop=FALSE],
                  .args = self$param_set$get_values(tags = "predict"),
                  type = "lp"))

      cdf = invoke(predict,
                   self$model,
                   newdata = model.matrix(~., as.data.frame(task$data(cols = task$feature_names)))[,-1,drop=FALSE],
                   .args = self$param_set$get_values(tags = "predict"),
                   type = "CIF",
                   times = sort(unique(self$model$time)))

      # define WeightedDiscrete distr6 object from predicted survival function
      x = rep(list(data = data.frame(x = sort(unique(self$model$time)), cdf = 0)), task$nrow)
      for(i in 1:task$nrow)
        x[[i]]$cdf = cdf[i, ]

      distr = distr6::VectorDistribution$new(distribution = "WeightedDiscrete", params = x,
                                             decorators = c("CoreStatistics", "ExoticStatistics"))

      PredictionSurv$new(task = task, crank = lp, distr = distr, lp = lp)
    }
  )
)

#' @template surv_learner
#' @templateVar title Cox Model with Likelihood Based Boosting
#' @templateVar fullname LearnerSurvCoxboost
#' @templateVar caller [CoxBoost::CoxBoost]
#' @templateVar distr by [CoxBoost::CoxBoost]
#' @templateVar lp by [CoxBoost::CoxBoost]
#' @description
#'
#'
#' @references
#' \cite{mlr3proba}{binder_2009}
#'
#' @export
LearnerSurvCoxboost = R6Class("LearnerSurvCoxboost", inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "surv.coxboost",
        param_set = ParamSet$new(
          params = list(
            ParamUty$new(id = "unpen.index", tags = "train"),
            ParamLgl$new(id = "standardize", default = TRUE, tags = "train"),
            ParamInt$new(id = "stepno", default = 100, lower = 1, tags = "train"),
            ParamDbl$new(id = "penalty", tags = "train"),
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

      with_package("CoxBoost", {
        invoke(
          CoxBoost::CoxBoost,
          time = task$truth()[, 1],
          status = task$truth()[, 2],
          x = model.matrix( ~ ., as.data.frame(task$data(cols = task$feature_names)))[,-1,drop = FALSE],
          .args = pars
        )
      })
    },

    .predict = function(task) {

      lp = invoke(predict,
                  self$model,
                  newdata = model.matrix(~., as.data.frame(task$data(cols = task$feature_names)))[,-1,drop=FALSE],
                  .args = self$param_set$get_values(tags = "predict"),
                  type = "lp")

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

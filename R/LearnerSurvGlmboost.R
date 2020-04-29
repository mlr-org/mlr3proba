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
LearnerSurvGlmboost = R6Class("LearnerSurvGlmboost", inherit = LearnerSurv,
    public = list(
      #' @description
      #' Creates a new instance of this [R6][R6::R6Class] class.
      initialize = function() {
        ps = ParamSet$new(
          params = list(
            ParamFct$new(id = "family", default = "coxph",
                         levels = c("coxph", "weibull", "loglog", "lognormal", "gehan", "cindex",
                                    "custom"), tags = "train"),
            ParamUty$new(id = "nuirange", default = c(0, 100), tags = "train"),
            ParamUty$new(id = "custom.family", tags = "train"),
            ParamLgl$new(id = "center", default = TRUE, tags = "train"),
            ParamInt$new(id = "mstop", default = 100L, lower = 0L, tags = "train"),
            ParamDbl$new(id = "nu", default = 0.1, lower = 0, upper = 1, tags = "train"),
            ParamFct$new(id = "risk", levels = c("inbag", "oobag", "none"), tags = "train"),
            ParamLgl$new(id = "stopintern", default = FALSE, tags = "train"),
            ParamLgl$new(id = "trace", default = FALSE, tags = "train"),
            ParamDbl$new(id = "sigma", default = 0.1, lower = 0, upper = 1, tags = "train"),
            ParamUty$new(id = "ipcw", default = 1, tags = "train")
          )
        )

        ps$values = list(family = "coxph")
        ps$add_dep("sigma", "family", CondEqual$new("cindex"))
        ps$add_dep("ipcw", "family", CondEqual$new("ipcw"))

        super$initialize(
          id = "surv.glmboost",
          param_set = ps,
          feature_types = c("integer", "numeric", "factor", "logical"),
          predict_types = c("distr","crank","lp","response"),
          packages = c("mboost","distr6","survival")
          )
        }
  ),

 private = list(
   .train = function(task) {

     pars = self$param_set$get_values(tags = "train")

     # convert data to model matrix
     x = model.matrix(~., as.data.frame(task$data(cols = task$feature_names)))

     family = switch(pars$family,
                     coxph = mboost::CoxPH(),
                     weibull = mboost::Weibull(nuirange = pars$nuirange),
                     loglog = mboost::Loglog(nuirange = pars$nuirange),
                     lognormal = mboost::Lognormal(nuirange = pars$nuirange),
                     gehan = mboost::Gehan(),
                     cindex = mboost::Cindex(sigma = sigma, ipcw = ipcw),
                     custom = pars$custom.family
     )

     pars = pars[!(names(pars) %in% c("family", "nuirange", "custom.family", "ipcw", "sigma"))]


     invoke(mboost::glmboost, x = x, y = task$truth(), family = family, .args = pars)
   },

   .predict = function(task) {

     # convert data to model matrix
     newdata = model.matrix(~., as.data.frame(task$data(cols = task$feature_names)))

     # predict linear predictor
     lp = as.numeric(invoke(predict, self$model, newdata = newdata, type = "link"))

     # predict survival
     surv = invoke(mboost::survFit, self$model, newdata = newdata)
     surv$cdf = 1 - surv$surv

     # define WeightedDiscrete distr6 object from predicted survival function
     x = rep(list(data = data.frame(x = surv$time, cdf = 0)), task$nrow)
     for(i in 1:task$nrow)
       x[[i]]$cdf = surv$cdf[, i]

     distr = distr6::VectorDistribution$new(distribution = "WeightedDiscrete", params = x,
                                            decorators = c("CoreStatistics", "ExoticStatistics"))

     response = NULL
     if (!is.null(self$param_set$values$family)) {
       if(self$param_set$values$family %in% c("weibull", "loglog", "lognormal")) {
         response = exp(lp)
       }
     }

     PredictionSurv$new(task = task, crank = lp, distr = distr, lp = lp, response = response)
   }
 )
)

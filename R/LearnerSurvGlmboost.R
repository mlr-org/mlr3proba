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
#' Peter Buehlmann and Bin Yu (2003), Boosting with the L2 loss: regression and classification.
#' Journal of the American Statistical Association, 98, 324–339.
#'
#' Peter Buehlmann (2006), Boosting for high-dimensional linear models. The Annals of
#' Statistics, 34(2), 559–583.
#'
#' Peter Buehlmann and Torsten Hothorn (2007), Boosting algorithms: regularization,
#' prediction and model fitting. Statistical Science, 22(4), 477–505.
#'
#' Torsten Hothorn, Peter Buehlmann, Thomas Kneib, Mattthias Schmid and Benjamin Hofner
#' (2010), Model-based Boosting 2.0. Journal of Machine Learning Research, 11, 2109–2113.
#'
#' Benjamin Hofner, Andreas Mayr, Nikolay Robinzonov and Matthias Schmid (2014). Model-based Boosting in R: A Hands-on Tutorial Using the R Package mboost. Computational Statistics, 29, 3–35.
#' \doi{10.1007/s00180-012-0382-5}.
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
      initialize = function() {
        ps = ParamSet$new(
          params = list(
            ParamFct$new(id = "family", default = "coxph",
                         levels = c("coxph", "weibull", "loglog", "lognormal", "gehan",
                                    "custom"), tags = "train"),
            ParamUty$new(id = "nuirange", default = c(0, 100), tags = "train"),
            ParamUty$new(id = "custom.family", tags = "train"),
            ParamLgl$new(id = "center", default = TRUE, tags = "train"),
            ParamInt$new(id = "mstop", default = 100L, lower = 0L, tags = "train"),
            ParamDbl$new(id = "nu", default = 0.1, lower = 0, upper = 1, tags = "train"),
            ParamFct$new(id = "risk", levels = c("inbag", "oobag", "none"), tags = "train"),
            ParamLgl$new(id = "stopintern", default = FALSE, tags = "train"),
            ParamLgl$new(id = "trace", default = FALSE, tags = "train")
          )
        )

        ps$values = list(family = "coxph")

        super$initialize(
          id = "surv.glmboost",
          param_set = ps,
          feature_types = c("integer", "numeric", "factor", "logical"),
          predict_types = c("distr","crank","lp"),
          packages = c("mboost","distr6","survival")
          )
        },

      train_internal = function(task) {

        pars = self$param_set$get_values(tags = "train")

        # convert data to model matrix
        x = model.matrix(~., as.data.frame(task$data(cols = task$feature_names)))

        family = switch(pars$family,
                        coxph = mboost::CoxPH(),
                        weibull = mboost::Weibull(nuirange = pars$nuirange),
                        loglog = mboost::Loglog(nuirange = pars$nuirange),
                        lognormal = mboost::Lognormal(nuirange = pars$nuirange),
                        gehan = mboost::Gehan(),
                        custom = pars$custom.family
        )

        pars = pars[!(names(pars) %in% c("family", "nuirange", "custom.family"))]

       invoke(mboost::glmboost, x = x, y = task$truth(), family = family, .args = pars)
 },

    predict_internal = function(task) {

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

      PredictionSurv$new(task = task, crank = lp, distr = distr, lp = lp)
      }
  )
)

#' @include predict_flexsurvreg.R
#' @template surv_learner
#' @templateVar title Flexible Parametric Spline
#' @templateVar fullname LearnerSurvFlexible
#' @templateVar caller [survival::survreg()]
#' @templateVar distr by using an internally defined `predict` method, see details.
#' @templateVar lp by using an internally defined `predict` method, see details.
#'
#' @details
#' The predict method is based on [flexsurv::summary.flexsurvreg()] but is quicker and more
#' efficient for adapting the return type to be compatible with \CRANpkg{distr6}. A large amount of the
#' predict method is copied from [flexsurv::summary.flexsurvreg()] and credit goes to Christopher Jackson
#' for creating/maintaining \CRANpkg{flexsurv}.
#'
#' `lp` is predicted using the formula \eqn{lp = X\beta} where \eqn{X} are the variables in the test data set
#' and \eqn{\beta} are the fitted coefficients.
#'
#' `distr` is predicted using the formula derived in [flexsurv::flexsurvspline()] and combined with parameters
#' defined in [flexsurv::summary.flexsurvreg()].
#'
#' @references
#' Royston, P. and Parmar, M. (2002).
#' Flexible parametric proportional-hazards and proportional-odds models for censored survival data,
#' with application to prognostic modelling and estimation of treatment effects.
#' Statistics in Medicine, 21(15), 2175-2197.
#' \doi{10.1002/sim.1203}.
#'
#' @template seealso_learner
#' @export
LearnerSurvFlexible = R6Class("LearnerSurvFlexible", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.flexible",
        param_set = ParamSet$new(
          params = list(
            ParamInt$new(id = "k", default = 1L, lower = 0L, tags = "train"),
            ParamUty$new(id = "knots", tags = "train"),
            ParamUty$new(id = "bknots", tags = "train"),
            ParamFct$new(id ="scale", default = "odds", levels = c("hazard","odds","normal"), tags = "train"),
            ParamFct$new(id ="timescale", default = "log", levels = c("log","identity"), tags = "train"),
            ParamUty$new(id = "inits", tags = "train"),
            ParamUty$new(id = "fixedpars", tags = "train"),
            ParamDbl$new(id = "cl", default = 0.95, lower = 0, upper = 1, tags = "train"),
            ParamInt$new(id = "maxiter", default = 30L, tags = "train"),
            ParamDbl$new(id = "rel.tolerance", default = 1e-09, tags = "train"),
            ParamDbl$new(id = "toler.chol", default = 1e-10, tags = "train"),
            ParamInt$new(id = "outer.max", default = 10L, tags = "train")
          )),
        predict_types = c("distr","lp","crank"),
        feature_types = c("logical", "integer", "factor","numeric"),
        properties = c("weights"),
        packages = c("flexsurv", "survival", "distr6")
        )
      },

    train_internal = function(task) {
      # Passes control parameters to survreg.control
      pars_ctrl = c("maxiter","rel.tolerance","toler.chol","outer.max")
      pv = self$param_set$get_values(tags = "train")
      pv = pv[names(pv) %in% pars_ctrl]
      ctrl = invoke(survival::survreg.control, .args = pv)

      # Adds control and other set parameters to list
      pv = self$param_set$get_values(tags = "train")
      pv = pv[!(names(pv) %in% pars_ctrl)]
      pv$sr.control = ctrl

      # Changes the default values to be in line with Royston/Parmar. The current defaults are
      # equivalent to fitting a parametric model and therefore surv.parametric should be used
      # instead.
      if(length(pv$k) == 0) pv$k = 1
      if(length(pv$scale) == 0) pv$scale = "odds"

      if(pv$k == 0)
        message("Model fit with zero knots, consider using surv.parametric learner instead.")

      if ("weights" %in% task$properties)
        pv$weights = task$weights$weight

      invoke(flexsurv::flexsurvspline, formula =  task$formula(task$feature_names), data = task$data(), .args = pv)
      },

    predict_internal = function(task) {

      # As we are using a custom predict method the missing assertions are performed here manually
      # (as opposed to the automatic assertions that take place after prediction)
      if(any(is.na(data.frame(task$data(cols = task$feature_names)))))
        stop(sprintf("Learner %s on task %s failed to predict: Missing values in new data (line(s) %s)\n",
                     self$id, task$id, which(is.na(data.frame(task$data(cols = task$feature_names))))))

      pred = invoke(predict_flexsurvreg, self$model, task)

      # crank is defined as the mean of the survival distribution
      PredictionSurv$new(task = task, distr = pred$distr, lp = pred$lp, crank = pred$lp)
    }
  )
)

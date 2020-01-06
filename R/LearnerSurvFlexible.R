#' @include predict_flexsurvreg.R
#' @template surv_learner
#' @templateVar title Flexible Parametric Spline
#' @templateVar fullname LearnerSurvFlexible
#' @templateVar caller [flexsurv::flexsurvspline()]
#' @templateVar distr by using an internally defined `predict` method, see details
#' @templateVar lp by using an internally defined `predict` method, see details
#'
#' @description
#' Parameter `k` is changed to `1` and `scale` is changed to `odds`, as these are more in line with
#' the Royston/Parmar proposed models, and the package defaults are equivalent to fitting a
#' parametric model and therefore [surv.parametric][LearnerSurvParametric] should be used instead.
#'
#' If fitting a model with `k = 0` then consider using [surv.parametric][LearnerSurvParametric] as
#' this is likely to have more optimal results, and has more options for tuning.
#'
#' @details
#' The `distr` prediction is estimated using the fitted custom distributions
#' from [flexsurv::flexsurvspline()] and the estimated coefficients.
#'
#' As flexible spline models estimate the baseline hazard as the intercept, the linear predictor,
#' `lp`, can be calculated as in the classical setting. i.e. For fitted coefficients,
#' \eqn{\beta = (\beta_0,...,\beta_P)}{\beta = (\beta0,...,\betaP)},
#' and covariates \eqn{X^T = (X_0,...,X_P)^T}{X^T = (X0,...,XP)^T}, where \eqn{X_0}{X0} is a column
#' of \eqn{1}s: \eqn{lp = \beta X}{lp = \betaX}.
#'
#' @references
#' \cite{mlr3proba}{royston_2002}
#'
#' @template seealso_learner
#' @export
LearnerSurvFlexible = R6Class("LearnerSurvFlexible", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamInt$new(id = "k", default = 0L, lower = 0L, tags = "train"),
          ParamUty$new(id = "knots", tags = "train"),
          ParamUty$new(id = "bknots", tags = "train"),
          ParamFct$new(id ="scale", default = "hazard", levels = c("hazard","odds","normal"), tags = "train"),
          ParamFct$new(id ="timescale", default = "log", levels = c("log","identity"), tags = "train"),
          ParamUty$new(id = "inits", tags = "train"),
          ParamUty$new(id = "fixedpars", tags = "train"),
          ParamDbl$new(id = "cl", default = 0.95, lower = 0, upper = 1, tags = "train"),
          ParamInt$new(id = "maxiter", default = 30L, tags = "train"),
          ParamDbl$new(id = "rel.tolerance", default = 1e-09, tags = "train"),
          ParamDbl$new(id = "toler.chol", default = 1e-10, tags = "train"),
          ParamInt$new(id = "outer.max", default = 10L, tags = "train")
        ))

      ps$values = list(k = 1, scale = "odds")

      super$initialize(
        id = "surv.flexible",
        param_set = ps,
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

      if ("weights" %in% task$properties)
        pv$weights = task$weights$weight

      invoke(flexsurv::flexsurvspline, formula =  task$formula(task$feature_names), data = task$data(), .args = pv)
      },

    predict_internal = function(task) {

      # As we are using a custom predict method the missing assertions are performed here manually
      # (as opposed to the automatic assertions that take place after prediction)
      if(any(is.na(data.frame(task$data(cols = task$feature_names)))))
        stop(sprintf("Learner %s on task %s failed to predict: Missing values in new data (line(s) %s)\n",
                     self$id, task$id,
                     paste0(which(is.na(data.frame(task$data(cols = task$feature_names)))), collapse = ", ")))

      pred = invoke(predict_flexsurvreg, self$model, task)

      # crank is defined as the mean of the survival distribution
      PredictionSurv$new(task = task, distr = pred$distr, lp = pred$lp, crank = pred$lp)
    }
  )
)

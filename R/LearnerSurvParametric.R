#' @include predict.flexsurvreg.R
#'
#' @title Fully Parametric Survival Learner
#'
#' @usage NULL
#' @aliases mlr_learners_surv.parametric
#' @format [R6::R6Class] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @section Construction:
#' ```
#' LearnerSurvParametric$new()
#' mlr_learners$get("mlr_learners_surv.parametric")
#' lrn("mlr_learners_surv.parametric")
#' ```
#'
#' @description
#' A [LearnerSurv] for a Fully Parametric model partially implemented in
#' [survival::survreg()] in package \CRANpkg{survival}.
#'
#' @details
#' The \code{distr} return type is composed by using the formulae for proportional hazard (PH),
#' accelerated failure time (AFT), or proportional odds (PO) models, with the choice dependent on the
#' \code{type} hyper-parameter. \cr
#' The \code{lp} return type is predicted by computing \eqn{X\beta} for the coefficients \eqn{\beta}
#' fit in [survival::survreg()]. \cr
#' The \code{crank} return type is the same as the `lp`. \cr
#'
#' The predict method is based on [survival::predict.survreg()] but is more efficient for composition
#' to distributions. \cr
#'
#' Currently six parameterisations can be assumed for the baseline, see [survival::survreg()]. These
#' are internally re-parameterised and defined as \code{distr6} objects. We plan on implementing more
#' custom distributions in the future via \CRANpkg{distr6}.
#'
#' @references
#' Kalbfleisch, J. D., Prentice, R. L. (2002).
#' The Statistical Analysis of Failure Time Data.
#' John Wiley & Sons.
#' \doi{10.1002/9781118032985}.
#'
#' @template seealso_learner
#' @export

LearnerSurvParametric = R6Class("LearnerSurvParametric", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.parametric",
        param_set = ParamSet$new(
          params = list(
            ParamFct$new(id = "type", default = "aft", levels = c("aft","ph","po"), tags = "predict"),
            ParamFct$new(id = "dist", default = "weibull",
                         levels = c("weibull", "exponential", "gaussian", "logistic",
                                    "lognormal","loglogistic"), tags = "train"),
            ParamUty$new(id = "parms", tags = "train"),
            ParamUty$new(id = "init", tags = "train"),
            ParamDbl$new(id = "scale", default = 0, lower = 0, tags = "train"),
            ParamInt$new(id = "maxiter", default = 30L, tags = "train"),
            ParamDbl$new(id = "rel.tolerance", default = 1e-09, tags = "train"),
            ParamDbl$new(id = "toler.chol", default = 1e-10, tags = "train"),
            ParamInt$new(id = "outer.max", default = 10L, tags = "train"),
            ParamLgl$new(id = "robust", default = FALSE, tags = "train"),
            ParamLgl$new(id = "score", default = FALSE, tags = "train")
          )
        ),
        predict_types = c("distr","lp","crank"),
        feature_types = c("logical", "integer", "numeric", "factor"),
        properties = "weights",
        packages = c("survival", "distr6")
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
      pv$control = ctrl

      if ("weights" %in% task$properties) {
        pv$weights = task$weights$weight
      }

      fit = invoke(survival::survreg, formula = task$formula(), data = task$data(), .args = pv)

      # Fits the baseline distribution by reparameterising the fitted coefficients. These were mostly
      # derived numerically as precise documentation on the parameterisations is hard to find.
      location = as.numeric(fit$coefficients[1])
      scale = fit$scale
      eps = .Machine$double.xmin

      if(scale == 0)
        scale = eps

      if(location < -709 & fit$dist %in% c("weibull", "exponential", "loglogistic"))
        location = -709


      basedist = switch(fit$dist,
                 "gaussian" = distr6::Normal$new(mean = location, sd = scale,
                                                 decorators = "ExoticStatistics"),
                 "weibull" = distr6::Weibull$new(shape = 1/scale, scale = exp(location),
                                                 decorators = "ExoticStatistics"),
                 "exponential" = distr6::Exponential$new(scale = exp(location),
                                                         decorators = "ExoticStatistics"),
                 "logistic" = distr6::Logistic$new(mean = location, scale = scale,
                                                   decorators = "ExoticStatistics"),
                 "lognormal" = distr6::Lognormal$new(meanlog = location, sdlog = scale,
                                                     decorators = "ExoticStatistics"),
                 "loglogistic" = distr6::Loglogistic$new(scale = exp(location),
                                                         shape = 1/scale,
                                                         decorators = "ExoticStatistics")
      )

      set_class(list(fit = fit, basedist = basedist), "surv.parametric")
    },

    predict_internal = function(task) {

      # As we are using a custom predict method the missing assertions are performed here manually
      # (as opposed to the automatic assertions that take place after prediction)
      if(any(is.na(data.frame(task$data(cols = task$feature_names)))))
        stop(sprintf("Learner %s on task %s failed to predict: Missing values in new data (line(s) %s)\n",
                     self$id, task$id, which(is.na(data.frame(task$data(cols = task$feature_names))))))

      pv = self$param_set$get_values(tags = "predict")

      # Call the predict method defined in mlr3proba
      pred = invoke(predict_survreg, object = self$model, task = task, .args = pv)

      PredictionSurv$new(task = task, distr = pred$distr, crank = pred$lp, lp = pred$lp)
    }
  )
)

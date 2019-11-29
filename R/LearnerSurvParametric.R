#' @include predict_survreg.R
#' @template surv_learner
#' @templateVar title Fully Parametric
#' @templateVar fullname LearnerSurvParametric
#' @templateVar caller [survival::survreg()]
#' @templateVar distr by using an internally defined `predict` method, see details
#' @templateVar lp by using an internally defined `predict` method, see details
#'
#' @description
#' Currently six parameterisations can be assumed for the baseline, see [survival::survreg()]. These
#' are internally re-parameterised and defined as \CRANpkg{distr6} objects, we plan on implementing more
#' custom distributions in the future.
#'
#' @details
#' The internal predict method is implemented in `mlr3proba`, which is more efficient for
#' composition to distributions than [survival::predict.survreg()].
#'
#' `lp` is predicted using the formula \eqn{lp = X\beta} where \eqn{X} are the variables in the test
#' data set and \eqn{\beta} are the fitted coefficients.
#'
#' The distribution `distr` is composed using the `lp` and specifying a model form in the
#' `type` hyper-parameter. These are as follows, with respective hazard functions,
#' * Proportional Hazards (`ph`) \deqn{h(t) = h_0(t)exp(lp)}{h(t) = h0(t)*exp(lp)}
#' * Accelerated Failure Time (`aft`) \deqn{h(t) = exp(-lp)h_0(\frac{t}{exp(lp)})}{h(t) = exp(-lp)*h0(t/exp(lp))}
#' * Proportional Odds (`po`) \deqn{\frac{h(t)}{h_0(t)} = {1 + (exp(lp) - 1)S_0(t)}^{-1}}{h(t)/h0(t) = {1 + (exp(lp) - 1)*S0(t)}^-1}
#'
#' Where \eqn{h_0}{h0} and \eqn{S_0}{S0} are the estimated baseline hazard/survival respectively
#' (in this case with a given parametric form), and \eqn{lp} is the predicted linear predictor `lp`.
#'
#' @references
#' Kalbfleisch, J. D., Prentice, R. L. (2002).
#' The Statistical Analysis of Failure Time Data.
#' John Wiley & Sons.
#' \doi{10.1002/9781118032985}.
#'
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

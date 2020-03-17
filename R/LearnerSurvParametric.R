#' @include predict_survreg.R
#' @template surv_learner
#' @templateVar title Fully Parametric
#' @templateVar fullname LearnerSurvParametric
#' @templateVar caller [survival::survreg()]
#' @templateVar distr by using an internally defined `predict` method, see details
#' @templateVar lp by using an internally defined `predict` method, see details
#'
#' @description
#' This learner allows you to choose a distribution and a model form to compose a predicted
#' survival probability distribution. Note: Just because any combination of distribution and model
#' form is possible, this does not mean it will necessarily be sensible or interpretable.
#'
#' @details
#' The internal predict method is implemented in `mlr3proba`, which is more efficient for
#' composition to distributions than [survival::predict.survreg()].
#'
#' `lp` is predicted using the formula \eqn{lp = X\beta} where \eqn{X} are the variables in the test
#' data set and \eqn{\beta} are the fitted coefficients.
#'
#' The distribution `distr` is composed using the `lp` and specifying a model form in the
#' `type` hyper-parameter. These are as follows, with respective survival functions,
#' * Accelerated Failure Time (`aft`) \deqn{S(t) = S_0(\frac{t}{exp(lp)})}{S(t) = S0(t/exp(lp))}
#' * Proportional Hazards (`ph`) \deqn{S(t) = S_0(t)^{exp(lp)}}{S(t) = S0(t)^exp(lp)}
#' * Proportional Odds (`po`) \deqn{S(t) = \frac{S_0(t)}{exp(-lp) + (1-exp(-lp)) S_0(t)}}{S(t) = S0(t) / [exp(-lp) + S0(t) (1-exp(-lp))]}
#'
#' where \eqn{S_0}{S0} is the estimated baseline survival distribution (in this case
#' with a given parametric form), and \eqn{lp} is the predicted linear predictor.
#'
#' @references
#' \cite{mlr3proba}{kalbfleisch_2002}
#'
#' @export
LearnerSurvParametric = R6Class("LearnerSurvParametric", inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
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
        packages = c("survival", "distr6", "set6")
      )
    }
  ),

  private = list(
    .train = function(task) {
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

    .predict = function(task) {

      # As we are using a custom predict method the missing assertions are performed here manually
      # (as opposed to the automatic assertions that take place after prediction)
      if(any(is.na(data.frame(task$data(cols = task$feature_names)))))
        stop(sprintf("Learner %s on task %s failed to predict: Missing values in new data (line(s) %s)\n",
                     self$id, task$id,
                     paste0(which(is.na(data.frame(task$data(cols = task$feature_names)))), collapse = ", ")))

      pv = self$param_set$get_values(tags = "predict")

      # Call the predict method defined in mlr3proba
      pred = invoke(predict_survreg, object = self$model, task = task, .args = pv)

      if(is.null(self$param_set$values$type)){
        return(PredictionSurv$new(task = task, distr = pred$distr, crank = pred$lp, lp = pred$lp,
                                  response = exp(pred$lp)))
      } else if(self$param_set$values$type == "aft"){
        return(PredictionSurv$new(task = task, distr = pred$distr, crank = pred$lp, lp = pred$lp,
                                  response = exp(pred$lp)))
      } else {
        return(PredictionSurv$new(task = task, distr = pred$distr, crank = pred$lp, lp = pred$lp))
      }
    }
  )
)

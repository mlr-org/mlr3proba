#' @title Cross-Validated GLM with Elastic Net Regularization Survival Learner
#'
#' @usage NULL
#' @aliases mlr_learners_surv.cvglmnet
#' @format [R6::R6Class()] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @section Construction:
#' ```
#' LearnerSurvCVGlmnet$new()
#' mlr_learners$get("surv.cvglmnet")
#' lrn("surv.cvglmnet")
#' ```
#'
#' @description
#' Generalized linear models with elastic net regularization and k-fold cross-validation for lambda.
#' Calls [glmnet::cv.glmnet()] from package \CRANpkg{glmnet}.
#'
#' @details
#' The \code{distr} predict.type is derived by multiplying the crank, \eqn{exp(lp)}, returned from [glmnet::cv.glmnet()]
#' with a baseline hazard/survival calculated from [survival::survfit()]. The choice of estimator
#' for the baseline distribution can be determined by the \code{estimator} hyper-parameter.
#'
#' See [LearnerSurvCVGlmnet] for the glmnet implemented in [glmnet::glmnet()] without internal cross-validation.
#' Tuning using the internal optimizer in [glmnet::cv.glmnet()] may be more efficient when tuning
#' lambda only, however for tuning multiple hyperparameters, \CRANpkg{mlr3tuning} and [glmnet::glmnet()] will
#' likely give better results.
#'
#' @references
#' Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010).
#' Regularization Paths for Generalized Linear Models via Coordinate Descent.
#' Journal of Statistical Software, 33(1), 1-22.
#' \doi{10.18637/jss.v033.i01}.
#'
#' @export
#' @template seealso_learner
#' @examples
#' library(mlr3)
#' task = tgen("simsurv")$generate(200)
#' learner = lrn("surv.cvglmnet")
#' resampling = rsmp("cv", folds = 3)
#' resample(task, learner, resampling)
LearnerSurvCVGlmnet = R6Class("LearnerSurvCVGlmnet", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.cvglmnet",
        param_set = ParamSet$new(
          params = list(
            ParamFct$new(id = "estimator", default = "kaplan", levels = c("kaplan","nelson"), tags = "train"),
            ParamDbl$new(id = "alpha", default = 1, lower = 0, upper = 1, tags = "train"),
            ParamInt$new(id = "nfolds", lower = 3L, default = 10L, tags = "train"),
            ParamInt$new(id = "nlambda", default = 100L, lower = 1L, tags = "train"),
            ParamDbl$new(id = "lambda.min.ratio", lower = 0, upper = 1, tags = "train"),
            ParamUty$new(id = "lambda", tags = "train"),
            ParamLgl$new(id = "standardize", default = TRUE, tags = "train"),
            ParamLgl$new(id = "intercept", default = TRUE, tags = "train"),
            ParamDbl$new(id = "thresh", default = 1e-07, lower = 0, tags = "train"),
            ParamInt$new(id = "dfmax", lower = 0L, tags = "train"),
            ParamInt$new(id = "pmax", lower = 0L, tags = "train"),
            ParamUty$new(id = "exclude", tags = "train"),
            ParamDbl$new(id = "penalty.factor", lower = 0, upper = 1, tags = "train"),
            ParamUty$new(id = "lower.limits", default = -Inf, tags = "train"),
            ParamUty$new(id = "upper.limits", default = Inf, tags = "train"),
            ParamInt$new(id = "maxit", default = 100000L, lower = 1L, tags = "train"),
            ParamFct$new(id = "type.logistic", default = "Newton", levels = c("Newton", "modified.Newton"), tags = "train"),
            ParamFct$new(id = "type.multinomial", default = "ungrouped", levels = c("ungrouped", "grouped"), tags = "train"),
            ParamDbl$new(id = "fdev", default = 1.0e-5, lower = 0, upper = 1, tags = "train"),
            ParamDbl$new(id = "devmax", default = 0.999, lower = 0, upper = 1, tags = "train"),
            ParamDbl$new(id = "eps", default = 1.0e-6, lower = 0, upper = 1, tags = "train"),
            ParamDbl$new(id = "big", default = 9.9e35, tags = "train"),
            ParamInt$new(id = "mnlam", default = 5L, lower = 1L, tags = "train"),
            ParamDbl$new(id = "pmin", default = 1.0e-9, lower = 0, upper = 1, tags = "train"),
            ParamDbl$new(id = "exmx", default = 250.0, tags = "train"),
            ParamDbl$new(id = "prec", default = 1e-10, tags = "train"),
            ParamInt$new(id = "mxit", default = 100L, lower = 1L, tags = "train"),
            ParamDbl$new(id = "s", lower = 0, upper = 1, special_vals = list("lambda.1se", "lambda.min"), default = "lambda.1se", tags = "predict")
          )
        ),
        feature_types = c("integer", "numeric"),
        predict_types = c("distr","crank","lp"),
        properties = "weights",
        packages = c("glmnet","distr6","survival")
      )
    },

    train_internal = function(task) {

      pars = self$param_set$get_values(tags = "train")
      # estimator parameter is used internally for composition (i.e. outside of glmnet) and is
      # thus ignored for now
      pars$estimator = NULL

      x = as.matrix(task$data(cols = task$feature_names))
      target = task$truth()
      if ("weights" %in% task$properties) {
        pars$weights = task$weights$weight
      }

      # Save control settings and return on exit
      saved_ctrl = glmnet::glmnet.control()
      on.exit(invoke(glmnet::glmnet.control, .args = saved_ctrl))
      glmnet::glmnet.control(factory = TRUE)
      is_ctrl_pars = (names(pars) %in% names(saved_ctrl))

      # ensure only relevant pars passed to fitted model
      if (any(is_ctrl_pars)) {
        do.call(glmnet::glmnet.control, pars[is_ctrl_pars])
        pars = pars[!is_ctrl_pars]
      }

      fit = invoke(glmnet::cv.glmnet, x = x, y = target, family = "cox", .args = pars)

      # for composition fit the baseline distribution
      basehaz = invoke(survival::survfit, formula = task$formula(1), data = task$data())

      # Kaplan-Meier estimator used by default
      estimator = self$param_set$values$estimator
      if(length(estimator) == 0) estimator = "kaplan"
      # survfit uses Nelson-Aalen in chf and Kaplan-Meier for survival, as these
      # don't give equivalent results one must be chosen and the relevant functions are transformed
      # as required.
      if(estimator == "nelson")
        basesurv = exp(-basehaz$cumhaz)
      else
        basesurv = basehaz$surv

      set_class(list(fit = fit, basesurv = basesurv, times = basehaz$time), "surv.cvglmnet")
    },

    predict_internal = function(task) {
      pars = self$param_set$get_values(tags = "predict")
      newdata = as.matrix(task$data(cols = task$feature_names))
      if(length(pars$s) == 0)
        pars$s = round(self$model$fit$lambda.1se, 6)
      else
        pars$s = round(pars$s, 6)

      # crank defined as the exponential of the linear predictor. we calculate exponential here
      # to speed up the lapply below but return to lp after
      risk = exp(invoke(predict, self$model$fit, newx = newdata, type = "link", .args = pars))

      # WeightedDiscrete distribution defined as a PH model with the baseline survival to the power of
      # the relative crank. Note here crank is identical to `risk` in the glmnet documentation, i.e. exp(lp)
      # but below crank is updated to be the mean of the survival distribution.
      distr = list()
      crank = c()
      for(x in 1:length(risk)){
        distr = c(distr, list(suppressAll(
          distr6::WeightedDiscrete$new(data.frame(x = self$model$times,
                                                  cdf = (1 - self$model$basesurv^risk[x])),
                                       decorators = c(distr6::CoreStatistics, distr6::ExoticStatistics)))))
        # crank defined as mean of survival distribution. The ranking of crank and lp is identical.
        crank = c(crank, distr[[x]]$mean())
      }

      # lp defined as fitted coefficients multiplied by new data covariates
      lp = drop(log(crank))

      PredictionSurv$new(task = task, distr = distr, crank = crank, lp = lp)
    }
  )
)

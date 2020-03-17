#' @template surv_learner
#' @templateVar title Cross-Validated GLM with Elastic Net Regularization
#' @templateVar fullname LearnerSurvCVGlmnet
#' @templateVar caller [glmnet::cv.glmnet()]
#' @templateVar lp by [glmnet::predict.cv.glmnet()]
#'
#' @description
#' Use [LearnerSurvGlmnet] and [LearnerSurvCVGlmnet] for glmnets without and with internal
#' cross-validation, respectively. Tuning using the internal optimizer in [LearnerSurvCVGlmnet]
#' may be more efficient when tuning lambda only. However, for tuning multiple hyperparameters,
#' \CRANpkg{mlr3tuning} and [LearnerSurvGlmnet] will likely give better results.
#'
#' @references
#' \cite{mlr3proba}{friedman_2010}
#'
#' @export
LearnerSurvCVGlmnet = R6Class("LearnerSurvCVGlmnet", inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "surv.cvglmnet",
        param_set = ParamSet$new(
          params = list(
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
        feature_types = c("integer", "numeric", "factor"),
        predict_types = c("crank","lp"),
        properties = "weights",
        packages = c("glmnet","survival")
      )
    }
  ),

  private = list(
    .train = function(task) {

      pars = self$param_set$get_values(tags = "train")

      # convert data to model matrix
      x = model.matrix(~., as.data.frame(task$data(cols = task$feature_names)))

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

      invoke(glmnet::cv.glmnet, x = x, y = target, family = "cox", .args = pars)
    },

    .predict = function(task) {
      pars = self$param_set$get_values(tags = "predict")

      # convert data to model matrix
      newdata = model.matrix(~., as.data.frame(task$data(cols = task$feature_names)))

      if(length(pars$s) == 0)
        pars$s = round(self$model$lambda.1se, 6)
      else
        pars$s = round(pars$s, 6)

      # predict linear predictor
      lp = as.numeric(invoke(predict, self$model, newx = newdata, type = "link", .args = pars))

      PredictionSurv$new(task = task, crank = lp, lp = lp)
    }
  )
)

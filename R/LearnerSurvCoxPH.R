#' @title Cox Proportional Hazard Learner
#'
#' @usage NULL
#' @aliases mlr_learners_surv.coxph
#' @format [R6::R6Class] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @section Construction:
#' ```
#' LearnerSurvCoxPH$new()
#' mlr_learners$get("surv.coxph")
#' lrn("surv.coxph")
#' ```
#'
#' @description
#' A [LearnerSurv] for a Cox PH model implemented in [survival::coxph()] in package \CRANpkg{survival}.
#'
#' @details
#' The \code{distr} return type is given natively by predicting the survival function in [survival::survfit.coxph()],
#' the method used for estimating the baseline hazard depends on the \code{type} hyper-parameter.\cr
#' The \code{crank} return type is defined by the expectation of the survival distribution.\cr
#' The \code{lp} return type is given natively by [survival::predict.coxph()].
#' The ranking given by \code{crank} and \code{lp} is identical.
#'
#' @references
#' Cox, David R. (1972).
#' Regression models and life‐tables.
#' Journal of the Royal Statistical Society: Series B (Methodological) 34.2 (1972): 187-202.
#' \doi{10.1111/j.2517-6161.1972.tb00899.x}.
#'
#' @template seealso_learner
#' @export
LearnerSurvCoxPH = R6Class("LearnerSurvCoxPH", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.coxph",
        param_set = ParamSet$new(
          params = list(
            ParamFct$new(id = "ties", default = "efron", levels = c("efron", "breslow", "exact"), tags = "train"),
            ParamLgl$new(id = "singular.ok", default = TRUE, tags = "train"),
            ParamFct$new(id = "type", default = "efron", levels = c("efron", "aalen", "kalbfleisch-prentice"), tags = "predict")
          )
        ),
        predict_types = c("distr","crank","lp"),
        feature_types = c("logical", "integer", "numeric", "factor"),
        properties = c("weights","importance"),
        packages = c("survival", "distr6")
      )
    },

    train_internal = function(task) {
      pv = self$param_set$get_values(tags = "train")

      if ("weights" %in% task$properties) {
        pv$weights = task$weights$weight
      }

      invoke(survival::coxph, formula = task$formula(), data = task$data(), .args = pv, x = TRUE)
    },

    predict_internal = function(task) {

      newdata = task$data(cols = task$feature_names)

      # We move the missingness checks here manually as if any NAs are made in predictions then the
      # distribution object cannot be create (initialization of distr6 objects does not handle NAs)
      if(any(is.na(data.frame(task$data(cols = task$feature_names)))))
        stop(sprintf("Learner %s on task %s failed to predict: Missing values in new data (line(s) %s)\n",
                     self$id, task$id, which(is.na(data.frame(task$data(cols = task$feature_names))))))

      pv = self$param_set$get_values(tags = "predict")

      # Get predicted values
      fit = invoke(survival::survfit, formula = self$model, newdata = newdata, se.fit = FALSE, .args = pv)

      # define WeightedDiscrete distr6 object from predicted survival function
      x = rep(list(data = data.frame(x = fit$time, cdf = 0)), task$nrow)
      for(i in 1:task$nrow)
        x[[i]]$cdf = 1 - fit$surv[, i]

      distr = distr6::VectorDistribution$new(distribution = "WeightedDiscrete", params = x,
                                             decorators = c("CoreStatistics", "ExoticStatistics"))

      lp = predict(self$model, type = "lp", newdata = newdata)

      # note the ranking of lp and crank is identical
      PredictionSurv$new(task = task, crank = lp, distr = distr, lp = lp)
    },

    importance = function() {
     if (is.null(self$model)) {
       stopf("No model stored")
     }
      # coefficient importance defined by the p-values
      sort(1-summary(self$model)$coefficients[,5L], decreasing = TRUE)
    },

    selected_features = function() {

    if (is.null(self$model)) {
      stopf("No model stored")
    }

    # features are selected if their coefficients are non-NA
    beta = coef(self$model)
    names(beta)[!is.na(beta)]
    }
  )
)

#' @title L1 and L2 Penalized Estiamtion in GLMs Survival Learner
#'
#' @usage NULL
#' @aliases mlr_learners_surv.penalized
#' @format [R6::R6Class()] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @section Construction:
#' ```
#' LearnerSurvPenalized$new()
#' mlr_learners$get("surv.penalized")
#' lrn("surv.penalized")
#' ```
#'
#' @description
#' Generalized linear models with elastic net regularization.
#' Calls [penalized::penalized()] from package \CRANpkg{penalized}.
#'
#' @details
#' The \code{distr} return type is given natively by predicting the survival function in [penalized::predict()].\cr
#' The \code{crank} return type is defined by the expectation of the survival distribution.
#'
#' @references
#' Goeman, J. J., L1 penalized estimation in the Cox proportional hazards model.
#' Biometrical Journal 52(1), 70{84}.
#'
#' @export
#' @template seealso_learner
#' @examples
#' library(mlr3)
#' task = tgen("simsurv")$generate(200)
#' learner = lrn("surv.penalized")
#' resampling = rsmp("cv", folds = 3)
#' resample(task, learner, resampling)
LearnerSurvPenalized = R6Class("LearnerSurvPenalized", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.penalized",
        param_set = ParamSet$new(
          params = list(
            ParamUty$new(id = "unpenalized", tags = c("train","predict")),
            ParamUty$new(id = "lambda1", default = 0, tags = "train"),
            ParamUty$new(id = "lambda2", default = 0, tags = "train"),
            ParamLgl$new(id = "positive", default = FALSE, tags = "train"),
            ParamLgl$new(id = "fusedl", default = FALSE, tags = "train"),
            ParamDbl$new(id = "startbeta", tags = "train"),
            ParamDbl$new(id = "startgamma", tags = "train"),
            ParamInt$new(id = "steps", lower = 1L, default = 1L, tags = "train"),
            ParamDbl$new(id = "epsilon", default = 1.0e-10, lower = 0, upper = 1, tags = "train"),
            ParamInt$new(id = "maxiter", lower = 1, tags = "train"),
            ParamLgl$new(id = "standardize", default = FALSE, tags = "train"),
            ParamLgl$new(id = "trace", default = TRUE, tags = "train")
            )
          ),
        feature_types = c("integer", "numeric","factor","ordered"),
        predict_types = c("distr","crank"),
        properties = "importance",
        packages = c("penalized","distr6")
        )
      },

    train_internal = function(task) {

      # Checks missing data early to prevent crashing
      if(any(task$missings() > 0))
        stop("Missing data is not supported by ", self$id)

      # Changes the structure of the penalized and unpenalized parameters to be more user friendly.
      # Now the user supplies the column names as a vector and these are added to the formula as
      # required.
      pars = self$param_set$get_values(tags = "train")
      if (length(pars$unpenalized) == 0)
        penalized = formulate(rhs = task$feature_names)
      else {
        penalized = formulate(rhs = task$feature_names[task$feature_names %nin% pars$unpenalized])
        pars$unpenalized = formulate(rhs = pars$unpenalized)
      }

      suppressAll(invoke(penalized::penalized, response = task$truth(), penalized = penalized,
             data = task$data(cols = task$feature_names), model = "cox", .args = pars))
      },

    predict_internal = function(task) {
      # Again the penalized and unpenalized covariates are automatically converted to the
      # correct formula
      pars = self$param_set$get_values(tags = "predict")
      if(length(pars$unpenalized) == 0)
        penalized = formulate(rhs = task$feature_names)
      else{
        penalized = formulate(rhs = task$feature_names[task$feature_names %nin% pars$unpenalized])
        pars$unpenalized = formulate(rhs = pars$unpenalized)
      }

      surv = invoke(penalized::predict, self$model, penalized = penalized, data = task$data(cols = task$feature_names),
                    .args = pars)

      # define WeightedDiscrete distr6 object from predicted survival function
      distr_crank = suppressAll(apply(1 - surv@curves, 1, function(x){
        distr = distr6::WeightedDiscrete$new(data.frame(x = fit$unique.death.times, cdf = x),
                                             decorators = c(distr6::CoreStatistics, distr6::ExoticStatistics))

        # crank defined as mean of survival distribution.
        crank = distr$mean()

        return(list(distr = distr, crank = crank))
      }))

      PredictionSurv$new(task = task,
                         crank = as.numeric(unlist(distr_crank)[seq.int(2, length(distr_crank)*2, 2)]),
                         distr = unname(unlist(distr_crank)[seq.int(1, length(distr_crank)*2, 2)]))
      },

    importance = function() {
      if (is.null(self$model))
        stopf("No model stored")

      # importance defined by decreasing fitted weights
      sort(self$model@weights, decreasing = TRUE)
      }
    )
)

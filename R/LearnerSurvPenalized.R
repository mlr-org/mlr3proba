#' @importFrom penalized contr.none contr.diff
#'
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
        predict_types = c("distr","risk"),
        properties = "importance",
        packages = c("penalized","distr6")
        )
      },

    train_internal = function(task) {

      if(any(task$missings() > 0))
        stop("Missing data is not supported by ", self$id)

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
      pars = self$param_set$get_values(tags = "predict")
      if(length(pars$unpenalized) == 0)
        penalized = formulate(rhs = task$feature_names)
      else{
        penalized = formulate(rhs = task$feature_names[task$feature_names %nin% pars$unpenalized])
        pars$unpenalized = formulate(rhs = pars$unpenalized)
      }

      surv = invoke(penalized::predict, self$model, penalized = penalized, data = task$data(cols = task$feature_names),
                    .args = pars)

      distr = apply(surv@curves, 1, function(x)
        suppressAll(distr6::WeightedDiscrete$new(data.frame(x = surv@time, cdf = 1 - x),
                                         decorators = c(distr6::CoreStatistics, distr6::ExoticStatistics)))
      )

      risk = rowMeans(-log(surv@curves))

      PredictionSurv$new(task = task, distr = distr, risk = as.numeric(risk))
      },

    importance = function() {
      if (is.null(self$model))
        stopf("No model stored")

      sort(self$model@weights, decreasing = TRUE)
      }
    )
)

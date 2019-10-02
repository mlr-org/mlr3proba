#' @importFrom survivalsvm makediff1 makediff2 makediff3
#'
#' @title Regression, Ranking and Hybrid Support Vector Machine Survival Learner
#'
#' @usage NULL
#' @aliases mlr_learners_surv.svm
#' @format [R6::R6Class()] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @section Construction:
#' ```
#' LearnerSurvSVM$new()
#' mlr_learners$get("surv.svm")
#' lrn("surv.svm")
#' ```
#'
#' @description
#' Support vector machines predicting either survival time, relative ranks or hybrid of risks.
#' Calls [survivalsvm::survivalsvm()] from package \CRANpkg{survivalsvm}.
#'
#' @details
#' Four possible models can be implemented, dependent on the \code{model} parameter. These correspond
#' to predicting the prognosting index (PI) via regression, \code{regression}; predicting the PI by
#' imposing a ranking constrant, \code{vanbelle1, vanbelle2}; a hybrid of the two \code{hybrid}. Whilst
#' \code{regression} is the default, \code{hybrid} may be more efficient in tuning as it can be reduced
#' to the previous depending on optimal parameters.
#'
#'
#' @references
#' Van Belle, V., Pelcmans, K., Van Huffel S. and Suykens J. A.K. (2011a). Improved performance on
#' high-dimensional survival data by application of Survival-SVM.
#' Bioinformatics (Oxford, England) 27, 87-94.
#'
#' Van Belle, V., Pelcmans, K., Van Huffel S. and Suykens J. A.K. (2011b). Support vector methods
#' for survival analysis: a comparaison between ranking and regression approaches.
#' Artificial Intelligence in medecine 53, 107-118.
#'
#' @export
#' @template seealso_learner
#' @examples
#' library(mlr3)
#' task = tgen("simsurv")$generate(200)
#' learner = lrn("surv.svm")
#' resampling = rsmp("cv", folds = 3)
#' resample(task, learner, resampling)
LearnerSurvSVM = R6Class("LearnerSurvSVM", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.svm",
        param_set = ParamSet$new(
          params = list(
            ParamFct$new(id = "type", default = "regression", levels = c("regression", "vanbelle1", "vanbelle2", "hybrid"), tags = "train"),
            ParamFct$new(id = "diff.meth", default ="makediff3", levels = c("makediff1", "makediff2", "makediff3"), tags = "train"),
            ParamUty$new(id = "gamma.mu", default = 0.1, tags = "train"),
            ParamFct$new(id = "opt.meth", default = "quadprog", levels = c("quadprog", "ipop"), tags = "train"),
            ParamFct$new(id = "kernel", default = "lin_kernel", levels = c("lin_kernel", "add_kernel","rbf_kernel","poly_kernel"), tags = "train"),
            ParamUty$new(id = "kernel.pars", tags = "train"),
            ParamInt$new(id = "sgf.sv", default = 5L, lower = 0L, tags = "train"),
            ParamInt$new(id = "sigf", default = 7L, lower = 0L, tags = "train"),
            ParamInt$new(id = "maxiter", default = 20L, lower = 0L, tags = "train"),
            ParamDbl$new(id = "margin", default = 0.05, lower = 0, tags = "train"),
            ParamDbl$new(id = "bound", default = 10, lower = 0, tags = "train"),
            ParamDbl$new(id = "eig.tol", default = 1e-06, lower = 0, tags = "train"),
            ParamDbl$new(id = "conv.tol", default = 1e-07, lower = 0, tags = "train"),
            ParamDbl$new(id = "posd.tol", default = 1e-08, lower = 0, tags = "train")
            )
          ),
        feature_types = c("integer", "numeric","factor"),
        #   properties = "importance",
        predict_types = c("risk"),
        packages = c("survivalsvm")
        )
      },
    train_internal = function(task) {

      pars = self$param_set$get_values(tags = "train")
      if(length(pars$diff.meth) == 0)
        pars$diff.meth = "makediff3"
      if(length(pars$gamma.mu) == 0)
        pars$gamma.mu = 0.1

      invoke(survivalsvm::survivalsvm, formula = task$formula(), data = task$data(), .args = pars)
      },

    predict_internal = function(task) {
      fit = predict(self$model, newdata = task$data(cols = task$feature_names))
      PredictionSurv$new(task = task, risk = as.numeric(fit$predicted))
    }
  )
)

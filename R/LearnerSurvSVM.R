#' @template surv_learner
#' @templateVar title Regression, Ranking and Hybrid Support Vector Machines
#' @templateVar fullname LearnerSurvSVM
#' @templateVar caller [survivalsvm::survivalsvm()]
#' @templateVar crank by [survivalsvm::predict.survivalsvm()]
#'
#' @description
#' Four possible SVMs can be implemented, dependent on the `type` parameter. These correspond
#' to predicting the survival time via regression (`regression`), predicting a continuous rank
#' (`vanbelle1`, `vanbelle2`), or a hybrid of the two (`hybrid`).
#' Whichever `type` is chosen determines how the `crank` predict type is calculated,
#' but in any case all can be considered a valid continuous ranking.
#'
#' To be in line with the Van Belle papers and to prevent the learner crashing without
#' user-set parameters, the default `diff.meth` is set to `diffmeth3` with `gamma.mu` equal to 0.1.
#'
#' @references
#' \cite{mlr3proba}{vanbelle_2010}
#'
#' \cite{mlr3proba}{vanbelle_2011}
#'
#' @export
LearnerSurvSVM = R6Class("LearnerSurvSVM", inherit = LearnerSurv,
  public = list(
    initialize = function() {

      ps = ParamSet$new(
        params = list(
          ParamFct$new(id = "type", default = "regression", levels = c("regression", "vanbelle1", "vanbelle2", "hybrid"), tags = "train"),
          ParamFct$new(id = "diff.meth", levels = c("makediff1", "makediff2", "makediff3"), tags = "train"),
          ParamUty$new(id = "gamma.mu", tags = "train"),
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
      )

      ps$values = insert_named(ps$values, list(diff.meth = "makediff3", gamma.mu = 0.1))

      super$initialize(
        id = "surv.svm",
        param_set = ps,
        feature_types = c("integer", "numeric"),
        #   properties = "importance",
        predict_types = c("crank"),
        packages = c("survivalsvm")
        )
      },
    train_internal = function(task) {
      invoke(survivalsvm::survivalsvm,
             formula = task$formula(),
             data = task$data(),
             .args = self$param_set$get_values(tags = "train"))
      },

    predict_internal = function(task) {
      # only a continuous ranking is returned
      fit = predict(self$model, newdata = task$data(cols = task$feature_names))
      PredictionSurv$new(task = task, crank = as.numeric(fit$predicted))
    }
  )
)

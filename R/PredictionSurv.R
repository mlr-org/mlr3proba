#' @title Prediction Object for Survival
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerSurv].
#'
#' The `task_type` is set to `"surv"`.
#'
#' @family Prediction
#' @export
#' @examples
#' if (requireNamespace("rpart", quietly = TRUE)) {
#' library(mlr3)
#' task = tsk("rats")
#' learner = mlr_learners$get("surv.rpart")
#' p = learner$train(task, row_ids = 1:20)$predict(task, row_ids = 21:30)
#' head(as.data.table(p))
#' }
PredictionSurv = R6Class("PredictionSurv",
  inherit = Prediction,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([TaskSurv])\cr
    #'   Task, used to extract defaults for `row_ids` and `truth`.
    #'
    #' @param row_ids (`integer()`)\cr
    #'   Row ids of the predicted observations, i.e. the row ids of the test set.
    #'
    #' @param truth (`survival::Surv()`)\cr
    #'   True (observed) response.
    #'
    #' @param crank (`numeric()`)\cr
    #'   Numeric vector of predicted continuous rankings (or relative risks). One element for each
    #'   observation in the test set. For a pair of continuous ranks, a higher rank indicates that
    #'   the observation is more likely to experience the event.
    #'
    #' @param distr ([VectorDistribution][distr6::VectorDistribution])\cr
    #'   [VectorDistribution][distr6::VectorDistribution] from \CRANpkg{distr6}.
    #'   Each individual distribution in the vector represents the random variable 'survival time'
    #'   for an individual observation.
    #'
    #' @param lp (`numeric()`)\cr
    #'   Numeric vector of linear predictor scores. One element for each observation in the test
    #'   set. \eqn{lp = X\beta} where \eqn{X} is a matrix of covariates and \eqn{\beta} is a vector
    #'   of estimated coefficients.
    #'
    #' @param response (`numeric()`)\cr
    #'   Numeric vector of predicted survival times.
    #'   One element for each observation in the test set.
    #'
    #' @param check (`logical(1)`)\cr
    #'   If `TRUE`, performs argument checks and predict type conversions.
    initialize = function(task = NULL, row_ids = task$row_ids, truth = task$truth(), crank = NULL,
      distr = NULL, lp = NULL, response = NULL, check = TRUE) {

      pdata = list(row_ids = row_ids, truth = truth, crank = crank, distr = distr, lp = lp, response = response)
      pdata = discard(pdata, is.null)
      class(pdata) = c("PredictionDataSurv", "PredictionData")

      if (check) {
        pdata = check_prediction_data(pdata)
      }

      self$task_type = "surv"
      self$man = "mlr3proba::PredictionSurv"
      self$data = pdata
      self$predict_types = intersect(c("crank", "distr", "lp", "response"), names(pdata))
    }
  ),

  active = list(
    #' @field truth (`Surv`)\cr
    #'   True (observed) outcome.
    truth = function() {
      self$data$truth
    },

    #' @field crank (`numeric()`)\cr
    #' Access the stored predicted continuous ranking.
    crank = function() {
      self$data$crank %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field distr ([VectorDistribution][distr6::VectorDistribution])\cr
    #' Access the stored predicted survival distribution.
    distr = function() {
      self$data$distr %??% NA_real_
    },

    #' @field lp (`numeric()`)\cr
    #' Access the stored predicted linear predictor.
    lp = function() {
      self$data$lp %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field response (`numeric()`)\cr
    #' Access the stored predicted survival time.
    response = function() {
      self$data$response %??% rep(NA_real_, length(self$data$row_ids))
    }
  ),

  private = list(
    .censtype = NULL
  )
)


#' @export
as.data.table.PredictionSurv = function(x, ...) { # nolint
  tab = as.data.table(x$data[c("row_ids", "crank", "lp", "response")])
  setnames(tab, "row_ids", "row_id")
  tab$time = x$data$truth[, 1L]
  tab$status = as.logical(x$data$truth[, 2L])
  if ("distr" %in% x$predict_types) {
    tab$distr = list(list(x$distr))
  }
  setcolorder(tab, c("row_id", "time", "status"))[]
}

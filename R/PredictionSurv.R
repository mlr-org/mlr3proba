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
#' library(mlr3)
#' task = tsk("rats")
#' learner = lrn("surv.kaplan")
#' p = learner$train(task, row_ids = 1:20)$predict(task, row_ids = 21:30)
#' head(as.data.table(p))
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
    #' @param distr (`matrix()|[distr6::VectorDistribution]`)\cr
    #'   Either a matrix of predicted survival probabilities or a [distr6::VectorDistribution].
    #'   If a matrix then column names must be given and correspond to survival times.
    #'   Rows of matrix correspond to individual predictions. It is advised that the
    #'   first column should be time `0` with all entries `1` and the last
    #'   with all entries `0`. If a `VectorDistribution` then each distribution in the vector
    #'   should correspond to a predicted survival distribution.
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

      if (inherits(distr, "Distribution")) {
        # coerce to matrix if possible
        distr <- private$.simplify_distr(distr)
      }

      pdata = list(row_ids = row_ids, truth = truth, crank = crank, distr = distr, lp = lp,
                   response = response)
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
    #' Convert the stored survival matrix to a survival distribution.
    distr = function() {
      private$.distrify_survmatrix(self$data$distr %??% NA_real_)
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
    .censtype = NULL,
    .distr = function() self$data$distr %??% NA_real_,
    .simplify_distr = function(x) {
      if (!inherits(x, "VectorDistribution")) {
        stop("'x' is not a 'VectorDistribution'")
      }

      ## check all distributions equal - return x if not
      if (x$distlist) {
        return(x)
      }

      ## check all distributions are WeightedDiscrete - return x if not
      if (all(x$modelTable$Distribution != "WeightedDiscrete")) {
        return(x)
      }

      times = x$getParameterValue("x")
      time1 = times[[1]]

      ## check all times equal - return x if not
      if (!all(vapply(times, identical, logical(1), y = time1))) {
        return(x)
      }

      surv <- 1 - do.call(rbind, x$getParameterValue("cdf"))
      rownames(surv) <- NULL
      surv
    },
    .distrify_survmatrix = function(x) {

      if (inherits(x, "Distribution")) {
        return(x)
      }

      assert(all(x[, 1] == 1))
      assert(all(x[, ncol(x)] == 0))

      distr6::as.Distribution(
        1 - x,
        fun = "cdf",
        decorators = c("CoreStatistics", "ExoticStatistics")
      )
    }
  )
)


#' @export
as.data.table.PredictionSurv = function(x, ...) { # nolint

  tab = as.data.table(x$data[c("row_ids", "crank", "lp", "response")])
  tab$time = x$data$truth[, 1L]
  tab$status = as.logical(x$data$truth[, 2L])
  if ("distr" %in% x$predict_types) {
    # annoyingly need this many lists to get nice printing
    tab$distr = list(list(list(r6_private(x)$.distr())))
  }
  setcolorder(tab, c("row_ids", "time", "status"))[]
}

#' @title Prediction Object for Survival
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerSurv].
#'
#' The `task_type` is set to `"surv"`.
#'
#' For accessing survival and hazard functions, as well as other complex methods
#' from a [PredictionSurv] object, see public methods on [distr6::ExoticStatistics()]
#' and example below.
#'
#' @family Prediction
#' @examples
#' library(mlr3)
#' task = tsk("rats")
#' learner = lrn("surv.kaplan")
#' p = learner$train(task, row_ids = 1:26)$predict(task, row_ids = 27:30)
#' head(as.data.table(p))
#'
#' p$distr # distr6::Matdist class (test obs x time points)
#'
#' # survival probabilities of the 4 test rats at two time points
#' p$distr$survival(c(20, 100))
#' @export
PredictionSurv = R6Class("PredictionSurv",
  inherit = Prediction,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @details
    #' Upon **initialization**, if the `distr` input is a [Distribution][distr6::Distribution],
    #' we try to coerce it either to a survival matrix or a survival array and store it
    #' in the `$data$distr` slot for internal use.
    #'
    #' If the stored `$data$distr` is a [Distribution][distr6::Distribution] object,
    #' the active field `$distr` (**external user API**) returns it without modification.
    #' Otherwise, if `$data$distr` is a survival matrix or array, `$distr`
    #' constructs a distribution out of the `$data$distr` object, which will be a
    #' [Matdist][distr6::Matdist] or [Arrdist][distr6::Arrdist] respectively.
    #'
    #' Note that if a survival 3d array is stored in `$data$distr`, the `$distr`
    #' field returns an [Arrdist][distr6::Arrdist] initialized with `which.curve = 0.5`
    #' by default (i.e. the median curve). This means that measures that require
    #' a `distr` prediction like [MeasureSurvGraf], [MeasureSurvRCLL], etc.
    #' will use the median survival probabilities.
    #' Note that it is possible to manually change `which.curve` after construction
    #' of the predicted distribution but we advise against this as it may lead to
    #' inconsistent results.
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
    #' @param distr (`matrix()|[distr6::Arrdist]|[distr6::Matdist]|[distr6::VectorDistribution]`)\cr
    #'   Either a matrix of predicted survival probabilities, a [distr6::VectorDistribution],
    #'   a [distr6::Matdist] or an [distr6::Arrdist].
    #'   If a matrix/array then column names must be given and correspond to survival times.
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
        # coerce to matrix/array if possible
        distr = private$.simplify_distr(distr)
      }

      pdata = list(row_ids = row_ids, truth = truth, crank = crank, distr = distr,
                   lp = lp, response = response)
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
    #' True (observed) outcome.
    truth = function() {
      self$data$truth
    },

    #' @field crank (`numeric()`)\cr
    #' Access the stored predicted continuous ranking.
    crank = function() {
      self$data$crank %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field distr ([distr6::Matdist]|[distr6::Arrdist]|[distr6::VectorDistribution])\cr
    #' Convert the stored survival array or matrix to a survival distribution.
    distr = function() {
      if (inherits(self$data$distr, "Distribution")) {
        return(self$data$distr)
      }

      private$.distrify_survarray(self$data$distr)
    },

    #' @field lp (`numeric()`)\cr
    #' Access the stored predicted linear predictor.
    lp = function() {
      self$data$lp
    },

    #' @field response (`numeric()`)\cr
    #' Access the stored predicted survival time.
    response = function() {
      self$data$response
    }
  ),

  private = list(
    .distr = function() self$data$distr %??% NA_real_,
    .simplify_distr = function(x) {
      if (inherits(x, c("Matdist", "Arrdist"))) {
        1 - gprm(x, "cdf") # matrix or 3d array
      } else {
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
        time1 = times[[1L]]

        ## check all times equal - return x if not
        if (!all(map_lgl(times, identical, y = time1))) {
          return(x)
        }

        surv = 1 - do.call(rbind, x$getParameterValue("cdf"))
        rownames(surv) = NULL
        colnames(surv) = time1
        surv
      }
    },
    .distrify_survarray = function(x) {
      if (inherits(x, "array") && nrow(x) > 0L) { # can be matrix as well
        # create Matdist or Arrdist (default => median curve)
        distr6::as.Distribution(1 - x, fun = "cdf",
          decorators = c("CoreStatistics", "ExoticStatistics"))
      } else {
        NULL
      }
    }
  )
)

#' @export
as.data.table.PredictionSurv = function(x, ...) {
  tab = as.data.table(x$data[c("row_ids", "crank", "lp", "response")])
  tab$time = x$data$truth[, 1L]
  tab$status = as.logical(x$data$truth[, 2L])

  n_obs = length(x$row_ids)
  if ("distr" %in% x$predict_types && n_obs > 0) {
    distr = x$data$distr
    # get survival matrix/array
    surv = if (inherits(distr, "Distribution"))
      1 - distr6::gprm(distr, "cdf")
    else distr

    # If survival array, take the median
    surv_mat = if (length(dim(surv)) == 3L) .ext_surv_mat(surv, 0.5) else surv

    # Edge case with 1 observation coming from a `distr6::WeightedDisc`
    if (is.vector(surv_mat)) {
      surv_mat = matrix(surv_mat, nrow = 1, dimnames = list(NULL, names(surv_mat)))
    }

    # split survival matrix to 1 vector (curve) per observation
    # wrapped in a list for nice printing
    tab$distr = lapply(1:n_obs, function(i) {
      list(surv_mat[i, , drop = TRUE])
    })

    # strange edge issue with 1 row `data.table` => extra list is removed!
    # so we put them back in:
    if (nrow(tab) == 1) {
      tab$distr = list(list(tab$distr))
    }
  }

  setcolorder(tab, c("row_ids", "time", "status"))[]
}

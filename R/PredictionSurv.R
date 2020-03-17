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
#' task = tgen("simsurv")$generate(20)
#' learner = mlr_learners$get("surv.rpart")
#' p = learner$train(task)$predict(task)
#' head(as.data.table(p))
PredictionSurv = R6Class("PredictionSurv", inherit = Prediction,
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
    #' @param truth (`numeric()`)\cr
    #'   True (observed) response.
    #'
    #' @param crank (`numeric()`)\cr
    #'   Numeric vector of predicted continuous rankings (or relative risks). One element for each
    #'   observation in the test set. For a pair of continuous ranks, a higher rank indicates that
    #'   the observation is more likely to experience the event.
    #'
    #' @param distr ([VectorDistribution][distr6::VectorDistribution])\cr
    #'   [VectorDistribution][distr6::VectorDistribution] from \CRANpkg{distr6}.
    #'   Each individual distribution in the vector represents the random variable 'survival time' for
    #'   an individual observation.
    #'
    #' @param lp (`numeric()`)\cr
    #'   Numeric vector of linear predictor scores. One element for each observation in the test set.
    #'   \eqn{lp = X\beta} where \eqn{X} is a matrix of covariates and \eqn{\beta} is a vector of
    #'   estimated coefficients.
    #'
    #' @param response (`numeric()`)\cr
    #'   Numeric vector of predicted survival times.
    #'   One element for each observation in the test set.
    initialize = function(task = NULL, row_ids = task$row_ids, truth = task$truth(), crank = NULL,
                          distr = NULL, lp = NULL, response = NULL) {
      assert_row_ids(row_ids)
      n = length(row_ids)

      self$task_type = "surv"
      private$.censtype = task$censtype

      # Check returned predict types have correct names and add to data.table
      self$predict_types = c("crank","distr","lp","response")[c(!is.null(crank),!is.null(distr),!is.null(lp),!is.null(response))]
      self$data$tab = data.table(
        row_id = row_ids
      )
      if (!is.null(truth)) {
        assert_surv(truth)
        self$data$tab[, c("time", "status") := list(truth[, 1L], as.logical(truth[, 2L]))]
      }

      if (!is.null(crank)) {
        self$data$tab$crank = assert_numeric(crank, len = n, any.missing = FALSE)
      }

      if (!is.null(distr)) {
        self$data$tab$distr = rep(list(assert_class(distr, "VectorDistribution")), n)
      }

      if (!is.null(lp)) {
        self$data$tab$lp = assert_numeric(lp, len = n, any.missing = FALSE)
      }

      if (!is.null(response)) {
        self$data$tab$response = assert_numeric(response, len = n, any.missing = FALSE)
      }

    }
  ),

  active = list(
    #' @field truth (`Surv`)\cr
    #'   True (observed) outcome.
    truth = function() {
      Surv(self$data$tab$time, self$data$tab$status, type = private$.censtype)
    },

    #' @field crank (`numeric()`)\cr
    #' Access the stored predicted continuous ranking.
    crank = function() {
      self$data$tab$crank %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field distr ([VectorDistribution][distr6::VectorDistribution])\cr
    #' Access the stored predicted survival distribution.
    distr = function() {
      self$data$tab$distr[[1]]
    },

    #' @field lp (`numeric()`)\cr
    #' Access the stored predicted linear predictor.
    lp = function() {
      self$data$tab$lp %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field response (`numeric()`)\cr
    #' Access the stored predicted survival time.
    response = function() {
      self$data$tab$response %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field missing (`integer()`)\cr
    #'   Returns `row_ids` for which the predictions are missing or incomplete.
    missing = function() {
      miss = logical(nrow(self$data$tab))

      if ("crank" %in% self$predict_types) {
        miss = is.na(self$data$tab$crank)
      }

      if ("distr" %in% self$predict_types) {
        miss = miss | is.na(self$data$tab$distr)
      }

      if ("lp" %in% self$predict_types) {
        miss = miss | is.na(self$data$tab$lp)
      }

      if ("response" %in% self$predict_types) {
        miss = miss | is.na(self$data$tab$response)
      }

      self$data$tab$row_id[miss]
    }
  ),

  private = list(
    .censtype = NULL
  )
)


#' @export
as.data.table.PredictionSurv = function(x, ...) {
  copy(x$data$tab)
}

#' @export
c.PredictionSurv = function(..., keep_duplicates = TRUE) {
  dots = list(...)
  assert_list(dots, "PredictionSurv")
  assert_flag(keep_duplicates)
  if (length(dots) == 1L) {
    return(dots[[1L]])
  }

  predict_types = map(dots, "predict_types")
  if (!every(predict_types[-1L], setequal, y = predict_types[[1L]]))
    stopf("Cannot rbind predictions: Different predict_types in objects.")

  if (any(grepl("distr", predict_types))) {
    tab = map_dtr(dots, function(p) subset(p$data$tab, select = -distr), .fill = FALSE)
    distr = do.call(c, lapply(dots, function(p) p$distr))
  } else {
    tab = map_dtr(dots, function(p) subset(p$data$tab), .fill = FALSE)
    distr = NULL
  }

  if (!keep_duplicates)
    tab = unique(tab, by = "row_id", fromLast = TRUE)

  PredictionSurv$new(row_ids = tab$row_id, truth = Surv(tab$time, tab$status), crank = tab$crank,
                     distr = distr, lp = tab$lp, response = tab$response)
}



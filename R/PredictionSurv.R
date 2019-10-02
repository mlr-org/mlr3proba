#' @title Prediction Object for Survival
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3::Prediction].
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerSurv].
#'
#' The `task_type` is set to `"surv"`.
#'
#' @section Construction:
#' ```
#' p = PredictionSurv$new(task = NULL, row_ids = task$row_ids, truth = task$truth(), distr = NULL,
#' risk = NULL, lp = NULL)
#' ```
#'
#' * `task` :: [TaskSurv]\cr
#'   Task, used to extract defaults for `row_ids` and `truth`.
#'
#' * `row_ids` :: (`integer()` | `character()`)\cr
#'   Row ids of the task. Per default, these are extracted from the `task`.
#'
#' * `truth` :: `survival::Surv()`\cr
#'   Observed survival times. Per default, these are extracted from the `task`.
#'
#' * `risk` :: `numeric()`\cr
#'   Vector of risk scores. One element for each observation in the test set.
#'   The higher the risk, the more likely is an event.
#'   Used in measures like [mlr_measures_surv.harrells_c].
#'
#' * `lp` :: `numeric()`\cr
#'   Vector of linear predictor scores. One element for each observation in the test set.
#'   lp = exp(risk).
#'   Used in measures like [mlr_measures_surv.harrells_c].
#'
#' * `distr` :: `distr6::Distribution()`\cr
#'   List of R6 distributions. One distribution for each observation in the test set.
#'   Each distribution contains the hazard, survival, and cumulative hazard (and other
#'   common functions) for all predictions.
#'   Used in measures like [mlr_measures_surv.brier].
#'
#' @section Fields:
#' See [mlr3::Prediction].
#'
#' The field `task_type` is set to `"surv"`.
#'
#' @family Prediction
#' @export
#' @examples
#' library(mlr3)
#' task = mlr_tasks$get("lung")
#' learner = mlr_learners$get("surv.rpart")
#' p = learner$train(task)$predict(task)
#' head(as.data.table(p))
PredictionSurv = R6Class("PredictionSurv", inherit = Prediction,
  public = list(
    initialize = function(task = NULL, row_ids = task$row_ids, truth = task$truth(), distr = NULL, risk = NULL, lp = NULL) {
      assert_row_ids(row_ids)
      n = length(row_ids)

      self$task_type = "surv"
      self$predict_types = c("distr","risk","lp")[c(!is.null(distr),!is.null(risk),!is.null(lp))]
      self$data$tab = data.table(
        row_id = row_ids
      )
      if (!is.null(truth)) {
        assert_surv(truth)
        self$data$tab[, c("time", "status") := list(truth[, 1L], as.logical(truth[, 2L]))]
      }

      if (!is.null(distr)) {
        self$data$tab$distr = distr6::assertDistributionList(distr)
      }

      if (!is.null(risk)) {
        self$data$tab$risk = assert_numeric(risk, len = n, any.missing = FALSE)
      }

      if (!is.null(lp)) {
        self$data$tab$lp = assert_numeric(lp, len = n, any.missing = FALSE)
      }

    },

    print = function(){
      x = as.data.table(self)
      x$distr = lapply(x$distr, distr6::strprint)
      print(x)
    }
  ),

  active = list(
    truth = function() {
      Surv(self$data$tab$time, self$data$tab$status, type = "right")
    },

    distr = function() {
      self$data$tab$distr %??% rep(NA_real_, length(self$data$row_ids))
    },

    risk = function() {
      self$data$tab$risk %??% rep(NA_real_, length(self$data$row_ids))
    },

    lp = function() {
      self$data$tab$lp %??% rep(NA_real_, length(self$data$row_ids))
    },

    missing = function() {
      miss = logical(nrow(self$data$tab))

      if ("distr" %in% self$predict_types) {
        miss = is.na(self$data$tab$distr)
      }

      if ("risk" %in% self$predict_types) {
        miss = miss | is.na(self$data$tab$risk)
      }

      if ("lp" %in% self$predict_types) {
        miss = miss | is.na(self$data$tab$lp)
      }

      self$data$tab$row_id[miss]
    }
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
  if (!every(predict_types[-1L], setequal, y = predict_types[[1L]])) {
    stopf("Cannot rbind predictions: Probabilities for some predictions, not all")
  }

  tab = map_dtr(dots, function(p) p$data$tab, .fill = FALSE)

  if (!keep_duplicates) {
    tab = unique(tab, by = "row_id", fromLast = TRUE)
  }

  PredictionSurv$new(row_ids = tab$row_id, truth = Surv(tab$time, tab$status), distr = tab$distr, risk = tab$risk, lp = tab$lp)
}



#' @title Prediction Object for Density
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerDens].
#'
#' The `task_type` is set to `"dens"`.
#'
#' @family Prediction
#' @export
#' @examples
#' library(mlr3)
#' task = mlr_tasks$get("precip")
#' learner = mlr_learners$get("dens.hist")
#' p = learner$train(task)$predict(task)
#' head(as.data.table(p))
PredictionDens = R6Class("PredictionDens",
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
    #' @param truth (`numeric()`)\cr
    #'   True (observed) response.
    #'
    #' @param pdf (`numeric()`)\cr
    #'   Numeric vector of estimated probability density function, evaluated at 'target' column of
    #'   test set. One element for each observation in the test set.
    #'
    #' @param cdf (`numeric()`)\cr
    #'   Numeric vector of estimated cumulative distribution function, evaluated at 'target' column
    #'   of test set. One element for each observation in the test set.
    initialize = function(task = NULL, row_ids = task$row_ids, truth = task$truth(), pdf = NULL,
                          cdf = NULL) {

      assert_row_ids(row_ids)
      n = length(row_ids)

      self$task_type = "dens"

      # Check returned predict types have correct names and add to data.table
      self$predict_types = c("pdf", "cdf")[c(!is.null(pdf), !is.null(cdf))]
      self$data$tab = data.table(
        row_id = row_ids,
        truth = assert_numeric(truth, len = n, null.ok = TRUE)
      )

      if (!is.null(pdf)) {
        self$data$tab$pdf = assert_numeric(pdf, len = n, any.missing = FALSE)
      }

      if (!is.null(cdf)) {
        self$data$tab$cdf = assert_numeric(cdf, len = n, any.missing = FALSE)
      }
    }
  ),

  active = list(
    #' @field pdf (`numeric()`)\cr
    #' Access the stored predicted probability density function.
    pdf = function() {
      self$data$tab$pdf %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field cdf (`numeric()`)\cr
    #' Access the stored predicted cumulative distribution function.
    cdf = function() {
      self$data$tab$cdf %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field missing (`integer()`)\cr
    #' Returns `row_ids` for which the predictions are missing or incomplete.
    missing = function() {
      miss = logical(nrow(self$data$tab))

      if ("pdf" %in% self$predict_types) {
        miss = is.na(self$data$tab$pdf)
      }

      if ("cdf" %in% self$predict_types) {
        miss = miss | is.na(self$data$tab$cdf)
      }

      self$data$tab$row_id[miss]
    }
  )
)


#' @export
as.data.table.PredictionDens = function(x, ...) { # nolint
  copy(x$data$tab)
}

#' @export
c.PredictionDens = function(..., keep_duplicates = TRUE) {

  dots = list(...)
  assert_list(dots, "PredictionDens")
  assert_flag(keep_duplicates)
  if (length(dots) == 1L) {
    return(dots[[1L]])
  }

  predict_types = map(dots, "predict_types")
  if (!every(predict_types[-1L], setequal, y = predict_types[[1L]])) {
    stopf("Cannot rbind predictions: Different predict_types.")
  }

  tab = map_dtr(dots, function(p) p$data$tab, .fill = FALSE)

  if (!keep_duplicates) {
    tab = unique(tab, by = "row_id", fromLast = TRUE)
  }

  PredictionDens$new(row_ids = tab$row_id, truth = tab$truth, pdf = tab$pdf, cdf = tab$cdf)
}

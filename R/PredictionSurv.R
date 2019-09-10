PredictionSurv = R6Class("PredictionSurv", inherit = Prediction,
  public = list(
    initialize = function(task = NULL, row_ids = task$row_ids, truth = task$truth(),
                          distribution = NULL) {
      self$data$row_ids = assert_atomic_vector(row_ids)
      self$data$truth = truth # assert_surv(truth)
      self$data$distribution = distr6::assertDistributionList(distribution)
      self$task_type = "surv"
    }
  ),

  active = list(
    distribution = function() self$data$distribution,
    missing = function() {
      if (is.null(self$data$distribution)) {
        return(self$data_row_ids[0L])
      }
      self$data$row_ids[is.na(self$data$distribution)]
    }
  )
)


#' @export
as.data.table.PredictionSurv = function(x, ...) {
  tab = data.table(row_id = x$data$row_ids, distribution = x$data$distribution)
  if (!is.null(x$data$truth)) {
    tab[, c("time", "status") := list(x$data$truth[, 1L], x$data$truth[, 2L])]
    setcolorder(tab, c("row_id", "time", "status"))[]
  }
  tab
}

#' @export
c.PredictionSurv = function(..., keep_duplicates = TRUE) {

  dots = list(...)
  assert_list(dots, "PredictionSurv")
  assert_flag(keep_duplicates)

  x = map_dtr(dots, function(p) {
    list(row_ids = p$data$row_ids, risk = p$data$risk)
  }, .fill = FALSE)
  truth = do.call(c, map(dots, "truth"))

  if (!keep_duplicates) {
    keep = !duplicated(x$row_ids, fromLast = TRUE)
    x = x[keep]
    truth = truth[keep]
  }

  PredictionSurv$new(row_ids = x$row_ids, truth = truth, distribution = x$distribution)
}



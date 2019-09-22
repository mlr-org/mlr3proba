PredictionSurv = R6Class("PredictionSurv", inherit = Prediction,
  public = list(
    initialize = function(task = NULL, row_ids = task$row_ids, truth = task$truth(),
                          distr = NULL, risk = NULL) {
      self$data$row_ids = assert_atomic_vector(row_ids)
      self$data$truth = truth # assert_surv(truth)
      self$data$distr = distr6::assertDistributionList(distr)
      self$data$risk = assert_numeric(risk, null.ok = TRUE)
      self$task_type = "surv"
    }
  ),

  active = list(
    risk = function() self$data$risk,
    distr = function() self$data$distr,
    missing = function() {
      if (is.null(self$data$distr)) {
        return(self$data_row_ids[0L])
      }
      self$data$row_ids[is.na(self$data$distr)]
    }
  )
)


#' @export
as.data.table.PredictionSurv = function(x, ...) {
  tab = data.table(row_id = x$data$row_ids, distr = x$data$distr)
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

  PredictionSurv$new(row_ids = x$row_ids, truth = truth, distr = x$distr)
}



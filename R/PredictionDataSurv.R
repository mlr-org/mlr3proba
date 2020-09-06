#' @export
as_prediction.PredictionDataSurv = function(x, ...) { # nolint
  invoke(PredictionSurv$new, .args = x)
}

#' @export
check_prediction_data.PredictionDataSurv = function(pdata) { # nolint
  row_ids = assert_row_ids(pdata$row_id)
  n = length(row_ids)
  assert_class(pdata$truth, "Surv")

  assert_numeric(pdata$response, len = n, any.missing = FALSE, null.ok = TRUE)
  assert_numeric(pdata$crank, len = n, any.missing = FALSE, null.ok = TRUE)
  assert_numeric(pdata$lp, len = n, any.missing = FALSE, null.ok = TRUE)

  if (!is.null(pdata$distr)) {
    assert_class(pdata$distr, "VectorDistribution")
    if (is.null(pdata$crank)) {
      pdata$crank = unname(pdata$distr$mean())
    }
  }

  pdata
}

#' @export
is_missing_prediction_data.PredictionDataSurv = function(pdata) { # nolint
  miss = logical(length(pdata$row_id))

  if (!is.null(pdata$response)) {
    miss = miss | is.na(self$data$tab$response)
  }

  if (!is.null(pdata$cran)) {
    miss = is.na(pdata$crank)
  }

  if (!is.null(pdata$lp)) {
    miss = miss | is.na(pdata$lp)
  }

  pdata$row_id[miss]
}

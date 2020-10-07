#' @export
as_prediction.PredictionDataDens = function(x, check = TRUE) { # nolint
  invoke(PredictionDens$new, check = check, .args = x)
}

#' @export
check_prediction_data.PredictionDataDens = function(pdata) { # nolint
  row_ids = assert_row_ids(pdata$row_ids)
  n = length(row_ids)

  assert_numeric(pdata$pdf, len = n, any.missing = FALSE, null.ok = TRUE)
  assert_numeric(pdata$cdf, len = n, any.missing = FALSE, null.ok = TRUE)

  pdata
}

#' @export
is_missing_prediction_data.PredictionDataDens = function(pdata) { # nolint
  miss = logical(length(pdata$row_ids))

  if (!is.null(pdata$pdf)) {
    miss = is.na(pdata$pdf)
  }

  if (!is.null(pdata$cdf)) {
    miss = miss | is.na(pdata$cdf)
  }

  pdata$row_ids[miss]
}


#' @export
c.PredictionDataDens = function(..., keep_duplicates = TRUE) { # nolint
  dots = list(...)
  assert_list(dots, "PredictionDataDens")
  assert_flag(keep_duplicates)
  if (length(dots) == 1L) {
    return(dots[[1L]])
  }

  predict_types = names(mlr_reflections$learner_predict_types$dens)
  predict_types = map(dots, function(x) intersect(names(x), predict_types))
  if (!every(predict_types[-1L], setequal, y = predict_types[[1L]])) {
    stopf("Cannot combine predictions: Different predict types")
  }

  elems = c("row_ids", predict_types[[1L]])
  tab = map_dtr(dots, function(x) x[elems], .fill = FALSE)

  if (!keep_duplicates) {
    tab = unique(tab, by = "row_id", fromLast = TRUE)
  }

  set_class(as.list(tab), "PredictionDataDens")
}

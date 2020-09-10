#' @export
as_prediction.PredictionDataSurv = function(x, check = TRUE) { # nolint
  invoke(PredictionSurv$new, check = check, .args = x)
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

  if (!is.null(pdata$crank)) {
    miss = is.na(pdata$crank)
  }

  if (!is.null(pdata$lp)) {
    miss = miss | is.na(pdata$lp)
  }

  if (!is.null(pdata$response)) {
    miss = miss | is.na(pdata$response)
  }

  pdata$row_ids[miss]
}


#' @export
c.PredictionDataSurv = function(..., keep_duplicates = TRUE) {
  dots = list(...)
  assert_list(dots, "PredictionDataSurv")
  assert_flag(keep_duplicates)
  if (length(dots) == 1L) {
    return(dots[[1L]])
  }

  predict_types = names(mlr_reflections$learner_predict_types$surv)
  predict_types = map(dots, function(x) intersect(names(x), predict_types))
  if (!every(predict_types[-1L], setequal, y = predict_types[[1L]])) {
    stopf("Cannot combine predictions: Different predict types")
  }

  predict_types = predict_types[[1L]]
  row_ids = do.call(c, map(dots, "row_ids"))
  ii = if (keep_duplicates) seq_along(row_ids) else which(!duplicated(row_ids, fromLast = TRUE))

  elems = c("truth", intersect(c("crank", "lp", "response"), predict_types))
  result = named_list(elems)
  result$row_ids = row_ids[ii]
  for (elem in elems) {
    result[[elem]] = do.call(c, map(dots, elem))[ii]
  }

  if ("distr" %in% predict_types) {
    result$distr = do.call(c, map(dots, "distr"))
  }

  set_class(result, "PredictionDataSurv")
}

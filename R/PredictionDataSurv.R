#' @export
as_prediction.PredictionDataSurv = function(x, check = TRUE, ...) { # nolint
  invoke(PredictionSurv$new, check = check, .args = x)
}


#' @export
check_prediction_data.PredictionDataSurv = function(pdata) { # nolint

  n = length(assert_row_ids(pdata$row_id))
  assert_surv(pdata$truth, "Surv", len = n, any.missing = TRUE, null.ok = TRUE)
  assert_numeric(pdata$crank, len = n, any.missing = FALSE, null.ok = FALSE)
  assert_numeric(pdata$response, len = n, any.missing = FALSE, null.ok = TRUE)
  assert_numeric(pdata$lp, len = n, any.missing = FALSE, null.ok = TRUE)
  if (inherits(pdata$distr, "VectorDistribution")) {
    assert(nrow(pdata$distr$modelTable) == n)
  } else {
    assert_matrix(pdata$distr, nrows = n, any.missing = FALSE, null.ok = TRUE)
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
    if (inherits(dots$distr, "VectorDistribution")) {
      result$distr = do.call(c, map(dots, "distr"))
    } else {
      result$distr = tryCatch(
        do.call(rbind, map(dots, "distr")),
        error = function(e) {
          do.call(c, map(dots,
                         function(x) as.Distribution(1 - x$distr, "cdf",
                                                     decorators = c("CoreStatistics",
                                                                    "ExoticStatistics"))))
        })
    }
  }

  set_class(result, "PredictionDataSurv")
}

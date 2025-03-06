# Here we define some mlr3-mandatory S3 methods of the `PredictionDataSurv` object

#' @export
as_prediction.PredictionDataCompRisks = function(x, check = TRUE, ...) {
  invoke(PredictionCompRisks$new, check = check, .args = x)
}

#' @export
check_prediction_data.PredictionDataCompRisks = function(pdata, ...) {
  n_obs = length(assert_row_ids(pdata$row_ids))
  assert_surv(pdata$truth, "Surv", len = n_obs, any.missing = TRUE, null.ok = TRUE)
  n_cmp_events = length(attr(pdata$truth, "states"))
  assert_cif_list(pdata$cif, n_obs, n_cmp_events)

  pdata
}

#' @export
is_missing_prediction_data.PredictionDataCompRisks = function(pdata, ...) {
  miss = logical(length(pdata$row_ids))

  # no missing values allowed in CIF list of matrices, see `assert_cif_list()`
  pdata$row_ids[miss]
}

#' @export
c.PredictionDataCompRisks = function(..., keep_duplicates = TRUE) {
  dots = list(...)
  assert_list(dots, "PredictionDataCompRisks")
  assert_flag(keep_duplicates)
  if (length(dots) == 1L) {
    return(dots[[1L]])
  }
  browser()

  predict_types = names(mlr_reflections$learner_predict_types$cmprsk)
  predict_types = map(dots, function(x) intersect(names(x), predict_types))
  if (!every(predict_types[-1L], setequal, y = predict_types[[1L]])) {
    stopf("Cannot combine predictions: Different predict types")
  }

  predict_types = predict_types[[1L]]
  row_ids = do.call(c, map(dots, "row_ids"))
  ii = if (keep_duplicates) seq_along(row_ids) else which(!duplicated(row_ids, fromLast = TRUE))

  # combine `truth` and `row_ids`
  elems = c("truth", intersect("cif", predict_types))
  result = named_list(elems)
  result$row_ids = row_ids[ii]
  for (elem in elems) {
    result[[elem]] = do.call(c, map(dots, elem))[ii]
  }

  browser()
  if ("cif" %in% predict_types) {
    cif_list = map(dots, "cif")
    #merge_cols = getFromNamespace(".merge_cols", ns = "distr6")
    #merged_array = merge_cols(distr_list, "surv")
  }

  set_class(result, "PredictionDataCompRisks")
}

#' @export
filter_prediction_data.PredictionDataCompRisks = function(pdata, row_ids, ...) {
  keep = pdata$row_ids %in% row_ids
  pdata$row_ids = pdata$row_ids[keep]
  pdata$truth = pdata$truth[keep]

  if (!is.null(pdata$cif)) {
    # simply keep the observations (rows) in each CIF matrix
    pdata$cif = lapply(pdata$cif, function(mat) mat[keep, , drop = FALSE])
  }

  pdata
}

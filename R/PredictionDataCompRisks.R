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
  assert_list(dots, types = "PredictionDataCompRisks")
  assert_flag(keep_duplicates)
  if (length(dots) == 1L) {
    return(dots[[1L]])
  }

  predict_types = names(mlr_reflections$learner_predict_types$cmprsk)
  predict_types = map(dots, function(x) intersect(names(x), predict_types))
  if (!every(predict_types[-1L], setequal, y = predict_types[[1L]])) {
    stopf("Cannot combine predictions: Different predict types")
  }

  predict_types = predict_types[[1L]]
  row_ids = do.call(c, map(dots, "row_ids"))
  # row ids to keep (default => all)
  ii = if (keep_duplicates) seq_along(row_ids) else which(!duplicated(row_ids, fromLast = TRUE))

  # combine `truth` and `row_ids` (easy to combine via `c`)
  elems = c("row_ids", "truth")
  result = named_list(elems)
  result$row_ids = row_ids[ii]
  for (elem in elems) {
    result[[elem]] = do.call(c, map(dots, elem))[ii]
  }

  if ("cif" %in% predict_types) {
    # Extract CIF lists
    cif_lists = map(dots, "cif")

    # Check that all CIF lists have the same number of competing risks
    # Note: we assume that the causes are in the same order, eg "1", "2", etc.
    # so just checking for their number is enough
    n_cmp_events = unique(sapply(cif_lists, length))
    if (length(n_cmp_events) != 1) {
      stop("Error: Can't combine CIFs with different numbers of competing events")
    }

    # Merge a list of CIF matrices (function from `distr6`)
    # We use this as the time points may be different between predicted CIFs
    # for a particular competing event (cause) and this function uses
    # constant interpolation to "fill in" the CIF values for the union of time points
    merge_cols = getFromNamespace(".merge_cols", ns = "distr6")

    # Check time points for each cause and merge accordingly
    merged_cifs = vector("list", n_cmp_events)
    for (cause_idx in seq_len(n_cmp_events)) {
      cause_cifs = lapply(cif_lists, function(cif) cif[[cause_idx]])
      # CIF is like a CDF, so this function works as it should (constant interpolation)
      merged_cifs[[cause_idx]] = do.call(rbind, merge_cols(cause_cifs, "cdf"))[ii, ]
    }
    # add the causes names
    names(merged_cifs) = names(cif_lists[[1]])
    result$cif = merged_cifs
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

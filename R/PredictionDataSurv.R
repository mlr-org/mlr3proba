# Here we define some mlr3-mandatory S3 methods of the `PredictionDataSurv` object

#' @export
as_prediction.PredictionDataSurv = function(x, check = TRUE, ...) {
  invoke(PredictionSurv$new, check = check, .args = x)
}

#' @export
check_prediction_data.PredictionDataSurv = function(pdata, ...) {
  n = length(assert_row_ids(pdata$row_ids))
  assert_surv(pdata$truth, "Surv", len = n, any.missing = TRUE, null.ok = TRUE)
  assert_numeric(pdata$crank, len = n, any.missing = FALSE, null.ok = FALSE)
  assert_numeric(pdata$response, len = n, any.missing = FALSE, null.ok = TRUE)
  assert_numeric(pdata$lp, len = n, any.missing = FALSE, null.ok = TRUE)
  if (inherits(pdata$distr, "VectorDistribution")) {
    assert(nrow(pdata$distr$modelTable) == n)
  } else if (inherits(pdata$distr, c("Matdist", "Arrdist"))) {
    assert(nrow(gprm(pdata$distr, "pdf")) == n)
  } else if (class(pdata$distr)[1L] == "array") { # from Arrdist
    assert_array(pdata$distr, d = 3, any.missing = FALSE, null.ok = TRUE)
  } else {
    assert_matrix(pdata$distr, nrows = n, any.missing = FALSE, null.ok = TRUE)
  }
  pdata
}

#' @export
is_missing_prediction_data.PredictionDataSurv = function(pdata, ...) { # nolint
  miss = logical(length(pdata$row_ids))

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
    distr_list = map(dots, "distr")
    test_dist = unique(map_lgl(distr_list, testDistribution))

    # Mix of distributions and arrays? Convert arrays to distributions!
    if (length(test_dist) == 2L) {
      distr_list = map(distr_list, function(.x) {
        if (testDistribution(.x)) {
          .x
        } else {
          as.Distribution(1 - .x, fun = "cdf",
            decorators = c("CoreStatistics", "ExoticStatistics"))
        }
      })
      test_dist = TRUE
    }

    # All distributions? Concatenate!
    if (test_dist) {
      result$distr = do.call(c, c(distr_list,
        list(decorators = c("CoreStatistics", "ExoticStatistics"))))
    } else {
      dims = map_int(distr_list, function(.x) length(dim(.x)))
      # If mix of arrays and matrices, convert arrays to median survival matrices
      if (length(unique(dims)) > 1L) {
        distr_list = lapply(distr_list, function(.x) {
          if (length(dim(.x)) == 3L) {
            .ext_surv_mat(.x, which.curve = 0.5)
          } else {
            .x
          }
        })
      }
      # All objects are now either 3d arrays or 2d matrices
      # row-bind arrays and ensure all have same column names
      # by automatically converting to pdf then back to surv
      merge_cols = getFromNamespace(".merge_cols", ns = "distr6")
      merged_array = merge_cols(distr_list, "surv")
      # abind works with matrices as well
      result$distr = abind::abind(merged_array, along = 1, force.array = FALSE)
    }
  }

  set_class(result, "PredictionDataSurv")
}

#' @export
filter_prediction_data.PredictionDataSurv = function(pdata, row_ids, ...) {
  keep = pdata$row_ids %in% row_ids
  pdata$row_ids = pdata$row_ids[keep]
  pdata$truth = pdata$truth[keep]

  if (!is.null(pdata$crank)) {
    pdata$crank = pdata$crank[keep]
  }

  if (!is.null(pdata$lp)) {
    pdata$lp = pdata$lp[keep]
  }

  if (!is.null(pdata$distr)) {
    distr = pdata$distr

    if (testDistribution(distr)) { # distribution
      ok = inherits(distr, c("VectorDistribution", "Matdist", "Arrdist")) &&
        length(keep) > 1L # e.g.: Arrdist(1xYxZ) and keep = FALSE
      if (ok) {
        pdata$distr = distr[keep] # we can subset row/samples like this
      } else {
        pdata$distr = base::switch(keep, distr) # one distribution only
      }
    } else {
      if (length(dim(distr)) == 2L) { # 2d matrix
        pdata$distr = distr[keep, , drop = FALSE]
      } else { # 3d array
        pdata$distr = distr[keep, , , drop = FALSE]
      }
    }
  }

  pdata
}

#' @export
as_prediction.PredictionDataSurv = function(x, check = TRUE, ...) { # nolint
  invoke(PredictionSurv$new, check = check, .args = x)
}


#' @export
check_prediction_data.PredictionDataSurv = function(pdata, ...) { # nolint

  n = length(assert_row_ids(pdata$row_ids))
  assert_surv(pdata$truth, "Surv", len = n, any.missing = TRUE, null.ok = TRUE)
  assert_numeric(pdata$crank, len = n, any.missing = FALSE, null.ok = FALSE)
  assert_numeric(pdata$response, len = n, any.missing = FALSE, null.ok = TRUE)
  assert_numeric(pdata$lp, len = n, any.missing = FALSE, null.ok = TRUE)
  if (inherits(pdata$distr, "VectorDistribution")) {
    assert(nrow(pdata$distr$modelTable) == n)
  } else if (inherits(pdata$distr, c("Matdist", "Arrdist"))) {
    assert(nrow(gprm(pdata$distr, "pdf")) == n)
  } else if (class(pdata$distr)[1] == "array") { # from Arrdist
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
    classes = sapply(distr_list, function(d) { class(d)[1] })
    distr6_classes = c("Matdist", "VectorDistribution", "Arrdist")
    data_classes = c("matrix", "array")

    # all distr predictions has the same class (most frequent scenario)
    if (length(unique(classes)) == 1) {
      if (classes[1] %in% distr6_classes) {
        # 1st case: distr6 objects
        result$distr = do.call(c, distr_list)
      } else if (classes[1] %in% data_classes) {
        # 2nd case: survival matrices (or arrays)
        # Can only be combined if:
        # 1) number and names of columns (time points) are the same
        # 2) in case of arrays, the size of the third dimension is also the same
        # TODO(?): use code from distr6 to make matrices and arrays with same
        # number of columns and fill in the survival probabilities inside
        ncols = sapply(distr_list, ncol)
        same_ncols = length(unique(ncols)) == 1

        same_colnames = FALSE
        if (same_ncols) {
          colnames_mat  = sapply(distr_list, colnames)
          same_colnames = all(colnames_mat[,1] == colnames_mat)
        }
        same_dim3 = TRUE # in case of matrices this is always true
        if (classes[1] == "array") {
          dim3_sizes = sapply(distr_list, function(x) dim(x)[[3L]])
          same_dim3 = length(unique(dim3_sizes)) == 1
        }

        if (same_ncols && same_colnames && same_dim3) {
          result$distr = abind::abind(distr_list, along = 1, force.array = FALSE)
        } else {
          # convert internally within distr6 and then combine
          result$distr = do.call(c, map(distr_list,
            function(x) {
              as.Distribution(1 - x, "cdf", decorators = c("CoreStatistics",
                "ExoticStatistics"), vector = TRUE)
            }))
        }
      } else {
        # should never reach this point
        stop("One prediction object to be combined is not a distr6 object,
          survival matrix or array")
      }
    } else {
      # distr predictions are of different classes (rare cases)
      if (all(classes %in% distr6_classes)) {
        stop("Combining different distr6 prediction objects is not implemented")
      } else if (all(classes %in% data_classes)) {
        # TODO: combine survival arrays with matrices somehow?
      } else {
        stop("Combining mix of distr6 and matrix/array prediction classes is not
          implemented")
      }
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
    if (inherits(pdata$distr, "matrix")) {
      pdata$distr = pdata$distr[keep, , drop = FALSE]
    } else { # array
      pdata$distr = pdata$distr[keep, , , drop = FALSE]
    }

  }

  pdata
}

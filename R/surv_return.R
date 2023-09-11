#' @title Get Survival Predict Types
#'
#' @description Internal helper function to easily return the correct survival predict types.
#'
#' @param times (`numeric()`) \cr Vector of survival times.
#' @param surv (`matrix()|array()`)\cr Matrix or array of predicted survival
#' probabilities, rows (1st dimension) are observations, columns (2nd dimension)
#' are times and in the case of an array there should be one more dimension.
#' Number of columns should be equal to length of `times`. In case a `numeric()`
#' vector is provided, it is converted to a single row (one observation) matrix.
#' @param which.curve Which curve (3rd dimension) should the `crank` be
#' calculated for, in case `surv` is an `array`? If between (0,1) it is taken as
#' the quantile of the curves (assumes these are ordered) otherwise if greater
#' than 1 it is taken as the curve index. It can also be 'mean' and the survival
#' probabilities are averaged across the 3rd dimension.
#' @param crank (`numeric()`)\cr Relative risk/continuous ranking. Higher value is associated
#' with higher risk. If `NULL` then either set as `-response` if available or
#' `lp` if available (this assumes that the `lp` prediction comes from a PH type
#' model - in case of an AFT model the user should provide `-lp`).
#' In case neither `response` or `lp` are provided, then `crank` is calculated
#' as the sum of the cumulative hazard function (expected mortality) derived from
#' the predicted survival function (`surv`). In case `surv` is a 3d array, we use
#' the `which.curve` parameter to decide which survival matrix (index in the 3rd
#' dimension) will be chosen for the calculation of `crank`.
#' @param lp (`numeric()`)\cr Predicted linear predictor, used to impute `crank` if `NULL`.
#' @param response (`numeric()`)\cr Predicted survival time, passed through function without
#' modification.
#'
#' @details
#' Uses [survivalmodels::surv_to_risk] to reduce survival matrices to relative
#' risks / rankings if `crank` is NULL.
#'
#' @references
#' Sonabend, R., Bender, A., & Vollmer, S. (2022). Avoiding C-hacking when
#' evaluating survival distribution predictions with discrimination measures.
#' Bioinformatics. https://doi.org/10.1093/BIOINFORMATICS/BTAC451
#' @export
.surv_return = function(times = NULL, surv = NULL, which.curve = NULL,
  crank = NULL, lp = NULL, response = NULL) {

  if (!is.null(surv)) {
    if (class(surv)[1] == "numeric") {
      # in case of a vector (one observation) convert to matrix
      surv = matrix(surv, nrow = 1, dimnames = list(NULL, names(surv)))
    }
    if (class(surv)[1] == "array") {
      if (length(dim(surv)) != 3) {
        stop("3D survival arrays supported only")
      }
    }
    times = times %||% colnames(surv)
    if (length(times) != ncol(surv)) {
      stop("'times' must have the same length as the 2nd dimension (columns of 'surv')")
    }
    colnames(surv) = times
  }

  if (is.null(crank)) {
    if (!is.null(response)) {
      # low survival time = high risk
      # high crank = high risk
      crank = -response
    } else if (!is.null(lp)) {
      # assumes PH-type lp where high value = high risk
      crank = lp
    } else if (!is.null(surv)) {
      if (inherits(surv, "matrix")) {
        crank = survivalmodels::surv_to_risk(surv)
      } else { # array
        surv_mat = .ext_surv_mat(surv, which.curve)
        crank = survivalmodels::surv_to_risk(surv_mat)
      }
    }
  }

  list(
    distr = surv, # matrix or array
    crank = crank,
    lp = lp,
    response = response
  )
}

# helper function to extract a survival matrix from a 3D survival array
.ext_surv_mat = function(arr, which.curve) {
  # if NULL return the 'mean' curve (default)
  if (is.null(which.curve)) {
    return(apply(arr, c(1,2), mean))
  }

  # which.curve must be length 1 and either 'mean' or >0
  ok = (length(which.curve) == 1) &&
    ((is.character(which.curve) && which.curve == "mean") ||
      (is.numeric(which.curve) && which.curve > 0))
  if (!ok) {
    stop("'which.curve' has to be a numeric between (0,1) or the index of the
      3rd dimension or 'mean'")
  }

  if (is.numeric(which.curve) && which.curve > dim(arr)[3L]) {
    stop(sprintf("Length is %s on third dimension but curve '%s' requested,
          change 'which.curve' parameter.", dim(arr)[3L], which.curve))
  }

  # mean
  if (which.curve == "mean") {
    apply(arr, c(1, 2), mean)
  # curve chosen based on quantile
  } else if (which.curve < 1) {
    array(apply(arr, c(1, 2), quantile, which.curve), c(nrow(arr), ncol(arr)),
      dimnames(arr)[c(1, 2)])
  # curve chosen based on index
  } else {
    array(arr[, , which.curve], c(nrow(arr), ncol(arr)), dimnames(arr)[c(1, 2)])
  }
}

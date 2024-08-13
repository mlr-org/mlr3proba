#' @title Get Survival Predict Types
#'
#' @description Internal helper function to easily return the correct survival predict types.
#'
#' @param times (`numeric()`) \cr Vector of survival times.
#' @param surv (`matrix()|array()`)\cr Matrix or array of predicted survival probabilities, rows (1st dimension) are observations, columns (2nd dimension) are times and in the case of an array there should be one more dimension.
#' Number of columns should be equal to length of `times`.
#' In case a `numeric()` vector is provided, it is converted to a single row (one observation) matrix.
#' @param crank (`numeric()`)\cr Relative risk/continuous ranking.
#' Higher value is associated with higher risk.
#' If `NULL` then either set as `-response` if available or `lp` if available (this assumes that the `lp` prediction comes from a PH type model - in case of an AFT model the user should provide `-lp`).
#' In case neither `response` or `lp` are provided, then `crank` is calculated as the sum of the cumulative hazard function (**expected mortality**) derived from the predicted survival function (`surv`), see [get_mortality].
#' In case `surv` is a 3d array, we use the `which.curve` parameter to decide which survival matrix (index in the 3rd dimension) will be chosen for the calculation of `crank`.
#' @param lp (`numeric()`)\cr Predicted linear predictor, used to impute `crank` if `NULL`.
#' @param response (`numeric()`)\cr Predicted survival time, passed through function without
#' modification.
#' @param which.curve Which curve (3rd dimension) should the `crank` be calculated for, in case `surv` is an `array`?
#' If between (0,1) it is taken as the quantile of the curves otherwise if greater than 1 it is taken as the  curve index.
#' It can also be 'mean' and the survival probabilities are averaged across the 3rd dimension.
#' Default value (`NULL`) is the **0.5 quantile** which is the median across the 3rd dimension of the survival array.
#'
#' @references
#' `r format_bib("sonabend_2022")`
#'
#' @export
.surv_return = function(times = NULL, surv = NULL, crank = NULL, lp = NULL,
  response = NULL, which.curve = NULL) {

  if (!is.null(surv)) {
    if (class(surv)[1L] == "numeric") {
      # in case of a vector (one observation) convert to matrix
      surv = matrix(surv, nrow = 1L, dimnames = list(NULL, names(surv)))
    }
    if (class(surv)[1L] == "array" && length(dim(surv)) != 3L) {
      stop("3D survival arrays supported only")
    }
    times = times %??% colnames(surv)
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

  # TODO: pass the 'which.curve' parameter in PredictionSurv
  list(
    distr = surv, # matrix or array
    crank = crank,
    lp = lp,
    response = response
  )
}

# helper function to extract a survival matrix from a 3D survival array
.ext_surv_mat = function(arr, which.curve) {
  # if NULL return the 'median' curve (default)
  if (is.null(which.curve)) {
    return(array(apply(arr, 1:2, stats::quantile, 0.5), c(nrow(arr), ncol(arr)),
      dimnames(arr)[1:2]))
  }

  # which.curve must be length 1 and either 'mean' or >0
  ok = (length(which.curve) == 1L) &&
    ((is.character(which.curve) && which.curve == "mean") ||
      (is.numeric(which.curve) && which.curve > 0))
  if (!ok) {
    stop("'which.curve' has to be a numeric between (0,1) or the index of the
      3rd dimension or 'mean'")
  }

  if (is.numeric(which.curve) && which.curve > dim(arr)[3L]) {
    stopf(
      "Length is %s on third dimension but curve '%s' requested, change 'which.curve' parameter.",
      dim(arr)[3L],
      which.curve
    )
  }

  # mean
  if (which.curve == "mean") {
    apply(arr, 1:2, mean)
    # curve chosen based on quantile
  } else if (which.curve < 1) {
    array(apply(arr, 1:2, stats::quantile, which.curve), c(nrow(arr), ncol(arr)),
      dimnames(arr)[1:2])
    # curve chosen based on index
  } else {
    array(arr[, , which.curve], c(nrow(arr), ncol(arr)), dimnames(arr)[1:2])
  }
}

#' @title Calculate the expected mortality risks from a survival matrix
#'
#' @description Many methods can be used to reduce a discrete survival
#' distribution prediction (i.e. matrix) to a relative risk / ranking
#' prediction, see Sonabend et al. (2022).
#'
#' This function calculates a relative risk score as the sum of the
#' predicted cumulative hazard function, also called **ensemble/expected mortality**.
#' This risk score can be loosely interpreted as the expected number of deaths for
#' patients with similar characteristics, see Ishwaran et al. (2008) and has no
#' model or survival distribution assumptions.
#'
#' @param x (`matrix()`) \cr A survival matrix where rows are the
#' (predicted) observations and columns the time-points.
#' For more details, see [assert_surv_matrix].
#'
#' @return a `numeric` vector of the mortality risk scores, one per row of the
#' input survival matrix.
#'
#' @references
#' `r format_bib("sonabend_2022", "ishwaran_2008")`
#'
#' @examples
#' n = 10 # number of observations
#' k = 50 # time points
#'
#' # Create the matrix with random values between 0 and 1
#' mat = matrix(runif(n * k, min = 0, max = 1), nrow = n, ncol = k)
#'
#' # transform it to a survival matrix
#' surv_mat = t(apply(mat, 1L, function(row) sort(row, decreasing = TRUE)))
#' colnames(surv_mat) = 1:k # time points
#'
#' # get mortality scores (the larger, the more risk)
#' mort = get_mortality(surv_mat)
#' mort
#'
#' @export
get_mortality = function(x) {
  assert_surv_matrix(x)

  # H(t) = -log(S(t))
  cumhaz = -log(x)

  # Ignore S(t) = 0 => -log(S(t)) = Inf
  cumhaz[is.infinite(cumhaz)] = 0

  rowSums(cumhaz)
}

#' @title Get Survival Predict Types
#' @description Internal helper function to easily return the correct survival predict types and to
#' automatically coerce a predicted survival probability matrix to a [distr6::Matdist].
#' @param times (`numeric()`) \cr Vector of survival times.
#' @param surv (`matrix()`)\cr Matrix of predicted survival probabilities, rows are observations,
#' columns are times. Number of columns should be equal to length of `times`.
#' @param crank (`numeric()`)\cr Relative risk/continuous ranking. Higher value is associated
#' with higher risk. If `NULL` then either set as `-response` if available or
#' `lp` if available (this assumes that the `lp` prediction comes from a PH type
#' model - in case of an AFT model the user should provide `-lp`).
#' In case neither `response` or `lp` are provided, then `crank` is calculated
#' as the sum of the cumulative hazard function (expected mortality) derived from
#' the predicted survival function (`surv`).
#' @param lp (`numeric()`)\cr Predicted linear predictor, used to impute `crank` if `NULL`.
#' @param response (`numeric()`)\cr Predicted survival time, passed through function without
#' modification.
#' @details
#' Uses [survivalmodels::surv_to_risk] to reduce survival matrices to relative
#' risks / rankings if `crank` is NULL.
#' @references
#' Sonabend, R., Bender, A., & Vollmer, S. (2022). Avoiding C-hacking when
#' evaluating survival distribution predictions with discrimination measures.
#' Bioinformatics. https://doi.org/10.1093/BIOINFORMATICS/BTAC451
#' @export
.surv_return = function(times = NULL, surv = NULL, crank = NULL, lp = NULL, response = NULL) {

  if (!is.null(surv)) {
    if (class(surv)[1] == "numeric") {
      surv = matrix(surv, nrow = 1)
    }
    times <- times %||% colnames(surv)
    assert(length(times) == ncol(surv))
    colnames(surv) <- times
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
      crank = survivalmodels::surv_to_risk(surv)
    }
  }

  list(
    distr = surv,
    crank = crank,
    lp = lp,
    response = response
  )
}

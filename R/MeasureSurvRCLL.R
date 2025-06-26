#' @template surv_measure
#' @templateVar title Right-Censored Log Loss
#' @templateVar fullname MeasureSurvRCLL
#' @templateVar eps 1e-15
#' @template param_eps
#' @template param_erv
#'
#' @description
#' Calculates the right-censored logarithmic (log), loss.
#'
#' @details
#' The RCLL, in the context of probabilistic predictions, is defined by
#' \deqn{L(f, t, \Delta) = -log(\Delta f(t) + (1 - \Delta) S(t))}
#' where \eqn{\Delta} is the censoring indicator, \eqn{f} the probability
#' density function and \eqn{S} the survival function.
#' RCLL is proper given that censoring and survival distribution are independent, see Rindt et al. (2022).
#'
#' **Note**: Even though RCLL is a proper scoring rule, the calculation of \eqn{f(t)} (which in our case is discrete, i.e. it is a *probability mass function*) for time points in the test set that don't exist in the predicted survival matrix (`distr`), results in 0 values, which are substituted by `"eps"` in our implementation, therefore skewing the result towards \eqn{-log(eps)}.
#' This problem is also discussed in Rindt et al. (2022), where the authors perform interpolation to get non-zero values for the \eqn{f(t)}.
#' Until this is handled in `mlr3proba` some way, we advise against using this measure for model evaluation.
#'
#' @section Parameter details:
#' - `na.rm` (`logical(1)`)\cr
#' If `TRUE` (default) then removes any NAs in individual score calculations.
#'
#' @references
#' `r format_bib("avati_2020", "rindt_2022")`
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvRCLL = R6Class("MeasureSurvRCLL",
  inherit = MeasureSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param ERV (`logical(1)`)\cr
    #'   Standardize measure against a Kaplan-Meier baseline
    #'   (Explained Residual Variation)
    initialize = function(ERV = FALSE) {
      assert_logical(ERV)

      ps = ps(
        eps = p_dbl(0, 1, default = 1e-15),
        ERV = p_lgl(default = FALSE),
        na.rm = p_lgl(default = TRUE)
      )
      ps$set_values(eps = 1e-15, ERV = ERV, na.rm = TRUE)

      range = if (ERV) c(-Inf, 1) else c(0, Inf)

      super$initialize(
        id = "surv.rcll",
        minimize = !ERV,
        predict_type = "distr",
        packages = "distr6",
        label = "Right-Censored Log Loss",
        man = "mlr3proba::mlr_measures_surv.rcll",
        range = range,
        param_set = ps
      )

      invisible(self)
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...) {
      if (self$param_set$values$ERV) {
        return(.scoring_rule_erv(self, prediction, task, train_set))
      }
      out = rep(-99L, length(prediction$row_ids))
      truth = prediction$truth
      event = truth[, 2L] == 1
      event_times = truth[event, 1L]
      cens_times = truth[!event, 1L]

      # Bypass distr6 construction if underlying distr represented by array
      if (inherits(prediction$data$distr, "array")) {
        surv = prediction$data$distr
        if (length(dim(surv)) == 3L) {
          # survival 3d array, extract median
          surv = .ext_surv_mat(arr = surv, which.curve = 0.5)
        }
        times = as.numeric(colnames(surv))

        if (any(!event)) {
          if (sum(!event) == 1) { # fix subsetting issue in case of 1 censored
            cdf = as.matrix(1 - surv[!event, ])
          } else {
            cdf = t(1 - surv[!event, ])
          }

          extend_times_cdf = getFromNamespace("C_Vec_WeightedDiscreteCdf", ns = "distr6")
          out[!event] = diag(
            extend_times_cdf(cens_times, times, cdf = cdf, FALSE, FALSE)
          )
        }
        if (any(event)) {
          convert_to_pdf = getFromNamespace("cdfpdf", ns = "distr6")
          pdf = convert_to_pdf(cdf = 1 - surv)
          if (sum(event) == 1) { # fix subsetting issue in case of 1 event
            pdf = as.matrix(pdf[event, ])
          } else {
            pdf = t(pdf[event, ])
          }

          extend_times_pdf = getFromNamespace("C_Vec_WeightedDiscretePdf", ns = "distr6")
          out[event] = diag(
            extend_times_pdf(event_times, times, pdf = pdf)
          )
        }
      } else {
        distr = prediction$distr

        # Splitting in this way bypasses unnecessary distr extraction
        if (!any(event)) { # all censored
          # survival at outcome time (survived *at least* this long)
          out = diag(as.matrix(distr$survival(cens_times)))
        } else if (all(event)) { # all uncensored
          # pdf at outcome time (survived *this* long)
          out = diag(as.matrix(distr$pdf(event_times)))
        } else { # mix
          out[event] = diag(as.matrix(distr[event]$pdf(event_times)))
          out[!event] = diag(as.matrix(distr[!event]$survival(cens_times)))
        }
      }

      stopifnot(!any(out == -99L)) # safety check
      # prevent infinite log errors
      out[out == 0] = self$param_set$values$eps

      out = -log(out)

      mean(out, na.rm = self$param_set$values$na.rm)
    }
  )
)

register_measure("surv.rcll", MeasureSurvRCLL)

#' @template surv_measure
#' @templateVar title Integrated Brier Score
#' @templateVar fullname MeasureSurvGraf
#' @template param_integrated
#' @template param_times
#' @template param_tmax
#' @template param_pmax
#' @template param_method
#' @template param_se
#' @template param_proper
#' @templateVar eps 1e-3
#' @template param_eps
#' @template param_erv
#'
#' @aliases MeasureSurvBrier mlr_measures_surv.brier
#'
#' @description
#' Calculates the **Integrated Survival Brier Score** (ISBS), Integrated Graf Score
#' or squared survival loss.
#'
#' @details
#' For an individual who dies at time \eqn{t}, with predicted Survival function, \eqn{S}, the
#' Graf Score at time \eqn{t^*}{t*} is given by
#' \deqn{L_{ISBS}(S,t|t^*) = [(S(t^*)^2)I(t \le t^*, \delta = 1)(1/G(t))] + [((1 - S(t^*))^2)I(t > t^*)(1/G(t^*))]}
#' where \eqn{G} is the Kaplan-Meier estimate of the censoring distribution.
#'
#' The re-weighted ISBS (RISBS) is given by
#' \deqn{L_{RISBS}(S,t|t^*) = [(S(t^*)^2)I(t \le t^*, \delta = 1)(1/G(t))] + [((1 - S(t^*))^2)I(t > t^*)(1/G(t))]}
#' where \eqn{G} is the Kaplan-Meier estimate of the censoring distribution, i.e. always
#' weighted by \eqn{G(t)}.
#' RISBS is strictly proper when the censoring distribution is independent
#' of the survival distribution and when G is fit on a sufficiently large dataset.
#' ISBS is never proper. Use `proper = FALSE` for ISBS and `proper = TRUE` for RISBS.
#' Results may be very different if many observations are
#' censored at the last observed time due to division by 1/`eps` in `proper = TRUE`.
#'
#' **Note**: If comparing the integrated graf score to other packages, e.g.
#' \CRANpkg{pec}, then `method = 2` should be used. However the results may
#' still be very slightly different as this package uses `survfit` to estimate
#' the censoring distribution, in line with the Graf 1999 paper; whereas some
#' other packages use `prodlim` with `reverse = TRUE` (meaning Kaplan-Meier is
#' not used).
#'
#' @template details_trainG
#' @template details_tmax
#'
#' @references
#' `r format_bib("graf_1999")`
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvGraf = R6::R6Class("MeasureSurvGraf",
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
        integrated = p_lgl(default = TRUE),
        times = p_uty(),
        t_max = p_dbl(0),
        p_max = p_dbl(0, 1),
        method = p_int(1L, 2L, default = 2L),
        se = p_lgl(default = FALSE),
        proper = p_lgl(default = FALSE),
        eps = p_dbl(0, 1, default = 1e-3),
        ERV = p_lgl(default = FALSE)
      )
      ps$set_values(
        integrated = TRUE, method = 2L, se = FALSE,
        proper = FALSE, eps = 1e-3, ERV = ERV
      )

      range = if (ERV) c(-Inf, 1) else c(0, Inf)

      super$initialize(
        param_set = ps,
        id = "surv.graf",
        range = range,
        minimize = !ERV,
        packages = character(),
        predict_type = "distr",
        label = "Integrated Graf Score",
        man = "mlr3proba::mlr_measures_surv.graf",
      )
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...) {
      ps = self$param_set$values
      # times must be unique, sorted and positive numbers
      times = assert_numeric(ps$times, lower = 0, any.missing = FALSE,
                             unique = TRUE, sorted = TRUE, null.ok = TRUE)
      # ERV score
      if (ps$ERV) return(.scoring_rule_erv(self, prediction, task, train_set))

      nok = sum(!is.null(times), !is.null(ps$t_max), !is.null(ps$p_max)) > 1
      if (nok) {
        stop("Only one of `times`, `t_max`, and `p_max` should be provided")
      }

      if (!ps$integrated) {
        msg = "If `integrated=FALSE` then `times` should be a scalar numeric."
        assert_numeric(times, len = 1L, .var.name = msg)
      } else {
        if (!is.null(times) && length(times) == 1L) {
          ps$integrated = FALSE
        }
      }

      x = as.integer(!is.null(task)) + as.integer(!is.null(train_set))
      if (x == 1) {
        stop("Either 'task' and 'train_set' should be passed to measure or neither.")
      } else if (x) {
        train = task$truth(train_set)
      } else {
        train = NULL
      }

      # `score` is a matrix, IBS(i,j) => n_test_obs x times
      score = weighted_survival_score("graf",
        truth = prediction$truth,
        distribution = prediction$data$distr, times = times,
        t_max = ps$t_max, p_max = ps$p_max, proper = ps$proper, train = train,
        eps = ps$eps
      )

      if (ps$se) {
        integrated_se(score, ps$integrated)
      } else {
        integrated_score(score, ps$integrated, ps$method)
      }
    }
  )
)

register_measure("surv.graf", MeasureSurvGraf)
register_measure("surv.brier", MeasureSurvGraf)

#' @template surv_measure
#' @templateVar title Integrated Log loss
#' @templateVar fullname MeasureSurvIntLogloss
#'
#' @description
#' Calculates the integrated survival logarithmic (log) (ISLL), loss, aka integrated cross entropy.
#'
#' For an individual who dies at time \eqn{t}, with predicted Survival function, \eqn{S}, the
#' probabilistic log loss at time \eqn{t^*}{t*} is given by
#' \deqn{L(S,t|t^*) = - [log(1 - S(t^*))I(t \le t^*, \delta = 1)(1/G(t))] - [log(S(t^*))I(t > t^*)(1/G(t^*))]}{L(S,t|t*) = - [log(1 - S(t*))I(t \le t*, \delta = 1)(1/G(t))] - [log(S(t*))I(t > t*)(1/G(t*))]} # nolint
#' where \eqn{G} is the Kaplan-Meier estimate of the censoring distribution.
#'
#' The re-weighted ISLL, ISLL* is given by
#' \deqn{L(S,t|t^*) = - [log(1 - S(t^*))I(t \le t^*, \delta = 1)(1/G(t))] - [log(S(t^*))I(t > t^*)(1/G(t))]}{L(S,t|t*) = - [log(1 - S(t*))I(t \le t*, \delta = 1)(1/G(t))] - [log(S(t*))I(t > t*)(1/G(t))]} # nolint
#' where \eqn{G} is the Kaplan-Meier estimate of the censoring distribution, i.e. always
#' weighted by \eqn{G(t)}. ISLL* is strictly proper when the censoring distribution is independent
#' of the survival distribution and when G is fit on a sufficiently large dataset. ISLL is never
#' proper. Use `proper = FALSE` for ISLL and `proper = TRUE` for ISLL*, in the future the default
#' will be changed to `proper = TRUE`.
#'
#' @template measure_integrated
#' @template param_integrated
#' @template param_times
#' @template param_eps
#' @template field_eps
#' @template param_method
#' @template param_proper
#' @template param_se
#' @template details_trainG
#'
#' @references
#' `r format_bib("graf_1999")`
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvIntLogloss = R6::R6Class("MeasureSurvIntLogloss",
  inherit = MeasureSurvIntegrated,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, times, eps = 1e-15, method = 2, se = FALSE,
                          proper = FALSE) {
      super$initialize(
        integrated = integrated,
        times = times,
        method = method,
        proper = proper,
        id = ifelse(se, "surv.intlogloss_se", "surv.intlogloss"),
        range = c(0, Inf),
        minimize = TRUE,
        packages = "distr6",
        predict_type = "distr",
        man = "mlr3proba::mlr_measures_surv.intlogloss",
      )

      private$.eps = assertNumeric(eps)
      private$.se = assertFlag(se)
    }
  ),

  active = list(
    eps = function(eps) {
      if (missing(eps)) {
        return(private$.eps)
      } else {
        assertNumeric(eps)
        private$.eps = eps
      }
    },

    #' @field se `(logical(1))` \cr
    #' If `TRUE` returns the standard error of the measure.
    se = function(x) {
      if (!missing(x)) {
        private$.se = assertFlag(x)
      } else {
        return(private$.se)
      }
    }
  ),

  private = list(
    .eps = numeric(0),
    .se = FALSE,
    .score = function(prediction, task, train_set, ...) {

      x = as.integer(!is.null(task)) + as.integer(!is.null(train_set))
      if (x == 1) {
        stop("Either 'task' and 'train_set' should be passed to measure or neither.")
      } else if (x) {
        train = task$truth(train_set)
      } else {
        train = NULL
      }

      if (self$se) {
        return(
          integrated_score(score = weighted_survival_score("intslogloss",
                                                           truth = prediction$truth,
                                                           distribution = prediction$distr,
                                                           times = self$times,
                                                           proper = self$proper,
                                                           train = train,
                                                           eps = self$eps),
                           integrated = self$integrated,
                           method = self$method)
        )
      } else {
        return(
          integrated_se(score = weighted_survival_score("intslogloss",
                                                        truth = prediction$truth,
                                                        distribution = prediction$distr,
                                                        times = self$times,
                                                        proper = self$proper,
                                                        train = train,
                                                        eps = self$eps),
                        integrated = self$integrated)
        )
      }
    }
  )
)

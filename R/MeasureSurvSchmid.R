#' @template surv_measure
#' @templateVar title Integrated Schmid Score
#' @templateVar fullname MeasureSurvSchmid
#'
#' @description
#' Calculates the Integrated Schmid Score (ISS), aka integrated absolute loss.
#'
#' For an individual who dies at time \eqn{t}, with predicted Survival function, \eqn{S}, the
#' Schmid Score at time \eqn{t^*}{t*} is given by
#' \deqn{L(S,t|t^*) = [(S(t^*))I(t \le t^*, \delta = 1)(1/G(t))] + [((1 - S(t^*)))I(t > t^*)(1/G(t^*))]}{L(S,t|t*) = [(S(t*))I(t \le t*, \delta = 1)(1/G(t))] + [((1 - S(t*)))I(t > t*)(1/G(t*))]} # nolint
#' where \eqn{G} is the Kaplan-Meier estimate of the censoring distribution.
#'
#' The re-weighted ISS, ISS* is given by
#' \deqn{L(S,t|t^*) = [(S(t^*))I(t \le t^*, \delta = 1)(1/G(t))] + [((1 - S(t^*)))I(t > t^*)(1/G(t))]}{L(S,t|t*) = [(S(t*))I(t \le t*, \delta = 1)(1/G(t))] + [((1 - S(t*)))I(t > t*)(1/G(t))]} # nolint
#' where \eqn{G} is the Kaplan-Meier estimate of the censoring distribution, i.e. always
#' weighted by \eqn{G(t)}. ISS* is strictly proper when the censoring distribution is independent
#' of the survival distribution and when G is fit on a sufficiently large dataset. ISS is never
#' proper. Use `proper = FALSE` for ISS and `proper = TRUE` for ISS*, in the future the default
#' will be changed to `proper = TRUE`. Results may be very different if many observations are
#' censored at the last observed time due to division by 1/`eps` in `proper = TRUE`.
#'
#' @template measure_integrated
#' @template param_integrated
#' @template param_times
#' @template param_method
#' @template param_proper
#' @template param_se
#' @template param_eps
#' @template details_trainG
#'
#' @references
#' `r format_bib("schemper_2000", "schmid_2011")`
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvSchmid = R6::R6Class("MeasureSurvSchmid",
  inherit = MeasureSurvIntegrated,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, times, method = 2, se = FALSE, proper = FALSE,
      eps = 1e-3) {
      if (!proper) {
        warning("The default of 'proper' will be changed to 'TRUE' in v0.6.0.")
      }

      super$initialize(
        integrated = integrated,
        times = times,
        method = method,
        proper = proper,
        eps = eps,
        id = ifelse(se, "surv.schmid_se", "surv.schmid"),
        range = c(0, Inf),
        minimize = TRUE,
        packages = "distr6",
        predict_type = "distr",
        man = "mlr3proba::mlr_measures_surv.schmid",
      )

      private$.se = assertFlag(se)
    }
  ),

  active = list(
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

      score = weighted_survival_score("schmid", truth = prediction$truth,
        distribution = prediction$distr, times = self$times,
        proper = self$proper, train = train, eps = self$eps)

      if (self$se) {
        integrated_se(score, self$integrated)
      } else {
        integrated_score(score, self$integrated, self$method)
      }
    }
  )
)

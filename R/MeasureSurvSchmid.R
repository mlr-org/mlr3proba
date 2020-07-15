#' @template surv_measure
#' @templateVar title Integrated Schmid Score
#' @templateVar fullname MeasureSurvSchmid
#'
#' @description
#' Calculates the Integrated Schmid Score, aka integrated absolute loss.
#'
#' For an individual who dies at time \eqn{t}, with predicted Survival function, \eqn{S}, the
#' Schmid Score at time \eqn{t^*}{t*} is given by
#' \deqn{L(S,t|t^*) = [(S(t^*))I(t \le t^*, \delta = 1)(1/G(t))] + [((1 - S(t^*)))I(t > t^*)(1/G(t^*))]}{L(S,t|t*) = [(S(t*))I(t \le t*, \delta = 1)(1/G(t))] + [((1 - S(t*)))I(t > t*)(1/G(t*))]} # nolint
#' where \eqn{G} is the Kaplan-Meier estimate of the censoring distribution.
#'
#' @template measure_integrated
#' @template param_integrated
#' @template param_times
#' @template param_method
#'
#' @references
#' \cite{mlr3proba}{schemper_2000}
#' \cite{mlr3proba}{schmid_2011}
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvSchmid = R6::R6Class("MeasureSurvSchmid",
  inherit = MeasureSurvIntegrated,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, times, method = 2) {
      super$initialize(
        integrated = integrated,
        times = times,
        method = method,
        id = "surv.schmid",
        range = c(0, Inf),
        minimize = TRUE,
        packages = "distr6",
        predict_type = "distr",
        man = "mlr3proba::mlr_measures_surv.schmid"
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      integrated_score(
        score = weighted_survival_score("schmid",
                                        truth = prediction$truth,
                                        distribution = prediction$distr,
                                        times = self$times),
        integrated = self$integrated,
        method = self$method)
    }
  )
)

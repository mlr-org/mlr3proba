#' @template surv_measure
#' @templateVar title Standard Error of Integrated Graf Score
#' @templateVar inherit [MeasureSurv]
#' @templateVar fullname MeasureSurvGrafSE
#' @templateVar shortname surv.grafSE
#' @description
#' Calculates the standard error of [MeasureSurvGraf].
#'
#' The standard error is approximated using Binomial approxiation. For \eqn{N} observations in the
#' test set, the standard eror is given by
#' \deqn{IGS_SE = sd(IGS(S))/sqrt(N)}
#'
#' @references
#' Graf, E., Schmoor, C., Sauerbrei, W. and Schumacher, M. (1999).\cr
#' Assessment and comparison of prognostic classification schemes for survival data.\cr
#' Statistics in Medicine, 18(17), 2529-2545.\cr
#' \doi{10.1002/(SICI)1097-0258(19990915/30)18:17/18<2529::AID-SIM274>3.0.CO;2-5}
#'
#' @family Probabilistic survival measures
#' @export
MeasureSurvGrafSE = R6::R6Class("MeasureSurvGrafSE",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.grafSE",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr"
      )
    },

    score_internal = function(prediction, ...) {
      graf = integrated_graf(prediction$truth, prediction$distr)

      sd(graf)/sqrt(length(graf))
    }
  )
)

#' @template surv_measure
#' @templateVar title Log loss
#' @templateVar fullname MeasureSurvLogloss
#'
#' @description
#' Calculates the cross-entropy, or logarithmic (log), loss.
#'
#' The logloss, in the context of probabilistic predictions, is defined as the negative log
#' probability density function, \eqn{f}, evaluated at the observation time, \eqn{t},
#' \deqn{L(f, t) = -log(f(t))}
#'
#' The standard error of the Logloss, L, is approximated via,
#' \deqn{se(L) = sd(L)/\sqrt{N}}{se(L) = sd(L)/\sqrt N}
#' where \eqn{N} are the number of observations in the test set, and \eqn{sd} is the standard
#' deviation.
#'
#' The IPCW log loss is defined by
#' \deqn{L(f, t, \Delta) = -\Delta log(f(t))/G(t)}
#' where \eqn{\Delta} is the censoring indicator and G is the Kaplan-Meier estimator of the
#' censoring distribution.
#'
#' @template param_id
#' @template param_eps
#' @template field_eps
#' @template param_se
#' @template details_trainG
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvLogloss = R6::R6Class("MeasureSurvLogloss",
  inherit = MeasureSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param rm_cens `(logical(1))` \cr
    #' Deprecated, please use `IPCW` instead.
    #' @param IPCW `(logical(1))` \cr
    #' If `TRUE` (default) removes censored observations and weights score with IPC weighting
    #' calculated from the survival probability of the censoring distribution at the time of death.
    initialize = function(eps = 1e-15, se = FALSE, rm_cens = TRUE, IPCW = TRUE) {
      super$initialize(
        id = ifelse(se, "surv.logloss_se", "surv.logloss"),
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr",
        packages = "distr6",
        man = "mlr3proba::mlr_measures_surv.logloss",
      )

      private$.eps = assertNumeric(eps)
      private$.se = assertFlag(se)
      private$.rm_cens = assertFlag(rm_cens)
      private$.IPCW = assertFlag(IPCW)
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
    },

    #' @field rm_cens `(logical(1))` \cr
    #' Deprecated, please use `IPCW` instead.
    rm_cens = function(x) {
      if (!missing(x)) {
        private$.rm_cens = assertFlag(x)
      } else {
        return(private$.rm_cens)
      }
    },

    #' @field IPCW `(logical(1))` \cr
    #' If `TRUE` (default) removes censored observations and weights score with IPC weighting
    #' calculated from the survival probability of the censoring distribution at the time of death.
    IPCW = function(x) {
      if (!missing(x)) {
        private$.IPCW = assertFlag(x)
      } else {
        return(private$.IPCW)
      }
    }
  ),

  private = list(
    .eps = numeric(0),
    .se = FALSE,
    .rm_cens = TRUE,
    .IPCW = TRUE,
    .score = function(prediction, task, train_set, ...) {

      if (self$IPCW || self$rm_cens) {
        self$IPCW = TRUE
        self$rm_cens = FALSE
      }

      x = as.integer(!is.null(task)) + as.integer(!is.null(train_set))
      if (x == 1) {
        stop("Either 'task' and 'train_set' should be passed to measure or neither.")
      } else if (x) {
        train = task$truth(train_set)
      } else {
        train = NULL
      }

      if (self$se) {
        ll = surv_logloss(prediction$truth, prediction$distr, self$eps, self$IPCW, train)
        return(sd(ll) / sqrt(length(ll)))
      } else {
        return(mean(surv_logloss(prediction$truth, prediction$distr, self$eps, self$IPCW, train)))
      }
    }
  )
)

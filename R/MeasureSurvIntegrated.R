#' @title Abstract Class for Integrated Measures
#' @description This is an abstract class that should not be constructed directly.
#' @template param_integrated
#' @template param_times
#' @template param_method
#' @template param_proper
#' @template param_id
#' @template param_range
#' @template param_minimize
#' @template param_packages
#' @template param_predict_type
#' @template param_measure_properties
#' @template param_man
#' @template param_se
#' @template param_eps
#' @template field_eps
#' @export
MeasureSurvIntegrated = R6Class("MeasureSurvIntegrated",
  inherit = MeasureSurv,
  public = list(
    #' @description This is an abstract class that should not be constructed directly.
    initialize = function(integrated = TRUE, times, method = 2, proper = FALSE,
      eps = 1e-15, id, range, minimize, packages, predict_type,
      properties = character(), man = NA_character_, se = FALSE) {

      if (class(self)[[1]] == "MeasureSurvIntegrated") {
        stop("This is an abstract class that should not be constructed directly.")
      }

      super$initialize(
        id = id,
        range = range,
        minimize = minimize,
        packages = packages,
        predict_type = predict_type,
        properties = properties,
        man = man,
        se = se
      )

      private$.integrated = assertFlag(integrated)
      private$.eps = assertNumeric(eps)
      private$.proper = assertFlag(proper)

      if (!integrated) {
        if (missing(times)) {
          stop("For the non-integrated score, only a single time-point can be returned.")
        } else {
          assertNumeric(times,
            len = 1,
            .var.name = "For the non-integrated score, only a single time-point can be returned.")
        }
        private$.times = times
      } else {
        assertNumeric(method, 1, 2, any.missing = FALSE, all.missing = FALSE)
        private$.method = method
        if (!missing(times)) {
          assertNumeric(times)
          private$.times = times
          if (length(times) == 1) {
            private$.integrated = FALSE
          }
        }
      }
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

    #' @field integrated `(logical(1))`
    #' Returns if the measure should be integrated or not.
    #' Settable.
    integrated = function(integrated) {
      if (missing(integrated)) {
        return(private$.integrated)
      } else {
        assertFlag(integrated)
        if (!integrated & length(self$times) > 1) {
          stop(sprintf("For the non-integrated score, only a single time-point can be returned. Currently self$times = %s", paste0("c(", paste0(self$times, collapse = ", "), ")."))) # nolint
        }
        private$.integrated = integrated
      }
    },

    #' @field times `(numeric())`
    #' Returns the times at which the measure should be evaluated at, or integrated over.
    #' Settable.
    times = function(times) {
      if (!missing(times)) {
        if (!self$integrated) {
          assertNumeric(times,
            len = 1,
            .var.name = "For the non-integrated score, only a single time-point can
                        be returned.")
        } else {
          assertNumeric(times)
        }
        private$.times = times
      } else {
        return(private$.times)
      }
    },

    #' @field method `(integer(1))`
    #' Returns which method is used for approximating integration.
    #' Settable.
    method = function(method) {
      if (missing(method)) {
        return(private$.method)
      } else {
        assertNumeric(method, 1, 2, any.missing = FALSE, all.missing = FALSE)
        private$.method = as.integer(method)
      }
    },

    #' @field proper (`logical(1)`)\cr
    #' If `TRUE` then weights scores by the censoring distribution at the observed event time,
    #' which results in a strictly proper scoring rule if censoring and survival time
    #' distributions are independent and a sufficiently large dataset is used to weight the
    #' measure. If `FALSE` then weights scores by the Graf method which is the more common usage
    #' but the loss is not proper.
    proper = function(proper) {
      if (missing(proper)) {
        return(private$.proper)
      } else {
        private$.proper = assertFlag(proper)
      }
    }
  ),

  private = list(
    .eps = numeric(0),
    .integrated = logical(),
    .times = numeric(),
    .method = integer(),
    .proper = logical()
  )
)

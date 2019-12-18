MeasureSurvIntegrated = R6Class("MeasureSurvIntegrated",
  inherit = MeasureSurv,
  public = list(
    initialize = function(integrated = TRUE, times, id, range, minimize, packages, predict_type, properties) {
      if(class(self)[[1]] == "MeasureSurvIntegrated")
        stop("This is an abstract class that should not be constructed directly.")

      super$initialize(
        id = id,
        range = range,
        minimize = minimize,
        packages = packages,
        predict_type = predict_type,
        properties = properties
      )

      assertFlag(integrated)
      private$.integrated = integrated

      if (!integrated) {
        if(missing(times))
          stop("For the non-integrated score, only a single time-point can be returned.")
        else
          assertNumeric(times, len = 1,
                        .var.name = "For the non-integrated score, only a single time-point can be returned.")
        private$.times = times
      } else if (!missing(times)) {
        assertNumeric(times)
        private$.times = times
      }
    }
  ),

  active = list(
    integrated = function(integrated){
      if(missing(integrated)){
        return(private$.integrated)
      } else {
        assertFlag(integrated)
        if(!integrated & length(self$times) > 1) {
          stop(sprintf("For the non-integrated score, only a single time-point can be returned. Currently self$times = %s",
                       paste0("c(",paste0(self$times, collapse = ", "),").")))
        }
        private$.integrated <- integrated
      }
    },

    times = function(times){
      if (!missing(times)) {
        if (!self$integrated) {
          assertNumeric(times, len = 1,
                        .var.name = "For the non-integrated score, only a single time-point can
                        be returned.")
        } else {
          assertNumeric(times)
        }
        private$.times = times
      } else {
        return(private$.times)
      }
    }
  ),

  private = list(
    .integrated = logical(),
    .times = numeric()
  )
)

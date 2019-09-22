MeasureSurvLogloss = R6::R6Class("MeasureSurvLogloss",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.logloss",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr"
#       task_properties = "twoclass",
#        packages = "Metrics"
        )
      },

    score_internal = function(prediction, ...) {
      times = sort(unique(prediction$truth[,1]))
      ll = sapply(prediction$distr, function(x){
        pred = x$pdf(x1=times)
        pred[pred == 0] = 1e-5
        return(mean(-log(pred)))
      })
      return(mean(ll))
      }
  )
)

MeasureSurvAUC = R6Class("MeasureSurvAUC",
  inherit = MeasureSurv,
  public = list(
    initialize = function(integrated = TRUE, times, id, properties) {
      super$initialize(
        id = id,
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
        predict_type = "lp",
        properties = properties
      )

      assertFlag(integrated)
      private$.integrated <- integrated
#
#       if(!integrated) {
#         if(missing(times))
#           stop("If integrated is 'FALSE' then one numeric for 'times' must be given.")
#         else
#           assertNumeric(times)
#       } else {
        if(missing(times))
          times <- numeric()
        else
          assertNumeric(times)
      # }

      private$.times <- times
    },

    score_internal = function(prediction, learner, task, train_set, FUN, ...) {
      args = list()
      if("requires_train_set" %in% self$properties)
        args$Surv.rsp = task$truth(train_set)
      if ("requires_learner" %in% self$properties)
        args$lp = learner$model$linear.predictors

      args$times = self$times
      if(length(args$times) == 0)
        args$times = sort(unique(prediction$truth[, 1]))

      if("Surv.rsp.new" %in% names(formals(FUN)))
        args$Surv.rsp.new = prediction$truth

      auc = invoke(FUN, lpnew = prediction$lp, ..., .args = args)

      if(self$integrated) {
        return(auc$iauc)
      } else {
        return(auc)
      }
    }
  ),

  active = list(
    integrated = function(integrated){
      if(missing(integrated)){
        return(private$.integrated)
      } else {
        assertFlag(integrated)
        private$.integrated <- integrated
      }
    },

    times = function(times){
      if(missing(times)){
        return(private$.times)
      } else {
        assertNumeric(times)
        private$.times <- times
      }
    }
  ),

  private = list(
    .integrated = logical(),
    .times = numeric()
  )
)

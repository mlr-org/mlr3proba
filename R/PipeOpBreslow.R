PipeOpBreslow = R6Class("PipeOpBreslow",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "po_breslow") {
      super$initialize(
        id = id,
        input = data.table(name = c("train_task", "train_model", "test_pred"),
                           train = c("TaskSurv", "LearnerSurv", "NULL"),
                           predict = c("NULL", "NULL", "PredictionSurv")),
        output = data.table(name = "output",
                            train = "NULL",
                            predict = "PredictionSurv"),
        packages = c("mlr3proba", "distr6")
      )
    }
  ),

  private = list(
    .train = function(inputs) {
      browser()
      task = inputs$train_task
      learner = inputs$train_model

      if (is.null(learner$model))
        stopf("No trained model stored")

      # get predictions on the train set
      p = learner$predict(task)

      # take crank if lp is absent
      if (anyMissing(p$lp)) {
        lp_train = p$crank
      } else {
        lp_train = p$lp
      }

      times  = task$truth()[,1L]
      status = task$truth()[,2L]

      # keep the training data that Breslow estimator needs
      self$state = list(times = times, status = status, lp_train = lp_train)
      list(NULL)
    },

    .predict = function(inputs) {
      p = inputs$test_pred

      #browser()
      # take crank if lp is absent
      has_lp = anyMissing(p$lp) || !("lp" %in% p$predict_types)
      if (has_lp) {
        lp_test = p$crank
      } else {
        lp_test = p$lp
      }

      # check for overwrite?
      surv = surv_breslow(
        times = self$state$times,
        status = self$state$status,
        lp_train = self$state$lp_train,
        lp_test = lp_test
      )

      list(
        PredictionSurv$new(row_ids = p$row_ids, truth = p$truth,
          crank = p$crank, distr = surv, lp = lp_test)
      )
    }
  )
)

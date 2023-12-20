#' @title PipeOpBreslow
#' @name mlr_pipeops_compose_breslow_distr
#' @description
#' A short description...
#'
PipeOpBreslow = R6Class("PipeOpBreslow",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param id description
    initialize = function(learner, id = "po_breslow", param_vals = list(overwrite = FALSE)) {
      if ("lp" %nin% learner$predict_types) {
        stopf("Learner %s must provide lp predictions", learner$id)
      }

      private$.learner = as_learner(learner, clone = TRUE)

      # PipeOp-specific ParamSet
      ps_pipeop = ps(
        overwrite = p_lgl(default = FALSE, tags = c("predict"))
      )
      # add learner's ParamSet
      param_set = ParamSetCollection$new(
        list(private$.learner$param_set, ps_pipeop)
      )

      super$initialize(
        id = id,
        param_set = param_set,
        param_vals = param_vals,
        input = data.table(name = "input", train = "TaskSurv", predict = "TaskSurv"),
        output = data.table(name = "output", train = "NULL", predict = "PredictionSurv"),
        packages = learner$packages
      )
    }
  ),

  active = list(
    learner = function(rhs) {
      assert_ro_binding(rhs)
      private$.learner
    }
  ),

  private = list(
    .train = function(inputs) {
      # browser()
      task = inputs[[1]]
      learner = private$.learner

      # train learner
      learner$train(task)

      if (is.null(learner$model)) {
        stopf("No trained model stored")
      }

      # predictions on the train set
      p = learner$predict(task)

      # Breslow works only with `lp` predictions (not crank)
      if (anyMissing(p$lp)) {
        stopf("Missing lp predictions")
      }

      # keep the training data that Breslow estimator needs
      self$state = list(
        times = task$times(),
        status = task$status(),
        lp_train = p$lp
      )

      list(NULL)
    },

    .predict = function(inputs) {
      task = inputs[[1]]
      learner = private$.learner

      if (is.null(learner$model) && is.null(learner$state$fallback_state$model)) {
        stopf("Cannot predict, Learner '%s' has not been trained yet", learner$id)
      }

      # predictions on the test set
      p = learner$predict(task)

      pv = self$param_set$get_values(tags = "predict")
      if (is.null(pv$overwrite) || !pv$overwrite) {
        #browser()
        pred = list(p)
      } else {
        # missing lp
        if (anyMissing(p$lp)) {
          stopf("Missing lp predictions!")
        }

        distr = breslow(
          times = self$state$times,
          status = self$state$status,
          lp_train = self$state$lp_train,
          lp_test = p$lp
        )

        pred = list(PredictionSurv$new(
          row_ids = p$row_ids,
          truth = p$truth,
          crank = p$crank,
          lp = p$lp,
          distr = distr
        ))
      }

      pred
    },

    .learner = NULL
  )
)

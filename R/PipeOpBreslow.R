#' @title Wrap a learner into a PipeOp with survival predictions estimated by the Breslow estimator
#' @name mlr_pipeops_compose_breslow_distr
#' @template param_pipelines
#' @description
#' Composes a survival distribution (`distr`) using the linear predictor
#' predictions (`lp`) from a given [LearnerSurv] during training and prediction,
#' utilizing the [breslow estimator][breslow]. The specified `learner` must be
#' capable of generating `lp`-type predictions (e.g., a Cox-type model).
#'
#' @section Dictionary:
#' This [PipeOp][mlr3pipelines::PipeOp] can be instantiated via the
#' [Dictionary][mlr3misc::Dictionary] [mlr_pipeops][mlr3pipelines::mlr_pipeops]
#' or with the associated sugar function [po()][mlr3pipelines::po]:
#' ```
#' PipeOpBreslow$new(learner)
#' mlr_pipeops$get("breslowcompose", learner)
#' po("breslowcompose", learner, breslow.overwrite = TRUE)
#' ```
#'
#' @section Input and Output Channels:
#' [PipeOpBreslow] is like a [LearnerSurv].
#' It has one input channel, named `input` that takes a [TaskSurv] during training
#' and another [TaskSurv] during prediction.
#' [PipeOpBreslow] has one output channel named `output`, producing `NULL` during
#' training and a [PredictionSurv] during prediction.
#'
#' @section State:
#' The `$state` slot stores the `times` and `status` survival target variables of
#' the train [TaskSurv] as well as the `lp` predictions on the train set.
#'
#' @section Parameters:
#' The parameters are:
#' * `breslow.overwrite` :: `logical(1)` \cr
#'    If `FALSE` (default) then the compositor does nothing and returns the
#'    input `learner`'s [PredictionSurv].
#'    If `TRUE` or in the case that the input `learner` doesn't have `distr`
#'    predictions, then the `distr` is overwritten with the `distr` composed
#'    from `lp` and the train set information using [breslow].
#'    This is useful for changing the prediction `distr` from one model form to
#'    another.
#' @seealso [pipeline_distrcompositor]
#' @export
#' @family survival compositors
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3pipelines)
#'   task = tsk("rats")
#'   part = partition(task, ratio = 0.8)
#'   train_task = task$clone()$filter(part$train)
#'   test_task  = task$clone()$filter(part$test)
#'
#'   learner = lrn("surv.coxph") # learner with lp predictions
#'   b = po("breslowcompose", learner = learner, breslow.overwrite = TRUE)
#'
#'   b$train(list(train_task))
#'   p = b$predict(list(test_task))[[1L]]
#' }
#' }
PipeOpBreslow = R6Class("PipeOpBreslow",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param learner ([LearnerSurv])\cr
    #' Survival learner which must provide `lp`-type predictions
    #' @param id (character(1))\cr
    #' Identifier of the resulting object. If `NULL` (default), it will be set
    #' as the `id` of the input `learner`.
    initialize = function(learner, id = NULL, param_vals = list()) {
      assert_learner(learner, task_type = "surv")
      if ("lp" %nin% learner$predict_types) {
        stopf("Learner %s must provide lp predictions", learner$id)
      }

      # id of the PipeOp is the id of the learner
      private$.learner = as_learner(learner, clone = TRUE)
      id = id %??% private$.learner$id

      # define `breslow.overwrite` parameter
      private$.breslow_ps = ps(
        overwrite = p_lgl(tags = c("predict", "required"))
      )
      private$.breslow_ps$values = list(overwrite = FALSE)
      if ("set_id" %in% names(private$.learner$param_set)) {
        # old paradox
        private$.learner$param_set$set_id = ""
        private$.breslow_ps$set_id = "breslow"
      }
      super$initialize(
        id = id,
        param_set = alist(breslow = private$.breslow_ps, private$.learner$param_set),
        param_vals = param_vals,
        input = data.table(name = "input", train = "TaskSurv", predict = "TaskSurv"),
        output = data.table(name = "output", train = "NULL", predict = "PredictionSurv"),
        packages = private$.learner$packages
      )
    }
  ),

  active = list(
    #' @field learner ([mlr3::Learner])\cr
    #' The input survival learner.
    learner = function(rhs) {
      assert_ro_binding(rhs)
      private$.learner
    }
  ),

  private = list(
    .train = function(inputs) {
      task = inputs[[1]]
      learner = private$.learner

      # train learner
      learner$train(task)

      # predictions on the train set
      p = learner$predict(task)

      # Breslow works only with non-NA `lp` predictions
      if (anyMissing(p$lp)) {
        stopf("Missing lp predictions")
      }

      # keep the training data that Breslow estimator needs
      self$state = list(
        learner = learner,
        times = task$times(),
        status = task$status(),
        lp_train = p$lp
      )

      list(NULL)
    },

    .predict = function(inputs) {
      task = inputs[[1]]
      learner = self$state$learner

      if (is.null(learner$model)) {
        stopf("Cannot predict, Learner '%s' has not been trained yet", learner$id)
      }

      # predictions on the test set
      p = learner$predict(task)

      pv = self$param_set$get_values(tags = "predict")
      overwrite = pv$breslow.overwrite

      # If learner predicts `distr` and overwrite is FALSE don't use breslow
      if ("distr" %in% learner$predict_types & !overwrite) {
        pred = list(p)
      } else {
        # Breslow works only with non-NA `lp` predictions
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

    .breslow_ps = NULL,
    .learner = NULL,
    .additional_phash_input = function() private$.learner$phash
  )
)

#' @title PipeOpTaskSurvRegr
#'
#' @name mlr_pipeops_trafotask_survregr
#'
#' @description
#' Transform [TaskSurv] to [TaskRegr][mlr3::TaskRegr].
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [PipeOpTaskTransformer].
#'
#' The output is the input [TaskSurv] transformed to a [TaskRegr][mlr3::TaskRegr].
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements
#'
#' * `instatus`: Censoring status from input training task.
#' * `outstatus` : Censoring status from input prediction task.
#'
#' @section Parameters:
#' The parameters are
#'
#' * `method::character(1))`\cr
#' Method to use for dealing with censoring. Options are `"ipcw"` (Vock et al., 2016): censoring
#' is column is removed and a `weights` column is added, weights are inverse estimated survival
#'  probability of the censoring distribution evaluated at survival time;
#'  `"mrl"` (Klein and Moeschberger, 2003): survival time of censored
#' observations is transformed to the observed time plus the mean residual life-time at the moment
#' of censoring; `"delete"`: censored observations are deleted from the data-set - should be
#' used with caution if censoring is informative; `"omit"`: the censoring status column is
#' deleted - again should be used with caution. Note that `"mrl"` and `"ipcw"` will perform worse
#' with Type I censoring.
#' * `estimator::(character(1))`\cr
#' Method for calculating censoring weights or mean residual lifetime in `"mrl"`,
#' current options are: `"kaplan"`: unconditional Kaplan-Meier estimator;
#'  `"akritas"`: conditional non-parameteric nearest-neighbours estimator;
#' `"cox"`.
#' * `alpha::(numeric(1))`\cr
#' When `ipcw` is used, optional hyper-parameter that adds an extra penalty to the weighting for
#' censored observations. If set to `0` then censored observations are given zero weight and
#' deleted, weighting only the non-censored observations. A weight for an observation is then
#' \eqn{(\delta + \alpha(1-\delta))/G(t)} where \eqn{\delta} is the censoring indicator.
#' * `lambda::(numeric(1))`\cr
#' Nearest neighbours parameter for [akritas][mlr3learners.proba::akritas] estimator, default `0.5`.
#'
#' @references
#' \cite{mlr3proba}{klein_2003}
#'
#' \cite{mlr3proba}{vock_2016}
#'
#' @examples
#' library("mlr3")
#'
#' # these methods are generally only successful if censoring is not too high
#' # create survival task by undersampling
#' set.seed(1)
#' task = tsk("rats")$filter(
#'    c(which(tsk("rats")$truth()[,2]==1),
#'    sample(which(tsk("rats")$truth()[,2]==0), 42))
#'    )
#'
#' # deletion
#' po = po("trafotask_survregr", method = "delete")
#' po$train(list(task = task))[[1]] # 42 deleted
#'
#' # omission
#' po = po("trafotask_survregr", method = "omit")
#' po$train(list(task = task))[[1]]
#'
#' # ipcw with Akritas
#' po = po("trafotask_survregr", method = "ipcw", estimator = "akritas", lambda = 0.4, alpha = 0)
#' new_task = po$train(list(task = task))[[1]]
#' print(new_task)
#' new_task$weights
#'
#' # mrl with Kaplan-Meier
#' po = po("trafotask_survregr", method = "mrl")
#' new_task = po$train(list(task = task))[[1]]
#' data.frame(new = new_task$truth(), old = task$truth())
#'
#' # outcome doesn't matter in predicting so omission is used
#' po$predict(list(task = task))[[1]]
#'
#' # note status is saved for use in later transformations
#' po$state
#'
#' @family PipeOps
#' @family Transformation PipeOps
#' @include PipeOpPredTransformer.R
#' @export
PipeOpTaskSurvRegr = R6Class("PipeOpTaskSurvRegr",
  inherit = PipeOpTaskTransformer,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier of the resulting  object.
    initialize = function(id = "trafotask_survregr", param_vals = list()) {
      ps = ParamSet$new(list(
        ParamFct$new("method", default = "ipcw", levels = c("ipcw", "mrl", "delete", "omit"),
                     tags = "train"),
        ParamFct$new("estimator", default = "kaplan", levels = c("kaplan", "akritas", "cox"),
                     tags = "train"),
        ParamDbl$new("alpha", default = 1, lower = 0, upper = 1, tags = "train"),
        ParamDbl$new("lambda", default = 0.5, lower = 0, upper = 1, tags = "train"),
        ParamDbl$new("eps", default = 1e-15, lower = 0, upper = 1, tags = "train")
      ))
      ps$add_dep("alpha", "method", CondEqual$new("ipcw"))
      ps$add_dep("eps", "method", CondEqual$new("ipcw"))
      ps$add_dep("estimator", "method", CondAnyOf$new(c("ipcw", "mrl")))
      ps$add_dep("lambda", "estimator", CondEqual$new("akritas"))

      super$initialize(id = id,
                       param_set = ps,
                       param_vals = param_vals,
                       input = data.table(name = "input", train = "TaskSurv", predict = "TaskSurv"),
                       output = data.table(name = "output", train = "TaskRegr", predict = "TaskRegr")
      )
    },

    predict_internal = function(inputs) {
      input = inputs[[1]]
      self$state$outstatus = input$truth()[, 2L]
      status = input$target_names[2L]
      backend = private$.omit(copy(input$data()), status)
      return(list(TaskRegr$new(id = input$id, backend = backend, target = input$target_names[1L])))
    }
  ),

  private = list(
    .transform = function(input) {

      input = input[[1]]
      backend = copy(input$data())
      time = input$target_names[1L]
      status = input$target_names[2L]

      self$state$instatus = input$truth()[,2L]

      method = self$param_set$values$method
      if (is.null(method)) {
        method = "ipcw"
      }
      estimator = self$param_set$values$estimator
      if (is.null(estimator)) {
        estimator = "kaplan"
      }
      eps = self$param_set$values$eps
      if (is.null(eps)) {
        eps = 1e-15
      }



      backend = switch(method,
                       ipcw = private$.ipcw(backend, status, time, estimator, eps),
                       mrl = private$.mrl(backend, status, time, input, estimator),
                       delete = private$.delete(backend, status),
                       omit = private$.omit(backend, status)
                       )

      new_task = TaskRegr$new(id = input$id, backend = backend, target = time)

      if (method == "ipcw") {
        new_task$col_roles$weight = "ipc_weights"
      }

      return(new_task)
    },

    .ipcw = function(backend, status, time, estimator, eps) {
      cens = backend[[status]] == 0
      new_backend = copy(backend)
      new_backend[[status]] = 1 - new_backend[[status]]
      task = TaskSurv$new("ipcw", new_backend, time, status)

      est = switch(estimator,
                   kaplan = LearnerSurvKaplan,
                   cox = LearnerSurvCoxPH,
                   akritas = mlr3learners.proba::LearnerSurvAkritas)$new()
      if (estimator == "akritas") {
        est$param_set$values$lambda = self$param_set$values$lambda
      }

      est = est$train(task)$predict(task)$distr
      weights = as.numeric(est$survival(data = matrix(task$truth()[,1], nrow = 1)))
      weights[weights == 0] = eps
      weights = 1/weights

      alpha = self$param_set$values$alpha
      if (!is.null(alpha)) {
        # catch 0 * Inf error
        if (alpha == 0) {
          weights[cens] = 0
        } else {
          weights[cens] = weights[cens] * alpha
        }
      }

      backend$ipc_weights = weights
      return(subset(backend, select = -status))
    },

    .mrl = function(backend, status, time, input, estimator) {

      cens = backend[[status]] == 0
      upper = max(backend[[time]])
      unique_times = sort(unique(backend[[time]]))

      if (estimator == "kaplan") {
        kaplan = LearnerSurvKaplan$new()$train(input)$predict(input, row_ids = 1)$distr[1]
        mrl = sapply(backend[[time]][cens], function(x) {
          int_range = unique_times[x <= unique_times & upper >= unique_times]
          (sum(kaplan$survival(int_range)) * (diff(range(int_range))/length(int_range))) /
            kaplan$survival(x)
        })
      } else {
        if (estimator == "cox") {
          est = LearnerSurvCoxPH$new()$train(input)$predict(input)$distr
        } else {
          est = mlr3learners.proba::LearnerSurvAkritas$new()
          est$param_set$values$lambda = self$param_set$values$lambda
          est = est$train(input)$predict(input)$distr
        }

        den = as.numeric(est$survival(data = matrix(backend[[time]], nrow = 1)))[cens]
        mrl = numeric(sum(cens))
        for (i in seq_along(mrl)) {
          x = backend[cens, ][[time]][i]
          int_range = unique_times[x <= unique_times & upper >= unique_times]
          num = (sum(est[i]$survival(int_range)) * (diff(range(int_range))/length(int_range)))
          mrl[i] = num/den[i]
        }
      }

      backend[[time]][cens] =  backend[[time]][cens] + mrl
      return(subset(backend, select = -status))
    },

    .delete = function(backend, status) {
      subset(backend, status == 1, select = -status)
    },

    .omit = function(backend, status) {
      subset(backend, select = -status)
    }
  )
)


#' @title PipeOpTaskSurvRegr
#' @template param_pipelines
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
#' of censoring; `"bj"` (Buckley and James, 1979): Buckley-James imputation assuming an AFT
#' model form, calls [bujar::bujar]; `"delete"`: censored observations are deleted from the
#' data-set - should be used with caution if censoring is informative; `"omit"`: the censoring
#' status column is deleted - again should be used with caution; `"reorder"`: selects features and
#' targets and sets the target in the new task object. Note that `"mrl"` and `"ipcw"` will perform
#' worse with Type I censoring.
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
#' * `eps::numeric(1)`\cr
#' Small value to replace `0` survival probabilities with in IPCW to prevent infinite weights.
#' * `lambda::(numeric(1))`\cr
#' Nearest neighbours parameter for the `"akritas"` estimator in the [mlr3extralearners package](https://mlr3extralearners.mlr-org.com/), default `0.5`.
#' * `features, target :: character())`\cr
#' For `"reorder"` method, specify which columns become features and targets.
#' * `learner cneter, mimpu, iter.bj, max.cycle, mstop, nu`\cr
#' Passed to [bujar::bujar].
#'
#' @references
#' `r format_bib("buckley_1979", "klein_2003", "vock_2016")`
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3pipelines)
#'
#'   # these methods are generally only successful if censoring is not too high
#'   # create survival task by undersampling
#'   task = tsk("rats")$filter(
#'     c(which(tsk("rats")$truth()[, 2] == 1),
#'       sample(which(tsk("rats")$truth()[, 2] == 0), 42))
#'   )
#'
#'   # deletion
#'   po = po("trafotask_survregr", method = "delete")
#'   po$train(list(task, NULL))[[1]] # 42 deleted
#'
#'   # omission
#'   po = po("trafotask_survregr", method = "omit")
#'   po$train(list(task, NULL))[[1]]
#'
#'   if (requireNamespace("mlr3extralearners", quietly = TRUE)) {
#'     # ipcw with Akritas
#'     po = po("trafotask_survregr", method = "ipcw", estimator = "akritas", lambda = 0.4, alpha = 0)
#'     new_task = po$train(list(task, NULL))[[1]]
#'     print(new_task)
#'     new_task$weights
#'   }
#'
#'   # mrl with Kaplan-Meier
#'   po = po("trafotask_survregr", method = "mrl")
#'   new_task = po$train(list(task, NULL))[[1]]
#'   data.frame(new = new_task$truth(), old = task$truth())
#'
#'   # Buckley-James imputation
#'   if (requireNamespace("bujar", quietly = TRUE)) {
#'     po = po("trafotask_survregr", method = "bj")
#'     new_task = po$train(list(task, NULL))[[1]]
#'     data.frame(new = new_task$truth(), old = task$truth())
#'   }
#'
#'   # reorder - in practice this will be only be used in a few graphs
#'   po = po("trafotask_survregr", method = "reorder", features = c("sex", "rx", "time", "status"),
#'     target = "litter")
#'   new_task = po$train(list(task, NULL))[[1]]
#'   print(new_task)
#'
#'   # reorder using another task for feature names
#'   po = po("trafotask_survregr", method = "reorder", target = "litter")
#'   new_task = po$train(list(task, task))[[1]]
#'   print(new_task)
#' }
#' }
#' @family PipeOps
#' @family Transformation PipeOps
#' @include PipeOpPredTransformer.R
#' @export
PipeOpTaskSurvRegr = R6Class("PipeOpTaskSurvRegr",
  inherit = PipeOpTaskTransformer,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "trafotask_survregr", param_vals = list()) {
      ps = ps(
        method    = p_fct(default = "ipcw", levels = c("ipcw", "mrl", "bj", "delete", "omit", "reorder"), tags = "train"),
        estimator = p_fct(
          default = "kaplan", levels = c("kaplan", "akritas", "cox"), tags = "train", depends = quote(method %in% c("ipcw", "mrl"))),
        alpha     = p_dbl(0, 1, default = 1, tags = "train", depends = quote(method == "ipcw")),
        lambda    = p_dbl(0, 1, default = 0.5, tags = "train", depends = quote(estimator == "akritas")),
        eps       = p_dbl(0, 1, default = 1e-15, tags = "train", depends = quote(method == "ipcw")),
        features  = p_uty(tags = "train", depends = quote(method == "reorder")),
        target    = p_uty(tags = "train", depends = quote(method == "reorder")),
        learner   = p_fct(default = "linear.regression",
          levels = c("linear.regression", "mars", "pspline", "tree", "acosso", "enet", "enet2", "mnet", "snet"),
          tags = c("train", "bj"),
          depends = quote(method == "bj")),
        center    = p_lgl(default = TRUE, tags = c("train", "bj"), depends = quote(method == "bj")),
        mimpu     = p_lgl(default = NULL, special_vals = list(NULL), tags = c("train", "bj"), depends = quote(method == "bj")),
        iter.bj   = p_int(2L, default = 20L, tags = c("train", "bj"), depends = quote(method == "bj")),
        max.cycle = p_int(1L, default = 5L, tags = c("train", "bj")),
        mstop     = p_int(1L, default = 50L, tags = c("train", "bj"), depends = quote(method == "bj")),
        nu        = p_dbl(0, default = 0.1, tags = c("train", "bj"), depends = quote(method == "bj"))
      )

      super$initialize(id = id,
        param_set = ps,
        param_vals = param_vals,
        input = data.table(name = c("input", "input_features"),
          train = c("TaskSurv", "*"),
          predict = c("TaskSurv", "*")),
        output = data.table(name = "output", train = "TaskRegr", predict = "TaskRegr")
      )
    }
  ),

  private = list(
    .predict = function(inputs) {
      pv = self$param_set$values
      target = pv$target
      if (is.null(target)) {
        target = inputs[[1L]]$target_names[1L]
      }
      backend = private$.reorder(copy(inputs[[1L]]$data()), pv$features, target, inputs[[2L]])
      return(list(TaskRegr$new(id = inputs[[1L]]$id, backend = backend, target = target)))
    },

    .transform = function(inputs) {

      input = inputs[[1L]]
      backend = copy(input$data())
      time = input$target_names[1L]
      status = input$target_names[2L]

      pv = self$param_set$values

      method = pv$method
      if (is.null(method)) {
        method = "ipcw"
      }
      estimator = pv$estimator
      if (is.null(estimator)) {
        estimator = "kaplan"
      }
      eps = pv$eps
      if (is.null(eps)) {
        eps = 1e-15
      }

      backend = switch(method,
        ipcw = private$.ipcw(backend, status, time, estimator, eps),
        mrl = private$.mrl(backend, status, time, input, estimator),
        bj = private$.bj(backend, status, time),
        delete = private$.delete(backend, status),
        omit = private$.omit(backend, status),
        reorder = private$.reorder(backend, pv$features, pv$target, inputs[[2]])
      )

      target = if (method == "reorder") pv$target else time

      new_task = TaskRegr$new(id = input$id, backend = backend, target = target)

      if (method == "ipcw") {
        new_task$col_roles$weights_learner = "ipc_weights"
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
        akritas = get_akritas_learner()
      )$new()
      if (estimator == "akritas") {
        est$param_set$values$lambda = self$param_set$values$lambda
      }

      est = est$train(task)$predict(task)$distr
      if (inherits(est, c("Matdist", "Arrdist"))) {
        weights = diag(est$survival(task$truth()[, 1L]))
      } else {
        weights = as.numeric(est$survival(data = matrix(task$truth()[, 1L], nrow = 1L)))
      }
      weights[weights == 0] = eps
      weights = 1 / weights

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
        est = LearnerSurvKaplan$new()$train(input)$predict(input, row_ids = 1)$distr[1L]
        den = est$survival(backend[[time]][cens])
        num = sapply(backend[[time]][cens], function(x) {
          est$survivalAntiDeriv(x)
        })
        mrl = num / den
      } else {
        if (estimator == "cox") {
          est = LearnerSurvCoxPH$new()$train(input)$predict(input)$distr
        } else {
          est = get_akritas_learner()$new()
          est$param_set$values$lambda = self$param_set$values$lambda
          est = est$train(input)$predict(input)$distr
        }
        den = as.numeric(est$survival(data = matrix(backend[[time]], nrow = 1L)))[cens]
        mrl = numeric(sum(cens))
        for (i in seq_along(mrl)) {
          x = backend[cens, ][[time]][i]
          int_range = unique_times[x <= unique_times & upper >= unique_times]
          num = (sum(est[i]$survival(int_range)) * (diff(range(int_range)) / length(int_range)))
          mrl[i] = num / den[i]
        }
      }

      backend[[time]][cens] = backend[[time]][cens] + mrl
      return(subset(backend, select = -status))
    },

    .bj = function(backend, status, time) {
      require_namespaces("bujar")

      x = data.frame(backend)[, colnames(backend) %nin% c(time, status), drop = FALSE]
      x = model.matrix(~., x)[, -1L]

      bj = invoke(bujar::bujar,
        y = backend[[time]],
        cens = backend[[status]],
        x = x,
        tuning = FALSE,
        vimpint = FALSE,
        .args = self$param_set$get_values(tags = "bj")
      )
      backend[[time]] = bj$ynew
      return(backend)
    },

    .delete = function(backend, status) {
      subset(backend, status == 1, select = -status)
    },

    .omit = function(backend, status) {
      subset(backend, select = -status)
    },

    .reorder = function(backend, features, target, task) {
      if (is.null(task)) {
        if (is.null(features)) {
          stop("One of 'features' or 'task' must be provided.")
        }
        features = subset(backend, select = features)
      } else {
        assert_class(task, "TaskSurv")
        features = copy(task$data(cols = task$feature_names))
      }

      if (target %nin% colnames(features)) {
        target = subset(backend, select = target)
        return(cbind(features, target))
      } else {
        return(features)
      }
    }
  )
)

register_pipeop("trafotask_survregr", PipeOpTaskSurvRegr)

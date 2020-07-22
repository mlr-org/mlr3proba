#' @title Average Survival Predictions
#' @description This is a wrapper around the [PipeOpSurvAvg] pipe operation, which
#' simplifies graph creation.
#' @param learners `(list())` \cr
#' List of [LearnerSurv]s to average.
#' @param param_vals `(list())` \cr
#' Parameters, including weights, to pass to [PipeOpSurvAvg].
#' @details For full details see [PipeOpSurvAvg].
#' @return [mlr3pipelines::GraphLearner]
#' @examples
#' \dontrun{
#' library("mlr3")
#'
#' task = tgen("simsurv")$generate(5)
#' avg = surv_averager(
#'   learners = lrns(c("surv.kaplan", "surv.coxph")),
#'   param_vals = list(weights = c(0.1, 0.9))
#'  )
#' avg$train(task)$predict(task)
#' }
#' @export
surv_averager = function(learners, param_vals = list()) {

  learners = mlr3pipelines::gunion(mlr3misc::map(learners,
                                                 function(.x) mlr3pipelines::po("learner", .x)))
  po = mlr3pipelines::po("survavg", param_vals = param_vals)

  mlr3pipelines::GraphLearner$new(mlr3pipelines::`%>>%`(learners, po))
}

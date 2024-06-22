#' @name mlr_graphs_survtoclassif
#' @title Survival to Classification Reduction Pipeline
#' @description Wrapper around multiple [PipeOp][mlr3pipelines::PipeOp]s to help in creation
#' of complex survival to reduction methods.
#'
#' @param classif_learner [LearnerClassif][mlr3::LearnerClassif]\cr
#' Classification learner to fit to the transformed [TaskClassif][mlr3::TaskClassif]. `classif_learner` must have `prob` predict_type.
#' @param cut `numeric()`\cr
#' Split points, used to partition the data into intervals.
#' If unspecified, all unique event times will be used.
#' @param max_time `numeric(1)`\cr
#' If cut is unspecified, this will be the last possible event time.
#' All event times after max_time will be administratively censored at max_time.
#'
#' @return [mlr3pipelines::Graph] or [mlr3pipelines::GraphLearner]
#' @family pipelines
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#'   library("mlr3")
#'   library("mlr3pipelines")
#'
#'   task = tsk("rats")
#'
#'   pipe = ppl(
#'     "survtoclassif",
#'     classif_learner = lrn("classif.log_reg")
#'   )
#'   pipe$train(task)
#'   pipe$predict(task)
#'
#' }
#' }
#' @export
pipeline_survtoclassif = function(classif_learner, cut = NULL, max_time = NULL) {
  gr = mlr3pipelines::po("trafotask_survclassif", cut = cut, max_time = max_time) |>
    mlr3pipelines::`%>>%`(list(mlr3pipelines::po("learner", classif_learner, predict_type = "prob"), mlr3pipelines::po("nop"))) |>
    mlr3pipelines::`%>>%`(mlr3pipelines::po("trafopred_classifsurv"))

  gr
}

register_graph("survtoclassif", pipeline_survtoclassif)

#' @param learner `[mlr3::Learner]|[mlr3pipelines::PipeOp]|[mlr3pipelines::Graph]` \cr
#' Either a `Learner` which will be wrapped in [mlr3pipelines::PipeOpLearner], a `PipeOp` which will
#' be wrapped in [mlr3pipelines::Graph] or a `Graph` itself. Underlying `Learner` should be
#' [LearnerRegr][mlr3::LearnerRegr].

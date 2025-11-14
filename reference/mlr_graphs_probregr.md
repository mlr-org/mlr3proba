# Estimate Regression distr Predict Type Pipeline

Wrapper around
[PipeOpProbregr](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_compose_probregr.md)
to simplify
[Graph](https://mlr3pipelines.mlr-org.com/reference/Graph.html)
creation.

**\[experimental\]**

## Usage

``` r
pipeline_probregr(
  learner,
  learner_se = NULL,
  dist = "Uniform",
  graph_learner = FALSE
)
```

## Arguments

- learner:

  `[mlr3::Learner]|[mlr3pipelines::PipeOp]|[mlr3pipelines::Graph]`  
  Either a `Learner` which will be wrapped in
  [mlr3pipelines::PipeOpLearner](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner.html),
  a `PipeOp` which will be wrapped in
  [mlr3pipelines::Graph](https://mlr3pipelines.mlr-org.com/reference/Graph.html)
  or a `Graph` itself. Underlying `Learner` should be
  [LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html).

- learner_se:

  `[mlr3::Learner]|[mlr3pipelines::PipeOp]`  
  Optional
  [LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)
  with predict_type `se` to estimate the standard error. If left `NULL`
  then `learner` must have `se` in predict_types.

- dist:

  (`character(1)`)  
  Location-scale distribution to use for composition. Current
  possibilities are'
  `"Cauchy", "Gumbel", "Laplace", "Logistic", "Normal", "Uniform"`.
  Default is `"Uniform"`.

- graph_learner:

  (`logical(1)`)  
  If `TRUE` returns wraps the
  [Graph](https://mlr3pipelines.mlr-org.com/reference/Graph.html) as a
  [GraphLearner](https://mlr3pipelines.mlr-org.com/reference/mlr_learners_graph.html)
  otherwise (default) returns as a `Graph`.

## Value

[mlr3pipelines::Graph](https://mlr3pipelines.mlr-org.com/reference/Graph.html)
or
[mlr3pipelines::GraphLearner](https://mlr3pipelines.mlr-org.com/reference/mlr_learners_graph.html)

## Dictionary

This [Graph](https://mlr3pipelines.mlr-org.com/reference/Graph.html) can
be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_graphs](https://mlr3pipelines.mlr-org.com/reference/mlr_graphs.html)
or with the associated sugar function
[ppl()](https://mlr3pipelines.mlr-org.com/reference/ppl.html):

    mlr_graphs$get("probregr")
    ppl("probregr")

## See also

Other pipelines:
[`mlr_graphs_crankcompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_crankcompositor.md),
[`mlr_graphs_distrcompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_distrcompositor.md),
[`mlr_graphs_responsecompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_responsecompositor.md),
[`mlr_graphs_survaverager`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survaverager.md),
[`mlr_graphs_survbagging`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survbagging.md),
[`mlr_graphs_survtoclassif_IPCW`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survtoclassif_IPCW.md),
[`mlr_graphs_survtoclassif_disctime`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survtoclassif_disctime.md),
[`mlr_graphs_survtoregr_pem`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survtoregr_pem.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  library(mlr3)
  library(mlr3pipelines)

  task = tsk("boston_housing")

  # method 1 - same learner for response and se
  pipe = ppl(
    "probregr",
    learner = lrn("regr.featureless", predict_type = "se"),
    dist = "Uniform"
  )
  pipe$train(task)
  pipe$predict(task)

  # method 2 - different learners for response and se
  pipe = ppl(
    "probregr",
    learner = lrn("regr.rpart"),
    learner_se = lrn("regr.featureless", predict_type = "se"),
    dist = "Normal"
  )
  pipe$train(task)
  pipe$predict(task)
} # }
```

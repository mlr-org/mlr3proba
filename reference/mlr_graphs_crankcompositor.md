# Estimate Survival crank Predict Type Pipeline

Wrapper around
[PipeOpCrankCompositor](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_crankcompose.md)
to simplify
[Graph](https://mlr3pipelines.mlr-org.com/reference/Graph.html)
creation.

## Usage

``` r
pipeline_crankcompositor(
  learner,
  method = c("mort"),
  overwrite = FALSE,
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
  [LearnerSurv](https://mlr3proba.mlr-org.com/reference/LearnerSurv.md).

- method:

  (`character(1)`)  
  Determines what method should be used to produce a continuous ranking
  from the distribution. Currently only `mort` is supported, which is
  the sum of the cumulative hazard, also called *expected/ensemble
  mortality*, see Ishwaran et al. (2008). For more details, see
  [`get_mortality()`](https://mlr3proba.mlr-org.com/reference/get_mortality.md).

- overwrite:

  (`logical(1)`)  
  If `FALSE` (default) and the prediction already has a `crank`
  prediction, then the compositor returns the input prediction
  unchanged. If `TRUE`, then the `crank` will be overwritten.

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

    mlr_graphs$get("crankcompositor")
    ppl("crankcompositor")

## See also

Other pipelines:
[`mlr_graphs_distrcompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_distrcompositor.md),
[`mlr_graphs_probregr`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_probregr.md),
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

  task = tsk("lung")
  part = partition(task)

  # change the crank prediction type of a Cox's model predictions
  grlrn = ppl(
    "crankcompositor",
    learner = lrn("surv.coxph"),
    method = "mort",
    overwrite = TRUE,
    graph_learner = TRUE
  )
  grlrn$train(task, part$train)
  grlrn$predict(task, part$test)
} # }
```

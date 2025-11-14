# Estimate Survival Time/Response Predict Type Pipeline

Wrapper around
[PipeOpResponseCompositor](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_responsecompose.md)
to simplify
[Graph](https://mlr3pipelines.mlr-org.com/reference/Graph.html)
creation.

## Usage

``` r
pipeline_responsecompositor(
  learner,
  method = "rmst",
  tau = NULL,
  add_crank = FALSE,
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
  Determines what method should be used to produce a survival time
  (response) from the survival distribution. Available methods are
  `"rmst"` and `"median"`, corresponding to the *restricted mean
  survival time* and the *median survival time* respectively.

- tau:

  (`numeric(1)`)  
  Determines the time point up to which we calculate the restricted mean
  survival time (works only for the `"rmst"` method). If `NULL`
  (default), all the available time points in the predicted survival
  distribution will be used.

- add_crank:

  (`logical(1)`)  
  If `TRUE` then `crank` predict type will be set as `-response` (as
  higher survival times correspond to lower risk). Works only if
  `overwrite` is `TRUE`.

- overwrite:

  (`logical(1)`)  
  If `FALSE` (default) and the prediction already has a `response`
  prediction, then the compositor returns the input prediction
  unchanged. If `TRUE`, then the `response` (and the `crank`, if
  `add_crank` is `TRUE`) will be overwritten.

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

    mlr_graphs$get("responsecompositor")
    ppl("responsecompositor")

## See also

Other pipelines:
[`mlr_graphs_crankcompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_crankcompositor.md),
[`mlr_graphs_distrcompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_distrcompositor.md),
[`mlr_graphs_probregr`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_probregr.md),
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

  # add survival time prediction type to the predictions of a Cox model
  grlrn = ppl(
    "responsecompositor",
    learner = lrn("surv.coxph"),
    method = "rmst",
    overwrite = TRUE,
    graph_learner = TRUE
  )
  grlrn$train(task, part$train)
  grlrn$predict(task, part$test)
} # }
```

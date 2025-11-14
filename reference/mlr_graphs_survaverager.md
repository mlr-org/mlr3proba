# Survival Prediction Averaging Pipeline

Wrapper around
[PipeOpSurvAvg](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_survavg.md)
to simplify
[Graph](https://mlr3pipelines.mlr-org.com/reference/Graph.html)
creation.

## Usage

``` r
pipeline_survaverager(learners, param_vals = list(), graph_learner = FALSE)
```

## Arguments

- learners:

  `(list())`  
  List of
  [LearnerSurv](https://mlr3proba.mlr-org.com/reference/LearnerSurv.md)s
  to average.

- param_vals:

  `(list())`  
  Parameters, including weights, to pass to
  [PipeOpSurvAvg](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_survavg.md).

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

    mlr_graphs$get("survaverager")
    ppl("survaverager")

## See also

Other pipelines:
[`mlr_graphs_crankcompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_crankcompositor.md),
[`mlr_graphs_distrcompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_distrcompositor.md),
[`mlr_graphs_probregr`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_probregr.md),
[`mlr_graphs_responsecompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_responsecompositor.md),
[`mlr_graphs_survbagging`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survbagging.md),
[`mlr_graphs_survtoclassif_IPCW`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survtoclassif_IPCW.md),
[`mlr_graphs_survtoclassif_disctime`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survtoclassif_disctime.md),
[`mlr_graphs_survtoregr_pem`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survtoregr_pem.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  library(mlr3)
  library(mlr3pipelines)

  task = tsk("rats")
  pipe = ppl(
    "survaverager",
    learners = lrns(c("surv.kaplan", "surv.coxph")),
    param_vals = list(weights = c(0.1, 0.9)),
    graph_learner = FALSE
  )
  pipe$train(task)
  pipe$predict(task)
} # }
```

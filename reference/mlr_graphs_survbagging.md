# Survival Prediction Averaging Pipeline

Wrapper around
[PipeOpSubsample](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_subsample.html)
and
[PipeOpSurvAvg](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_survavg.md)
to simplify
[Graph](https://mlr3pipelines.mlr-org.com/reference/Graph.html)
creation.

## Usage

``` r
pipeline_survbagging(
  learner,
  iterations = 10,
  frac = 0.7,
  avg = TRUE,
  weights = 1,
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

- iterations:

  (`integer(1)`)  
  Number of bagging iterations. Defaults to 10.

- frac:

  (`numeric(1)`)  
  Percentage of rows to keep during subsampling. See
  [PipeOpSubsample](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_subsample.html)
  for more information. Defaults to 0.7.

- avg:

  (`logical(1)`)  
  If `TRUE` (default) predictions are aggregated with
  [PipeOpSurvAvg](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_survavg.md),
  otherwise returned as multiple predictions. Can only be `FALSE` if
  `graph_learner = FALSE`.

- weights:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Weights for model avering, ignored if `avg = FALSE`. Default is
  uniform weighting, see
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

## Details

Bagging (Bootstrap AGGregatING) is the process of bootstrapping data and
aggregating the final predictions. Bootstrapping splits the data into
`B` smaller datasets of a given size and is performed with
[PipeOpSubsample](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_subsample.html).
Aggregation is the sample mean of deterministic predictions and a
[MixtureDistribution](https://xoopr.github.io/distr6/reference/MixtureDistribution.html)
of distribution predictions. This can be further enhanced by using a
weighted average by supplying `weights`.

## Dictionary

This [Graph](https://mlr3pipelines.mlr-org.com/reference/Graph.html) can
be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_graphs](https://mlr3pipelines.mlr-org.com/reference/mlr_graphs.html)
or with the associated sugar function
[ppl()](https://mlr3pipelines.mlr-org.com/reference/ppl.html):

    mlr_graphs$get("survbagging")
    ppl("survbagging")

## See also

Other pipelines:
[`mlr_graphs_crankcompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_crankcompositor.md),
[`mlr_graphs_distrcompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_distrcompositor.md),
[`mlr_graphs_probregr`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_probregr.md),
[`mlr_graphs_responsecompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_responsecompositor.md),
[`mlr_graphs_survaverager`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survaverager.md),
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
    "survbagging",
    learner = lrn("surv.coxph"),
    iterations = 5,
    graph_learner = FALSE
  )
  pipe$train(task)
  pipe$predict(task)
} # }
```

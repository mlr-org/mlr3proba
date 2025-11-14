# Estimate Survival distr Predict Type Pipeline

Wrapper around
[PipeOpDistrCompositor](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_distrcompose.md)
or
[PipeOpBreslow](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_compose_breslow_distr.md)
to simplify
[Graph](https://mlr3pipelines.mlr-org.com/reference/Graph.html)
creation.

**\[experimental\]**

## Usage

``` r
pipeline_distrcompositor(
  learner,
  estimator = "kaplan",
  form = "aft",
  overwrite = FALSE,
  scale_lp = FALSE,
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

- estimator:

  (`character(1)`)  
  One of `kaplan` (default), `nelson` or `breslow`, corresponding to the
  Kaplan-Meier, Nelson-Aalen and
  [Breslow](https://mlr3proba.mlr-org.com/reference/breslow.md)
  estimators respectively. Used to estimate the baseline survival
  distribution.

- form:

  (`character(1)`)  
  One of `aft` (default), `ph`, or `po`, corresponding to accelerated
  failure time, proportional hazards, and proportional odds
  respectively. Used to determine the form of the composed survival
  distribution. Ignored if estimator is `breslow`.

- overwrite:

  (`logical(1)`)  
  If `FALSE` (default) then if the `learner` already has a `distr`, the
  compositor does nothing. If `TRUE` then the `distr` is overwritten by
  the compositor if already present, which may be required for changing
  the prediction `distr` from one model form to another.

- scale_lp:

  (`logical(1)`)  
  If `TRUE` and `form` is `"aft"`, the linear predictor scores are
  scaled before the composition. Experimental option, see more details
  on
  [PipeOpDistrCompositor](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_distrcompose.md).
  Default is `FALSE`.

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

    mlr_graphs$get("distrcompositor")
    ppl("distrcompositor")

## See also

Other pipelines:
[`mlr_graphs_crankcompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_crankcompositor.md),
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
  library(mlr3pipelines)

  # let's change the distribution prediction of Cox (Breslow-based) to an AFT form:
  task = tsk("rats")
  grlrn = ppl(
    "distrcompositor",
    learner = lrn("surv.coxph"),
    estimator = "kaplan",
    form = "aft",
    overwrite = TRUE,
    graph_learner = TRUE
  )
  grlrn$train(task)
  grlrn$predict(task)
} # }
```

# Survival to Classification Reduction using Discrete Time Pipeline

Wrapper around
[PipeOpTaskSurvClassifDiscTime](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafotask_survclassif_disctime.md)
and
[PipeOpPredClassifSurvDiscTime](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_classifsurv_disctime.md)
to simplify
[Graph](https://mlr3pipelines.mlr-org.com/reference/Graph.html)
creation.

## Usage

``` r
pipeline_survtoclassif_disctime(
  learner,
  cut = NULL,
  max_time = NULL,
  graph_learner = FALSE
)
```

## Arguments

- learner:

  [LearnerClassif](https://mlr3.mlr-org.com/reference/LearnerClassif.html)  
  Classification learner to fit the transformed
  [TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html).
  `learner` must have `predict_type` of type `"prob"`.

- cut:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Split points, used to partition the data into intervals. If
  unspecified, all unique event times will be used. If `cut` is a single
  integer, it will be interpreted as the number of equidistant intervals
  from 0 until the maximum event time.

- max_time:

  (`numeric(1)`)  
  If cut is unspecified, this will be the last possible event time. All
  event times after max_time will be administratively censored at
  max_time.

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

The pipeline consists of the following steps:

1.  [PipeOpTaskSurvClassifDiscTime](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafotask_survclassif_disctime.md)
    Converts
    [TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md) to a
    [TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html).

2.  A
    [LearnerClassif](https://mlr3.mlr-org.com/reference/LearnerClassif.html)
    is fit and predicted on the new `TaskClassif`.

3.  [PipeOpPredClassifSurvDiscTime](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_classifsurv_disctime.md)
    transforms the resulting
    [PredictionClassif](https://mlr3.mlr-org.com/reference/PredictionClassif.html)
    to
    [PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md).

4.  Optionally:
    [PipeOpModelMatrix](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_modelmatrix.html)
    is used to transform the formula of the task before fitting the
    learner.

## Dictionary

This [Graph](https://mlr3pipelines.mlr-org.com/reference/Graph.html) can
be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_graphs](https://mlr3pipelines.mlr-org.com/reference/mlr_graphs.html)
or with the associated sugar function
[ppl()](https://mlr3pipelines.mlr-org.com/reference/ppl.html):

    mlr_graphs$get("survtoclassif_disctime")
    ppl("survtoclassif_disctime")

## References

Tutz, Gerhard, Schmid, Matthias (2016). *Modeling Discrete Time-to-Event
Data*, series Springer Series in Statistics. Springer International
Publishing. ISBN 978-3-319-28156-8 978-3-319-28158-2,
<http://link.springer.com/10.1007/978-3-319-28158-2>.

## See also

Other pipelines:
[`mlr_graphs_crankcompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_crankcompositor.md),
[`mlr_graphs_distrcompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_distrcompositor.md),
[`mlr_graphs_probregr`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_probregr.md),
[`mlr_graphs_responsecompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_responsecompositor.md),
[`mlr_graphs_survaverager`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survaverager.md),
[`mlr_graphs_survbagging`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survbagging.md),
[`mlr_graphs_survtoclassif_IPCW`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survtoclassif_IPCW.md),
[`mlr_graphs_survtoregr_pem`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survtoregr_pem.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)

  task = tsk("lung")
  part = partition(task)

  grlrn = ppl(
    "survtoclassif_disctime",
    learner = lrn("classif.log_reg"),
    cut = 4, # 4 equidistant time intervals
    graph_learner = TRUE
  )
  grlrn$train(task, row_ids = part$train)
  grlrn$predict(task, row_ids = part$test)
} # }
```

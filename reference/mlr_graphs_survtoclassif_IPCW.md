# Survival to Classification Reduction using IPCW Pipeline

Wrapper around
[PipeOpTaskSurvClassifIPCW](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafotask_survclassif_IPCW.md)
and
[PipeOpPredClassifSurvIPCW](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_classifsurv_IPCW.md)
to simplify
[Graph](https://mlr3pipelines.mlr-org.com/reference/Graph.html)
creation.

## Usage

``` r
pipeline_survtoclassif_IPCW(
  learner,
  tau = NULL,
  eps = 0.001,
  graph_learner = FALSE
)
```

## Arguments

- learner:

  [LearnerClassif](https://mlr3.mlr-org.com/reference/LearnerClassif.html)  
  Classification learner to fit the transformed
  [TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html).

- tau:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Predefined time point for IPCW. Observations with time larger than
  \\\tau\\ are censored. Must be less or equal to the maximum event
  time.

- eps:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Small value to replace \\G(t) = 0\\ censoring probabilities to prevent
  infinite weights (a warning is triggered if this happens).

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

1.  [PipeOpTaskSurvClassifIPCW](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafotask_survclassif_IPCW.md)
    Converts
    [TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md) to a
    [TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html).

2.  A
    [LearnerClassif](https://mlr3.mlr-org.com/reference/LearnerClassif.html)
    is fit and predicted on the new `TaskClassif`.

3.  [PipeOpPredClassifSurvIPCW](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_classifsurv_IPCW.md)
    transforms the resulting
    [PredictionClassif](https://mlr3.mlr-org.com/reference/PredictionClassif.html)
    to
    [PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md).

## Dictionary

This [Graph](https://mlr3pipelines.mlr-org.com/reference/Graph.html) can
be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_graphs](https://mlr3pipelines.mlr-org.com/reference/mlr_graphs.html)
or with the associated sugar function
[ppl()](https://mlr3pipelines.mlr-org.com/reference/ppl.html):

    mlr_graphs$get("survtoclassif_IPCW")
    ppl("survtoclassif_IPCW")

Additional alias id for pipeline construction:

    ppl("survtoclassif_vock")

## References

Vock, M D, Wolfson, Julian, Bandyopadhyay, Sunayan, Adomavicius,
Gediminas, Johnson, E P, Vazquez-Benitez, Gabriela, O'Connor, J P
(2016). “Adapting machine learning techniques to censored time-to-event
health record data: A general-purpose approach using inverse probability
of censoring weighting.” *Journal of Biomedical Informatics*, **61**,
119–131.
[doi:10.1016/j.jbi.2016.03.009](https://doi.org/10.1016/j.jbi.2016.03.009)
, <https://www.sciencedirect.com/science/article/pii/S1532046416000496>.

## See also

Other pipelines:
[`mlr_graphs_crankcompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_crankcompositor.md),
[`mlr_graphs_distrcompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_distrcompositor.md),
[`mlr_graphs_probregr`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_probregr.md),
[`mlr_graphs_responsecompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_responsecompositor.md),
[`mlr_graphs_survaverager`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survaverager.md),
[`mlr_graphs_survbagging`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survbagging.md),
[`mlr_graphs_survtoclassif_disctime`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survtoclassif_disctime.md),
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
    "survtoclassif_IPCW",
    learner = lrn("classif.rpart"),
    tau = 500, # Observations after 500 days are censored
    graph_learner = TRUE
  )
  grlrn$train(task, row_ids = part$train)
  pred = grlrn$predict(task, row_ids = part$test)
  pred # crank and distr at the cutoff time point included

  # score predictions
  pred$score() # C-index
  pred$score(msr("surv.brier", times = 500, integrated = FALSE)) # Brier score at tau
} # }
```

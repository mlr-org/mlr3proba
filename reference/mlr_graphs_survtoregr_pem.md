# Survival to Poisson Regression Reduction Pipeline

Wrapper around multiple
[PipeOp](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)s to
help in creation of complex survival reduction methods.

## Usage

``` r
pipeline_survtoregr_pem(
  learner,
  cut = NULL,
  max_time = NULL,
  graph_learner = FALSE
)
```

## Arguments

- learner:

  [LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)  
  Regression learner to fit the transformed
  [TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html).
  `learner` must be able to handle `offset` and support optimization of
  a poisson likelihood.

- cut:

  [`numeric()`](https://rdrr.io/r/base/numeric.html)  
  Split points, used to partition the data into intervals. If
  unspecified, all unique event times will be used. If `cut` is a single
  integer, it will be interpreted as the number of equidistant intervals
  from 0 until the maximum event time.

- max_time:

  `numeric(1)`  
  If cut is unspecified, this will be the last possible event time. All
  event times after max_time will be administratively censored at
  max_time.

- graph_learner:

  `logical(1)`  
  If `TRUE` returns wraps the
  [Graph](https://mlr3pipelines.mlr-org.com/reference/Graph.html) as a
  [GraphLearner](https://mlr3pipelines.mlr-org.com/reference/mlr_learners_graph.html)
  otherwise (default) returns as a `Graph`.

## Value

[mlr3pipelines::Graph](https://mlr3pipelines.mlr-org.com/reference/Graph.html)
or
[mlr3pipelines::GraphLearner](https://mlr3pipelines.mlr-org.com/reference/mlr_learners_graph.html)

## Details

A brief mathematical summary of PEMs (see referenced article for more
detail):

1.  **PED Transformation:** Survival data is converted into piece-wise
    exponential data (PED) format. Key elements are: Continuous time is
    divided into \\j = 1, \ldots, J\\ intervals for each subject, \\i =
    1, \ldots, n\\. A status variable in each entry indicates whether an
    event or censoring occurred during that interval. For any subject,
    data entries are created only up until the interval including the
    event time. An offset column is introduced and represents the
    logarithm of the time a subject spent in any given interval. For
    more details, see
    [`pammtools::as_ped()`](https://adibender.github.io/pammtools/reference/as_ped.html).

2.  **Hazard Estimation with PEM:** The PED transformation combined with
    the working assumption \$\$\delta\_{ij} \stackrel{\text{iid}}{\sim}
    Poisson \left( \mu\_{ij} = \lambda\_{ij} t\_{ij} \right),\$\$ where
    \\\delta\_{ij}\\ denotes the event or censoring indicator, allows
    framing the problem of piecewise constant hazard estimation as a
    poisson regression with offset. Specifically, we want to estimate
    \$\$\lambda(t \mid \mathbf{x}\_i) := exp(g(x\_{i},t\_{j})), \quad
    \forall t \in \[t\_{j-1}, t\_{j}), \quad i = 1, \dots, n.\$\$
    \\g(x\_{i},t\_{j})\\ is a general function of features \\x\\ and
    \\t\\, i.e. a learner, and may include non-linearity and complex
    feature interactions. Two important prerequisites of the learner are
    its capacity to model a poisson likelihood and accommodate the
    offset.

3.  **From Piecewise Hazards to Survival Probabilities:** Lastly, the
    computed hazards are back transformed to survival probabilities via
    the following identity \$\$S(t \| \mathbf{x}) = \exp \left( -
    \int\_{0}^{t} \lambda(s \| \mathbf{x}) \\ ds \right) = \exp \left( -
    \sum\_{j = 1}^{J} \lambda(j \| \mathbf{x}) d_j\\ \right),\$\$ where
    \\d_j\\ specifies the duration of interval \\j\\.

The previous considerations are reflected in the pipeline which consists
of the following steps:

1.  [PipeOpTaskSurvRegrPEM](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafotask_survregr_pem.md)
    Converts
    [TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md) to a
    [TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html).

2.  A [LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html)
    is fit and predicted on the new `TaskRegr`.

3.  [PipeOpPredRegrSurvPEM](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_regrsurv_pem.md)
    transforms the resulting
    [PredictionRegr](https://mlr3.mlr-org.com/reference/PredictionRegr.html)
    to
    [PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md).

## References

Bender, Andreas, Groll, Andreas, Scheipl, Fabian (2018). “A generalized
additive model approach to time-to-event analysis.” *Statistical
Modelling*, **18**(3-4), 299–321.
<https://doi.org/10.1177/1471082X17748083>.

## See also

Other pipelines:
[`mlr_graphs_crankcompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_crankcompositor.md),
[`mlr_graphs_distrcompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_distrcompositor.md),
[`mlr_graphs_probregr`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_probregr.md),
[`mlr_graphs_responsecompositor`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_responsecompositor.md),
[`mlr_graphs_survaverager`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survaverager.md),
[`mlr_graphs_survbagging`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survbagging.md),
[`mlr_graphs_survtoclassif_IPCW`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survtoclassif_IPCW.md),
[`mlr_graphs_survtoclassif_disctime`](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survtoclassif_disctime.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  library(mlr3)
  library(mlr3learners)
  library(mlr3extralearners)
  library(mlr3pipelines)

  task = tsk("lung")
  part = partition(task)

  # typically model formula and features types are extracted from the task
  learner = lrn("regr.gam", family = "poisson")
  grlrn = ppl(
   "survtoregr_pem",
    learner = learner,
    graph_learner = TRUE
  )
  grlrn$train(task, row_ids = part$train)
  grlrn$predict(task, row_ids = part$test)

  # In some instances special formulas can be specified in the learner
  learner = lrn("regr.gam", family = "poisson", formula = pem_status ~ s(tend) + s(age) + meal.cal)
  grlrn = ppl(
   "survtoregr_pem",
    learner = learner,
    graph_learner = TRUE
  )
  grlrn$train(task, row_ids = part$train)
  grlrn$predict(task, row_ids = part$test)

  # if necessary encode data before passing to learner with e.g. po("encode"),
  # po("modelmatrix"), etc.
  # With po("modelmatrix") feature types and formula can be adjusted at the same time
  cut = round(seq(0, max(task$data()$time), length.out = 20))
  learner = as_learner(
    po("modelmatrix", formula = ~ as.factor(tend) + .) %>>%
    lrn("regr.glmnet", family = "poisson", lambda = 0)
  )
  grlrn = ppl(
    "survtoregr_pem",
    learner = learner,
    cut = cut,
    graph_learner = TRUE
  )
  grlrn$train(task, row_ids = part$train)
  grlrn$predict(task, row_ids = part$test)

  # xgboost regression learner
  learner = as_learner(
    po("modelmatrix", formula = ~ .) %>>%
    lrn("regr.xgboost", objective = "count:poisson", nrounds = 100, eta = 0.1)
  )

  grlrn = ppl(
    "survtoregr_pem",
    learner = learner,
    graph_learner = TRUE
  )
  grlrn$train(task, row_ids = part$train)
  grlrn$predict(task, row_ids = part$test)
} # }
```

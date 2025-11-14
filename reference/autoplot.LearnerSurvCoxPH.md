# Plots for Cox Proportional Hazards Learner

Visualizations for
[LearnerSurvCoxPH](https://mlr3proba.mlr-org.com/reference/mlr_learners_surv.coxph.md).

The argument `type` controls what kind of plot is drawn. The only
possible choice right now is `"ggforest"` which is a Forest Plot, using
[ggforest](https://rdrr.io/pkg/survminer/man/ggforest.html). This plot
displays the estimated hazard ratios (HRs) and their confidence
intervals (CIs) for different variables included in the trained model.

## Usage

``` r
# S3 method for class 'LearnerSurvCoxPH'
autoplot(object, type = "ggforest", ...)
```

## Arguments

- object:

  ([LearnerSurvCoxPH](https://mlr3proba.mlr-org.com/reference/mlr_learners_surv.coxph.md)).

- type:

  (character(1))  
  Type of the plot. See description.

- ...:

  Additional parameters passed down to `ggforest`.

## Value

[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Examples

``` r
library(ggplot2)

task = tsk("lung")
learner = lrn("surv.coxph")
learner$train(task)
autoplot(learner)
```

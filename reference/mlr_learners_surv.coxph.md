# Cox Proportional Hazards Survival Learner

Calls
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html).

- lp is predicted by
  [`survival::predict.coxph()`](https://rdrr.io/pkg/survival/man/predict.coxph.html)

- distr is predicted by
  [`survival::survfit.coxph()`](https://rdrr.io/pkg/survival/man/survfit.coxph.html)

- `crank` is identical to `lp`

## Dictionary

This [Learner](https://mlr3.mlr-org.com/reference/Learner.html) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_learners](https://mlr3.mlr-org.com/reference/mlr_learners.html) or
with the associated sugar function
[lrn()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    LearnerSurvCoxPH$new()
    mlr_learners$get("surv.coxph")
    lrn("surv.coxph")

## Meta Information

- Task type: “surv”

- Predict Types: “crank”, “distr”, “lp”

- Feature Types: “logical”, “integer”, “numeric”, “factor”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3),
  [mlr3proba](https://CRAN.R-project.org/package=mlr3proba),
  [survival](https://CRAN.R-project.org/package=survival),
  [distr6](https://CRAN.R-project.org/package=distr6)

## Parameters

|             |           |         |                                    |              |
|-------------|-----------|---------|------------------------------------|--------------|
| Id          | Type      | Default | Levels                             | Range        |
| ties        | character | efron   | efron, breslow, exact              | \-           |
| singular.ok | logical   | TRUE    | TRUE, FALSE                        | \-           |
| type        | character | efron   | efron, aalen, kalbfleisch-prentice | \-           |
| stype       | integer   | 2       |                                    | \\\[1, 2\]\\ |

## References

Cox DR (1972). “Regression Models and Life-Tables.” *Journal of the
Royal Statistical Society: Series B (Methodological)*, **34**(2),
187–202.
[doi:10.1111/j.2517-6161.1972.tb00899.x](https://doi.org/10.1111/j.2517-6161.1972.tb00899.x)
.

## See also

Other survival learners:
[`mlr_learners_surv.kaplan`](https://mlr3proba.mlr-org.com/reference/mlr_learners_surv.kaplan.md),
[`mlr_learners_surv.rpart`](https://mlr3proba.mlr-org.com/reference/mlr_learners_surv.rpart.md)

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`mlr3proba::LearnerSurv`](https://mlr3proba.mlr-org.com/reference/LearnerSurv.md)
-\> `LearnerSurvCoxPH`

## Methods

### Public methods

- [`LearnerSurvCoxPH$new()`](#method-LearnerSurvCoxPH-new)

- [`LearnerSurvCoxPH$clone()`](#method-LearnerSurvCoxPH-clone)

Inherited methods

- [`mlr3::Learner$base_learner()`](https://mlr3.mlr-org.com/reference/Learner.html#method-base_learner)
- [`mlr3::Learner$configure()`](https://mlr3.mlr-org.com/reference/Learner.html#method-configure)
- [`mlr3::Learner$encapsulate()`](https://mlr3.mlr-org.com/reference/Learner.html#method-encapsulate)
- [`mlr3::Learner$format()`](https://mlr3.mlr-org.com/reference/Learner.html#method-format)
- [`mlr3::Learner$help()`](https://mlr3.mlr-org.com/reference/Learner.html#method-help)
- [`mlr3::Learner$predict()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict)
- [`mlr3::Learner$predict_newdata()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict_newdata)
- [`mlr3::Learner$print()`](https://mlr3.mlr-org.com/reference/Learner.html#method-print)
- [`mlr3::Learner$reset()`](https://mlr3.mlr-org.com/reference/Learner.html#method-reset)
- [`mlr3::Learner$selected_features()`](https://mlr3.mlr-org.com/reference/Learner.html#method-selected_features)
- [`mlr3::Learner$train()`](https://mlr3.mlr-org.com/reference/Learner.html#method-train)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://rdrr.io/pkg/R6/man/R6Class.html) class.

#### Usage

    LearnerSurvCoxPH$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerSurvCoxPH$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

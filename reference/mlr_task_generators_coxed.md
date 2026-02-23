# Survival Task Generator for Package 'coxed'

A [TaskGenerator](https://mlr3.mlr-org.com/reference/TaskGenerator.html)
calling
[`coxed::sim.survdata()`](https://rdrr.io/pkg/coxed/man/sim.survdata.html).

This generator creates a survival dataset using `coxed`, and exposes
some parameters from the
[`coxed::sim.survdata()`](https://rdrr.io/pkg/coxed/man/sim.survdata.html)
function. We don't include the parameters `X` (user-specified
variables), `covariate`, `low`, `high`, `compare`, `beta` and
`hazard.fun` for this generator. The latter means that no user-specified
hazard function can be used and the generated datasets always use the
*flexible-hazard* method from the package.

## Dictionary

This
[TaskGenerator](https://mlr3.mlr-org.com/reference/TaskGenerator.html)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_task_generators](https://mlr3.mlr-org.com/reference/mlr_task_generators.html)
or with the associated sugar function
[tgen()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    mlr_task_generators$get("coxed")
    tgen("coxed")

## Parameters

|             |           |         |              |                  |
|-------------|-----------|---------|--------------|------------------|
| Id          | Type      | Default | Levels       | Range            |
| T           | numeric   | 100     |              | \\\[1, \infty)\\ |
| type        | character | none    | none, tvbeta | \-               |
| knots       | integer   | 8       |              | \\\[1, \infty)\\ |
| spline      | logical   | TRUE    | TRUE, FALSE  | \-               |
| xvars       | integer   | 3       |              | \\\[1, \infty)\\ |
| mu          | untyped   | 0       |              | \-               |
| sd          | untyped   | 0.5     |              | \-               |
| censor      | numeric   | 0.1     |              | \\\[0, 1\]\\     |
| censor.cond | logical   | FALSE   | TRUE, FALSE  | \-               |

## References

Harden, J. J, Kropko, Jonathan (2019). “Simulating Duration Data for the
Cox Model.” *Political Science Research and Methods*, **7**(4), 921–928.
[doi:10.1017/PSRM.2018.19](https://doi.org/10.1017/PSRM.2018.19) .

## See also

- [Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  of
  [TaskGenerators](https://mlr3.mlr-org.com/reference/TaskGenerator.html):
  [mlr3::mlr_task_generators](https://mlr3.mlr-org.com/reference/mlr_task_generators.html)

- `as.data.table(mlr_task_generators)` for a table of available
  [TaskGenerators](https://mlr3.mlr-org.com/reference/TaskGenerator.html)
  in the running session

Other TaskGenerator:
[`mlr_task_generators_simdens`](https://mlr3proba.mlr-org.com/reference/mlr_task_generators_simdens.md),
[`mlr_task_generators_simsurv`](https://mlr3proba.mlr-org.com/reference/mlr_task_generators_simsurv.md)

## Super class

[`mlr3::TaskGenerator`](https://mlr3.mlr-org.com/reference/TaskGenerator.html)
-\> `TaskGeneratorCoxed`

## Methods

### Public methods

- [`TaskGeneratorCoxed$new()`](#method-TaskGeneratorCoxed-new)

- [`TaskGeneratorCoxed$help()`](#method-TaskGeneratorCoxed-help)

- [`TaskGeneratorCoxed$clone()`](#method-TaskGeneratorCoxed-clone)

Inherited methods

- [`mlr3::TaskGenerator$format()`](https://mlr3.mlr-org.com/reference/TaskGenerator.html#method-format)
- [`mlr3::TaskGenerator$generate()`](https://mlr3.mlr-org.com/reference/TaskGenerator.html#method-generate)
- [`mlr3::TaskGenerator$print()`](https://mlr3.mlr-org.com/reference/TaskGenerator.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://rdrr.io/pkg/R6/man/R6Class.html) class.

#### Usage

    TaskGeneratorCoxed$new()

------------------------------------------------------------------------

### Method [`help()`](https://rdrr.io/r/utils/help.html)

Opens the corresponding help page referenced by field `$man`.

#### Usage

    TaskGeneratorCoxed$help()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaskGeneratorCoxed$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
  library(mlr3)

  # time horizon = 365 days, censoring proportion = 60%, 6 covariates normally
  # distributed with mean = 1 and sd = 2, independent censoring, no time-varying
  # effects
  gen = tgen("coxed", T = 365, type = "none", censor = 0.6, xvars = 6,
              mu = 1, sd = 2, censor.cond = FALSE)
  gen$generate(50)
#> 
#> ── <TaskSurv> (50x8) ───────────────────────────────────────────────────────────
#> • Target: y and failed
#> • Properties: -
#> • Features (6):
#>   • dbl (6): X1, X2, X3, X4, X5, X6

  # same as above, but with time-varying coefficients
  gen$param_set$set_values(type = "tvbeta")
  gen$generate(50)
#> 
#> ── <TaskSurv> (50x8) ───────────────────────────────────────────────────────────
#> • Target: y and failed
#> • Properties: -
#> • Features (6):
#>   • dbl (6): X1, X2, X3, X4, X5, X6
```

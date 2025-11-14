# Survival Task Generator for Package 'simsurv'

A
[mlr3::TaskGenerator](https://mlr3.mlr-org.com/reference/TaskGenerator.html)
calling
[`simsurv::simsurv()`](https://rdrr.io/pkg/simsurv/man/simsurv.html)
from package [simsurv](https://CRAN.R-project.org/package=simsurv).

This generator currently only exposes a small subset of the flexibility
of [simsurv](https://CRAN.R-project.org/package=simsurv), and just
creates a small dataset with the following numerical covariates:

- `treatment`: Bernoulli distributed with hazard ratio `0.5`.

- `height`: Normally distributed with hazard ratio `1`.

- `weight`: normally distributed with hazard ratio `1`.

See [`simsurv::simsurv()`](https://rdrr.io/pkg/simsurv/man/simsurv.html)
for an explanation of the hyperparameters. Initial values for
hyperparameters are `lambdas` = 0.1, `gammas` = 1.5 and `maxt` = 5. The
last one, by default generates samples which are administratively
censored at \\\tau = 5\\, so increase this value if you want to change
this.

## Dictionary

This
[TaskGenerator](https://mlr3.mlr-org.com/reference/TaskGenerator.html)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_task_generators](https://mlr3.mlr-org.com/reference/mlr_task_generators.html)
or with the associated sugar function
[tgen()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    mlr_task_generators$get("simsurv")
    tgen("simsurv")

## Parameters

|         |           |         |                                |                  |
|---------|-----------|---------|--------------------------------|------------------|
| Id      | Type      | Default | Levels                         | Range            |
| dist    | character | weibull | weibull, exponential, gompertz | \-               |
| lambdas | numeric   | \-      |                                | \\\[0, \infty)\\ |
| gammas  | numeric   | \-      |                                | \\\[0, \infty)\\ |
| maxt    | numeric   | \-      |                                | \\\[0, \infty)\\ |

## References

Brilleman, L. S, Wolfe, Rory, Moreno-Betancur, Margarita, Crowther, J. M
(2021). “Simulating Survival Data Using the simsurv R Package.” *Journal
of Statistical Software*, **97**(3), 1–27.
[doi:10.18637/JSS.V097.I03](https://doi.org/10.18637/JSS.V097.I03) .

## See also

- [Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  of
  [TaskGenerators](https://mlr3.mlr-org.com/reference/TaskGenerator.html):
  [mlr3::mlr_task_generators](https://mlr3.mlr-org.com/reference/mlr_task_generators.html)

- `as.data.table(mlr_task_generators)` for a table of available
  [TaskGenerators](https://mlr3.mlr-org.com/reference/TaskGenerator.html)
  in the running session

Other TaskGenerator:
[`mlr_task_generators_coxed`](https://mlr3proba.mlr-org.com/reference/mlr_task_generators_coxed.md),
[`mlr_task_generators_simdens`](https://mlr3proba.mlr-org.com/reference/mlr_task_generators_simdens.md)

## Super class

[`mlr3::TaskGenerator`](https://mlr3.mlr-org.com/reference/TaskGenerator.html)
-\> `TaskGeneratorSimsurv`

## Methods

### Public methods

- [`TaskGeneratorSimsurv$new()`](#method-TaskGeneratorSimsurv-new)

- [`TaskGeneratorSimsurv$help()`](#method-TaskGeneratorSimsurv-help)

- [`TaskGeneratorSimsurv$clone()`](#method-TaskGeneratorSimsurv-clone)

Inherited methods

- [`mlr3::TaskGenerator$format()`](https://mlr3.mlr-org.com/reference/TaskGenerator.html#method-format)
- [`mlr3::TaskGenerator$generate()`](https://mlr3.mlr-org.com/reference/TaskGenerator.html#method-generate)
- [`mlr3::TaskGenerator$print()`](https://mlr3.mlr-org.com/reference/TaskGenerator.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TaskGeneratorSimsurv$new()

------------------------------------------------------------------------

### Method [`help()`](https://rdrr.io/r/utils/help.html)

Opens the corresponding help page referenced by field `$man`.

#### Usage

    TaskGeneratorSimsurv$help()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaskGeneratorSimsurv$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
  # generate 20 samples with Weibull survival distribution
  gen = tgen("simsurv")
  task = gen$generate(20)
  head(task)
#>    eventtime status   height treatment   weight
#>        <num>  <int>    <num>     <int>    <num>
#> 1:  1.379427      1 151.8680         1 80.52075
#> 2:  5.000000      0 185.9242         1 82.85124
#> 3:  5.000000      0 166.2240         1 89.84010
#> 4:  5.000000      0 150.3844         1 81.55950
#> 5:  4.250381      1 163.7347         1 90.79093
#> 6:  5.000000      0 172.9649         1 85.11452

  # generate 100 samples with exponential survival distribution and tau = 40
  gen = tgen("simsurv", dist = "exponential", gammas = NULL, maxt = 40)
  task = gen$generate(100)
  head(task)
#>     eventtime status   height treatment   weight
#>         <num>  <int>    <num>     <int>    <num>
#> 1:  4.5918305      1 157.2615         0 60.97819
#> 2:  3.2588734      1 157.9940         1 78.06570
#> 3: 33.9640224      1 162.4152         0 67.66418
#> 4:  0.4700053      1 172.1267         0 83.22141
#> 5: 40.0000000      0 170.7479         1 82.32868
#> 6:  5.6373356      1 158.6357         0 84.88671
```

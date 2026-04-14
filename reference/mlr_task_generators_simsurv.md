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
#> 1: 5.0000000      0 195.0268         0 80.45247
#> 2: 0.6306407      1 174.0041         1 96.48540
#> 3: 5.0000000      0 180.7211         1 72.44317
#> 4: 2.5334734      1 170.5500         1 77.11821
#> 5: 0.7165469      1 169.3864         1 72.69849
#> 6: 4.3512901      1 159.2356         1 85.24149

  # generate 100 samples with exponential survival distribution and tau = 40
  gen = tgen("simsurv", dist = "exponential", gammas = NULL, maxt = 40)
  task = gen$generate(100)
  head(task)
#>     eventtime status   height treatment    weight
#>         <num>  <int>    <num>     <int>     <num>
#> 1: 14.0902730      1 163.7044         1  69.81636
#> 2:  9.5564871      1 173.8492         1  84.80034
#> 3:  1.6580893      1 175.4791         0 104.04713
#> 4:  1.0691358      1 176.7335         0  86.03244
#> 5:  6.7565146      1 171.8410         0 100.06418
#> 6:  0.4434743      1 175.9910         0  67.76009
```

# Density Task Generator for Package 'distr6'

A
[mlr3::TaskGenerator](https://mlr3.mlr-org.com/reference/TaskGenerator.html)
calling
[`distr6::distrSimulate()`](https://xoopr.github.io/distr6/reference/distrSimulate.html).
See
[`distr6::distrSimulate()`](https://xoopr.github.io/distr6/reference/distrSimulate.html)
for an explanation of the hyperparameters. See
[`distr6::listDistributions()`](https://xoopr.github.io/distr6/reference/listDistributions.html)
for the names of the available distributions.

## Dictionary

This
[TaskGenerator](https://mlr3.mlr-org.com/reference/TaskGenerator.html)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_task_generators](https://mlr3.mlr-org.com/reference/mlr_task_generators.html)
or with the associated sugar function
[tgen()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    mlr_task_generators$get("simdens")
    tgen("simdens")

## Parameters

|              |           |         |                                                                                                                                                             |
|--------------|-----------|---------|-------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Id           | Type      | Default | Levels                                                                                                                                                      |
| distribution | character | Normal  | Arcsine, Arrdist, Bernoulli, Beta, BetaNoncentral, Binomial, Categorical, Cauchy, ChiSquared, ChiSquaredNoncentral, [...](https://rdrr.io/r/base/dots.html) |
| pars         | untyped   | \-      |                                                                                                                                                             |

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
[`mlr_task_generators_simsurv`](https://mlr3proba.mlr-org.com/reference/mlr_task_generators_simsurv.md)

## Super class

[`mlr3::TaskGenerator`](https://mlr3.mlr-org.com/reference/TaskGenerator.html)
-\> `TaskGeneratorSimdens`

## Methods

### Public methods

- [`TaskGeneratorSimdens$new()`](#method-TaskGeneratorSimdens-new)

- [`TaskGeneratorSimdens$help()`](#method-TaskGeneratorSimdens-help)

- [`TaskGeneratorSimdens$clone()`](#method-TaskGeneratorSimdens-clone)

Inherited methods

- [`mlr3::TaskGenerator$format()`](https://mlr3.mlr-org.com/reference/TaskGenerator.html#method-format)
- [`mlr3::TaskGenerator$generate()`](https://mlr3.mlr-org.com/reference/TaskGenerator.html#method-generate)
- [`mlr3::TaskGenerator$print()`](https://mlr3.mlr-org.com/reference/TaskGenerator.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://rdrr.io/pkg/R6/man/R6Class.html) class.

#### Usage

    TaskGeneratorSimdens$new()

------------------------------------------------------------------------

### Method [`help()`](https://rdrr.io/r/utils/help.html)

Opens the corresponding help page referenced by field `$man`.

#### Usage

    TaskGeneratorSimdens$help()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaskGeneratorSimdens$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# generate 20 samples from a standard Normal distribution
dens_gen = tgen("simdens")
dens_gen$param_set
#> <ParamSet(2)>
#>              id    class lower upper nlevels        default  value
#>          <char>   <char> <num> <num>   <num>         <list> <list>
#> 1: distribution ParamFct    NA    NA      47         Normal [NULL]
#> 2:         pars ParamUty    NA    NA     Inf <NoDefault[0]> [NULL]

task = dens_gen$generate(20)
head(task)
#>             x
#>         <num>
#> 1: -0.7994923
#> 2: -0.0871060
#> 3:  1.2420805
#> 4:  1.1361390
#> 5: -1.1946120
#> 6: -0.4110691

# generate 50 samples from a Binomial distribution with specific parameters
dens_gen = tgen("simdens", distribution = "Bernoulli", pars = list(prob = 0.8))
task = dens_gen$generate(50)
task$data()[["x"]]
#>  [1] 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1
#> [39] 0 1 1 1 0 1 1 1 1 1 1 0
```

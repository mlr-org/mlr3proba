# Log Loss Density Measure

Calculates the cross-entropy, or logarithmic (log), loss.

## Details

The Log Loss, in the context of probabilistic predictions, is defined as
the negative log probability density function, \\f\\, evaluated at the
observed value, \\y\\, \$\$L(f, y) = -\log(f(y))\$\$

## Dictionary

This [Measure](https://mlr3.mlr-org.com/reference/Measure.html) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.html) or
with the associated sugar function
[msr()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    MeasureDensLogloss$new()
    mlr_measures$get("dens.logloss")
    msr("dens.logloss")

## Parameters

|     |         |         |              |
|-----|---------|---------|--------------|
| Id  | Type    | Default | Range        |
| eps | numeric | 1e-15   | \\\[0, 1\]\\ |

## Meta Information

- Type: `"density"`

- Range: \\\[0, \infty)\\

- Minimize: `TRUE`

- Required prediction: `pdf`

## Parameter details

- `eps` (`numeric(1)`)  
  Very small number to substitute near-zero values in order to prevent
  errors in e.g. log(0) and/or division-by-zero calculations. Default
  value is 1e-15.

## Super classes

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.html) -\>
[`mlr3proba::MeasureDens`](https://mlr3proba.mlr-org.com/reference/MeasureDens.md)
-\> `MeasureDensLogloss`

## Methods

### Public methods

- [`MeasureDensLogloss$new()`](#method-MeasureDensLogloss-new)

- [`MeasureDensLogloss$clone()`](#method-MeasureDensLogloss-clone)

Inherited methods

- [`mlr3::Measure$aggregate()`](https://mlr3.mlr-org.com/reference/Measure.html#method-aggregate)
- [`mlr3::Measure$format()`](https://mlr3.mlr-org.com/reference/Measure.html#method-format)
- [`mlr3::Measure$help()`](https://mlr3.mlr-org.com/reference/Measure.html#method-help)
- [`mlr3::Measure$print()`](https://mlr3.mlr-org.com/reference/Measure.html#method-print)
- [`mlr3::Measure$score()`](https://mlr3.mlr-org.com/reference/Measure.html#method-score)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://rdrr.io/pkg/R6/man/R6Class.html) class.

#### Usage

    MeasureDensLogloss$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureDensLogloss$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

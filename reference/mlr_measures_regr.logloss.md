# Log Loss Regression Measure

Calculates the cross-entropy, or logarithmic (log), loss.

## Details

The Log Loss, in the context of probabilistic predictions, is defined as
the negative log probability density function, \\f\\, evaluated at the
observed value, \\y\\, \$\$L(f, y) = -\log(f(y))\$\$

## Parameters

|     |         |         |              |
|-----|---------|---------|--------------|
| Id  | Type    | Default | Range        |
| eps | numeric | 1e-15   | \\\[0, 1\]\\ |

## Meta Information

- Type: `"regr"`

- Range: \\\[0, \infty)\\

- Minimize: `TRUE`

- Required prediction: `distr`

## Parameter details

- `eps` (`numeric(1)`)  
  Very small number to substitute near-zero values in order to prevent
  errors in e.g. log(0) and/or division-by-zero calculations. Default
  value is 1e-15.

## Super classes

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.html) -\>
[`mlr3::MeasureRegr`](https://mlr3.mlr-org.com/reference/MeasureRegr.html)
-\> `MeasureRegrLogloss`

## Methods

### Public methods

- [`MeasureRegrLogloss$new()`](#method-MeasureRegrLogloss-new)

- [`MeasureRegrLogloss$clone()`](#method-MeasureRegrLogloss-clone)

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

    MeasureRegrLogloss$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureRegrLogloss$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

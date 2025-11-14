# Plot for Density Tasks

Generates plots for
[TaskDens](https://mlr3proba.mlr-org.com/reference/TaskDens.md).

## Usage

``` r
# S3 method for class 'TaskDens'
autoplot(object, type = "dens", theme = theme_minimal(), ...)
```

## Arguments

- object:

  ([TaskDens](https://mlr3proba.mlr-org.com/reference/TaskDens.md)).

- type:

  (`character(1)`)  
  Type of the plot. Available choices:

  - `"dens"`: histogram density estimator (default) with
    [`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html).

  - `"freq"`: histogram frequency plot with
    [`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html).

  - `"overlay"`: histogram with overlaid density plot with
    [`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)
    and
    [`ggplot2::geom_density()`](https://ggplot2.tidyverse.org/reference/geom_density.html).

  - `"freqpoly"`: frequency polygon plot with
    [ggplot2::geom_freqpoly](https://ggplot2.tidyverse.org/reference/geom_histogram.html).

- theme:

  ([`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html))  
  The
  [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
  is applied by default to all plots.

- ...:

  (`any`)  
  Additional arguments, possibly passed down to the underlying plot
  functions.

## Value

[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Examples

``` r
library(ggplot2)

task = tsk("precip")
task$head()
#>    precip
#>     <num>
#> 1:   67.0
#> 2:   54.7
#> 3:    7.0
#> 4:   48.5
#> 5:   14.0
#> 6:   17.2

autoplot(task, bins = 15)
#> Warning: Arguments in `...` must be used.
#> ✖ Problematic argument:
#> • bins = 15
#> ℹ Did you misspell an argument name?

autoplot(task, type = "freq", bins = 15)
#> Warning: Arguments in `...` must be used.
#> ✖ Problematic argument:
#> • bins = 15
#> ℹ Did you misspell an argument name?

autoplot(task, type = "overlay", bins = 15)
#> Warning: Arguments in `...` must be used.
#> ✖ Problematic argument:
#> • bins = 15
#> ℹ Did you misspell an argument name?

autoplot(task, type = "freqpoly", bins = 15)
#> Warning: Arguments in `...` must be used.
#> ✖ Problematic argument:
#> • bins = 15
#> ℹ Did you misspell an argument name?
```

# Plot for Survival Tasks

Generates plots for
[TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md),
depending on argument `type`:

- `"target"`: Calls
  [`GGally::ggsurv()`](https://ggobi.github.io/ggally/reference/ggsurv.html)
  on a
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
  object. This computes the **Kaplan-Meier survival curve** for the
  observations if this task.

- `"duo"`: Passes data and additional arguments down to
  [`GGally::ggduo()`](https://ggobi.github.io/ggally/reference/ggduo.html).
  `columnsX` is target, `columnsY` is features.

- `"pairs"`: Passes data and additional arguments down to
  [`GGally::ggpairs()`](https://ggobi.github.io/ggally/reference/ggpairs.html).
  Color is set to target column.

## Usage

``` r
# S3 method for class 'TaskSurv'
autoplot(
  object,
  type = "target",
  theme = theme_minimal(),
  reverse = FALSE,
  ...
)
```

## Arguments

- object:

  ([TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md)).

- type:

  (`character(1)`)  
  Type of the plot. See Description for available choices.

- theme:

  ([`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html))  
  The
  [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
  is applied by default to all plots.

- reverse:

  ([`logical()`](https://rdrr.io/r/base/logical.html))  
  If `TRUE` and `type = 'target'`, it plots the Kaplan-Meier curve of
  the censoring distribution. Default is `FALSE`.

- ...:

  (`any`)  
  Additional arguments. `rhs` is passed down to `$formula` of
  [TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md) for
  stratification for type `"target"`. Other arguments are passed to the
  respective underlying plot functions.

## Value

[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Examples

``` r
library(ggplot2)

task = tsk("lung")
task$head()
#>     time status   age meal.cal pat.karno ph.ecog ph.karno    sex wt.loss
#>    <int>  <int> <int>    <int>     <int>   <int>    <int> <fctr>   <int>
#> 1:   455      1    68     1225        90       0       90      m      15
#> 2:   210      1    57     1150        60       1       90      m      11
#> 3:  1022      0    74      513        80       1       50      m       0
#> 4:   310      1    68      384        60       2       70      f      10
#> 5:   361      1    71      538        80       2       60      f       1
#> 6:   218      1    53      825        80       1       70      m      16

autoplot(task) # KM

autoplot(task) # KM of the censoring distribution

autoplot(task, rhs = "sex")

autoplot(task, type = "duo")
#> Warning: pseudoinverse used at -0.005
#> Warning: neighborhood radius 1.005
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 1.01
#> Warning: pseudoinverse used at -0.005
#> Warning: neighborhood radius 1.005
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 1.01
#> Warning: pseudoinverse used at -0.005
#> Warning: neighborhood radius 1.005
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 1.01
#> Warning: pseudoinverse used at -0.005
#> Warning: neighborhood radius 1.005
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 1.01
#> Warning: pseudoinverse used at -0.005
#> Warning: neighborhood radius 1.005
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 1.01
#> Warning: pseudoinverse used at -0.005
#> Warning: neighborhood radius 1.005
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 1.01
#> Warning: pseudoinverse used at -0.005
#> Warning: neighborhood radius 1.005
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 1.01
#> Warning: pseudoinverse used at -0.005
#> Warning: neighborhood radius 1.005
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 1.01
#> Warning: pseudoinverse used at -0.005
#> Warning: neighborhood radius 1.005
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 1.01
#> Warning: pseudoinverse used at -0.005
#> Warning: neighborhood radius 1.005
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 1.01
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
#> Warning: pseudoinverse used at -0.005
#> Warning: neighborhood radius 1.005
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 1.01
#> Warning: pseudoinverse used at -0.005
#> Warning: neighborhood radius 1.005
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 1.01
```

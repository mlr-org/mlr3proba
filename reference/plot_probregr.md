# Visualise probabilistic regression distribution predictions

Plots probability density functions from `n` predicted probability
distributions.

## Usage

``` r
plot_probregr(
  p,
  n,
  type = c("point", "line", "both", "none"),
  which_plot = c("random", "top"),
  rm_zero = TRUE,
  ...
)
```

## Arguments

- p:

  ([PredictionRegr](https://mlr3.mlr-org.com/reference/PredictionRegr.html))  
  With at least column `distr`.

- n:

  (`integer(1)`)  
  Number of predictions to plot.

- type:

  (`character(1)`)  
  One of `"point"` (default), `"line"`, `"both"`, `"none"`.

- which_plot:

  (`character(1)`)  
  One of `"random"` (default) or `"top"`. See details.

- rm_zero:

  (`logical(1)`)  
  If `TRUE` (default) does not plot points where `f(x) = 0`.

- ...:

  Unused

## Details

`type`:

- `"point"` (default) - Truth plotted as point (truth,
  predicted_pdf(truth))

- `"line"` - Truth plotted as vertical line intercepting x-axis at the
  truth.

- `"both"` - Plots both the above options.

- `"none"` - Truth not plotted (default if `p$truth` is missing).

`which_plot`:

- "random"`(default) - Random selection of`n\` distributions are
  plotted.

- "top"`- Top`n\` distributions are plotted.

It is unlikely the plot will be interpretable when `n >> 5`.

## Examples

``` r
if (FALSE) { # \dontrun{
library(mlr3verse)
task = tsk("boston_housing")
pipe = as_learner(ppl("probregr", lrn("regr.ranger"), dist = "Normal"))
p = pipe$train(task)$predict(task)
plot_probregr(p, 10, "point", "top")
} # }
```

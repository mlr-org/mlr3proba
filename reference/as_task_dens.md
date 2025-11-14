# Convert to a Density Task

Convert object to a density task
([TaskDens](https://mlr3proba.mlr-org.com/reference/TaskDens.md)).

## Usage

``` r
as_task_dens(x, ...)

# S3 method for class 'TaskDens'
as_task_dens(x, clone = FALSE, ...)

# S3 method for class 'data.frame'
as_task_dens(x, id = deparse(substitute(x)), ...)

# S3 method for class 'DataBackend'
as_task_dens(x, id = deparse(substitute(x)), ...)
```

## Arguments

- x:

  (`any`)  
  Object to convert, e.g. a
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html).

- ...:

  (`any`)  
  Additional arguments.

- clone:

  (`logical(1)`)  
  If `TRUE`, ensures that the returned object is not the same as the
  input `x`.

- id:

  (`character(1)`)  
  Id for the new task. Defaults to the (deparsed and substituted) name
  of `x`.

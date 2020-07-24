#' @title PipeOpTransformer
#'
#' @usage NULL
#' @format Abstract [R6Class][R6::R6Class] inheriting from [PipeOp].
#'
#' @description
#' Parent class for [PipeOp]s that transform [Task][mlr3::Task] and [Prediction][mlr3::Prediction]
#' objects to different types.
#'
#' @section Input and Output Channels:
#' Determined by child classes.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Internals:
#' The commonality of methods using [PipeOpTransformer] is that they take a [Task][mlr3::Task]
#' or [Prediction][mlr3::Prediction] of one type (e.g. regr or classif) and transform it to
#' another type.
#'
#' @section Fields:
#' Only fields inherited from [PipeOp].
#'
#' @family PipeOps
#' @family Transformers
#' @export
PipeOpTransformer = R6Class("PipeOpTransformer",
  inherit = PipeOp,
  public = list(
    initialize = function(id, param_set = ParamSet$new(), param_vals = list(),
                          packages = character(), input = data.table(),
                          output = data.table()) {

      super$initialize(id = id, param_set = param_set, param_vals = param_vals, packages = packages,
                       input = input, output = output)
    }
  ),

  private = list(
    .train = function(inputs) {
      list(private$.transform(inputs))
    },

    .predict = function(inputs) {
      list(private$.transform(inputs))
    },

    .transform = function(...) stop("Abstract.")
  )
)


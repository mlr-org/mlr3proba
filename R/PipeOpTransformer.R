#' @title PipeOpTransformer
#'
#' @usage NULL
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Parent class for [`PipeOp`]s that transform task objects top different types.
#'
#' @section Input and Output Channels:
#' [`PipeOpTransformer`] has one input and output channel named `"input"` and `"output"`.
#' In training and testing these expect and produce [mlr3::Task] objects with the type depending on
#' the transformers.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Internals:
#' The commonality of methods using [`PipeOpTransformer`] is that they take a [mlr3::Task] of
#' one class and transform it to another class. This usually involves transformation of the data,
#' which can be controlled via parameters.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @family Transformers
#' @export
PipeOpTransformer = R6Class("PipeOpTransformer",
  inherit = PipeOp,
  public = list(
    initialize = function(id, param_set = ParamSet$new(), param_vals = list(),
                          packages = character(0), input = data.table(),
                          output = data.table()) {

      super$initialize(id = id, param_set = param_set, param_vals = param_vals, packages = packages,
                       input = input, output = output)
    },

    train_internal = function(inputs) {
      list(private$.transform(inputs))
    },

    predict_internal = function(inputs) {
      list(private$.transform(inputs))
    }
  ),

  private = list(
    .transform = function(...) stop("Abstract.")
  )
)


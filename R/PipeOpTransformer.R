#' @title PipeOpTransformer
#' @template param_pipelines
#' @template param_param_set
#' @template param_packages
#' @template param_input_output
#'
#' @description
#' Parent class for [PipeOp][mlr3pipelines::PipeOp]s that transform [Task][mlr3::Task] and [Prediction][mlr3::Prediction]
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
#' @family PipeOps
#' @family Transformers
#' @export
PipeOpTransformer = R6Class("PipeOpTransformer",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
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


#' @title PipeOpTaskTransformer
#' @template param_pipelines
#' @template param_param_set
#' @template param_packages
#' @template param_input_output
#'
#' @description
#' Parent class for [PipeOp][mlr3pipelines::PipeOp]s that transform task objects top different types.
#'
#' @section Input and Output Channels:
#' [`PipeOpTaskTransformer`] has one input and output channel named `"input"` and `"output"`.
#' In training and testing these expect and produce [mlr3::Task] objects with the type depending on
#' the transformers.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Internals:
#' The commonality of methods using [`PipeOpTaskTransformer`] is that they take a [mlr3::Task] of
#' one class and transform it to another class. This usually involves transformation of the data,
#' which can be controlled via parameters.
#'
#' @family PipeOps
#' @family Transformers
#' @include PipeOpTransformer.R
#' @export
PipeOpTaskTransformer = R6Class("PipeOpTaskTransformer",
  inherit = PipeOpTransformer,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, param_set = ParamSet$new(), param_vals = list(),
                          packages = character(0), input, output) {

      super$initialize(id = id,
                       param_set = param_set,
                       param_vals = param_vals,
                       packages = packages,
                       input = input,
                       output = output
                       )
    }
  ),

  private = list(
    .predict = function(inputs) {
      list(inputs[[1]]$clone(deep = TRUE))
    }
  )
)


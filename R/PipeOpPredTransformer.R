#' @title PipeOpPredTransformer
#'
#' @template param_pipelines
#' @template param_param_set
#' @template param_packages
#' @template param_input_output
#'
#' @description
#' Parent class for [PipeOp][mlr3pipelines::PipeOp]s that transform [Prediction][mlr3::Prediction]
#' objects to different types.
#'
#' @section Input and Output Channels:
#' [`PipeOpPredTransformer`] has one input and output channel named `"input"` and `"output"`.
#' In training and testing these expect and produce [mlr3::Prediction] objects with the type
#' depending on the transformers.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements
#'
#' * `inpredtypes`: Predict types in the input prediction object during training.
#' * `outpredtypes` : Predict types in the input prediction object during prediction, checked
#' against `inpredtypes`.
#'
#' @section Internals:
#' Classes inheriting from [`PipeOpPredTransformer`] transform [Prediction][mlr3::Prediction]
#' objects from one class (e.g. regr, classif) to another.
#'
#' @family PipeOps
#' @family Transformers
#' @include PipeOpTransformer.R
#' @export
PipeOpPredTransformer = R6Class("PipeOpPredTransformer",
  inherit = PipeOpTransformer,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, param_set = ParamSet$new(), param_vals = list(),
                          packages = character(0), input = data.table(), output = data.table()) {
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
    .train = function(inputs) {
      list(NULL)
    }
  )
)


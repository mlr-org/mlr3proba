#' @title PipeOpPredSurvRegr
#' @name mlr_pipeops_trafopred_survregr
#' @template param_pipelines
#'
#' @description
#' Transform [PredictionSurv] to [PredictionRegr].
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [PipeOpPredTransformer].
#'
#' The output is the input [PredictionSurv] transformed to a [PredictionRegr]. Censoring is ignored.
#' `crank` and `lp` predictions are also ignored.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [PipeOpPredTransformer].
#'
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#' library(mlr3)
#' library(mlr3pipelines)
#' library(survival)
#'
#' # simple example
#' pred = PredictionSurv$new(row_ids = 1:10, truth = Surv(1:10, rbinom(10, 1, 0.5)),
#'    response = 1:10)
#' po = po("trafopred_survregr")
#' new_pred = po$predict(list(pred = pred))[[1]]
#' print(new_pred)
#' }
#' }
#' @family PipeOps
#' @family Transformation PipeOps
#' @include PipeOpPredTransformer.R
#' @export
PipeOpPredSurvRegr = R6Class("PipeOpPredSurvRegr",
  inherit = PipeOpPredTransformer,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "trafopred_survregr") {
      super$initialize(id = id,
                       input = data.table(name = "input", train = "NULL", predict = "PredictionSurv"),
                       output = data.table(name = "output", train = "NULL", predict = "PredictionRegr")
      )
    }
  ),

  private = list(
    .transform = function(input) {
      input = input[[1]]
      PredictionRegr$new(row_ids = input$row_ids, truth = input$truth[, 1L],
                         distr = input$distr, response = input$response)
    }
  )
)


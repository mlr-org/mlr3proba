#' @title PipeOpPredClassifSurv
#' @name mlr_pipeops_trafopred_classifsurv
#'
#' @description
#' Transform [PredictionClassif] to [PredictionSurv].
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [PipeOpPredTransformer].
#'
#' The output is the input [PredictionClassif] transformed to a [PredictionSurv].
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3pipelines)
#'
#'   task = tsk("rats")
#'
#'   if (requireNamespace("mlr3learners", quietly = TRUE)) {
#'     po_tasktoclassif = po("trafotask_survclassif")
#'     po_tasktoclassif$train(list(task))
#'     task_classif = po_tasktoclassif$predict(list(task))[[1]]
#'     trafo_data = po_tasktoclassif$predict(list(task))[[2]]
#'
#'     learner = lrn("classif.log_reg", predict_type = "prob")
#'     pred = learner$train(task_classif)$predict_newdata(trafo_data)
#'
#'     po_predtosurv = po("trafopred_classifsurv")
#'     po_predtosurv$train(list(pred, trafo_data))
#'     po_predtosurv$predict(list(pred, trafo_data))
#'
#'   }
#' }
#' }
#' @family PipeOps
#' @family Transformation PipeOps
#' @include PipeOpPredTransformer.R
#' @export
PipeOpPredClassifSurv = R6Class(
  "PipeOpPredClassifSurv",
  inherit = mlr3pipelines::PipeOp,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param id (character(1))\cr
    #' Identifier of the resulting object.
    initialize = function(id = "trafopred_classifsurv") {
      super$initialize(
        id = id,
        input = data.table::data.table(
          name = c("input", "transformed_data"),
          train = c("NULL", "data.frame"),
          predict = c("PredictionClassif", "data.frame")
        ),
        output = data.table::data.table(
          name = "output",
          train = "NULL",
          predict = "PredictionSurv"
        )
      )
    }
  ),

  private = list(
    .predict = function(input) {
      pred = input[[1]]
      data = input[[2]]
      data = cbind(data, pred = pred$prob[, 2])

      ## convert hazards to surv as prod(1 - h(t))
      surv = t(vapply(unique(data$id), function(id) {
        x = cumprod((data[data$id == id, "pred"]))
        x
      }, numeric(sum(data$id == 1))))

      pred_list = list()
      unique_end_times = sort(unique(data$tend))
      ## coerce to distribution and crank
      pred_list = .surv_return(unique_end_times, surv = surv)

      # select the real tend values by only selecting the last row of each id
      # basically a slightly more complex unique()
      real_tend = data$time2[seq_len(nrow(data)) %% sum(data$id == 1) == 0]

      # select last row for every id
      data = as.data.table(data)
      id = NULL # to fix note
      data = data[, .SD[.N, list(ped_status)], by = id]

      ## create prediction object
      p = PredictionSurv$new(
        row_ids = seq(nrow(data)),
        crank = pred_list$crank, distr = pred_list$distr,
        truth = Surv(real_tend, data[["ped_status"]] |> as.numeric()))

      list(p)
    },

    .train = function(input) {
      self$state = list()
      list(input)
    }
  )
)

register_pipeop("trafopred_classifsurv", PipeOpPredClassifSurv)

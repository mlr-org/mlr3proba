#' @title PipeOpPredClassifSurv
#' @name mlr_pipeops_trafopred_classifsurv
#'
#' @description
#' Transform [PredictionClassif] to [PredictionSurv] by converting
#' event probabilities of a pseudo status variable (discrete time hazards)
#' to survival probabilities.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [PipeOp][mlr3pipelines::PipeOp].
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
#'     library(mlr3learners)
#'     po_tasktoclassif = po("trafotask_survclassif")
#'     po_tasktoclassif$train(list(task))
#'     task_classif = po_tasktoclassif$predict(list(task))[[1]]
#'     trafo_data = po_tasktoclassif$predict(list(task))[[2]]
#'
#'     learner = lrn("classif.log_reg", predict_type = "prob")
#'     learner$train(task_classif)
#'     pred = learner$predict(task_classif)
#'
#'     po_predtosurv = po("trafopred_classifsurv")
#'     po_predtosurv$train(list(pred, trafo_data))
#'     po_predtosurv$predict(list(pred, trafo_data))
#'   }
#' }
#' }
#' @family PipeOps
#' @family Transformation PipeOps
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
          train = c("NULL", "data.table"),
          predict = c("PredictionClassif", "data.table")
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
      assert_true(!is.null(pred$prob))
      data = cbind(data, pred = pred$prob[, "0"])

      ## convert hazards to surv as prod(1 - h(t))
      rows_per_id = nrow(data)/length(unique(data$id))
      surv = t(vapply(unique(data$id), function(unique_id) {
        x = cumprod((data[data$id == unique_id, ][["pred"]]))
        x
      }, numeric(rows_per_id)))

      pred_list = list()
      unique_end_times = sort(unique(data$tend))
      ## coerce to distribution and crank
      pred_list = .surv_return(times = unique_end_times, surv = surv)

      # select the real tend values by only selecting the last row of each id
      # basically a slightly more complex unique()
      real_tend = data$time2[seq_len(nrow(data)) %% rows_per_id == 0]

      # select last row for every id
      data = as.data.table(data)
      id = ped_status = NULL # to fix note
      data = data[, .SD[.N, list(ped_status)], by = id]

      ## create prediction object
      p = PredictionSurv$new(
        row_ids = seq_row(data),
        crank = pred_list$crank, distr = pred_list$distr,
        truth = Surv(real_tend, as.integer(as.character(data$ped_status))))

      list(p)
    },

    .train = function(input) {
      self$state = list()
      list(input)
    }
  )
)

register_pipeop("trafopred_classifsurv", PipeOpPredClassifSurv)

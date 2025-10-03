#' @title PipeOpPredClassifSurvDiscTime
#' @name mlr_pipeops_trafopred_classifsurv_disctime
#'
#' @description
#' Transform [PredictionClassif][mlr3::PredictionClassif] to [PredictionSurv] by converting
#' event probabilities of a pseudo status variable (discrete time hazards)
#' to survival probabilities using the product rule (Tutz et al. 2016):
#'
#' \deqn{P_k = p_k\cdot ... \cdot p_1}
#'
#' Where:
#' - We assume that continuous time is divided into time intervals
#' \eqn{[0, t_1), [t_1, t_2), ..., [t_n, \infty)}
#' - \eqn{P_k = P(T > t_k)} is the survival probability at time \eqn{t_k}
#' - \eqn{h_k} is the discrete-time hazard (classifier prediction), i.e. the
#' conditional probability for an event in the \eqn{k}-interval.
#' - \eqn{p_k = 1 - h_k = P(T \ge t_k | T \ge t_{k-1})}
#'
#' @section Dictionary:
#' This [PipeOp][mlr3pipelines::PipeOp] can be instantiated via the
#' [dictionary][mlr3misc::Dictionary] [mlr3pipelines::mlr_pipeops]
#' or with the associated sugar function [mlr3pipelines::po()]:
#' ```
#' PipeOpPredClassifSurvDiscTime$new()
#' mlr_pipeops$get("trafopred_classifsurv_disctime")
#' po("trafopred_classifsurv_disctime")
#' ```
#'
#' @section Input and Output Channels:
#' The input is a [PredictionClassif][mlr3::PredictionClassif] and a
#' [data.table][data.table::data.table] with the transformed data both generated
#' by [PipeOpTaskSurvClassifDiscTime].
#' The output is the input [PredictionClassif][mlr3::PredictionClassif]
#' transformed to a [PredictionSurv].
#' Only works during prediction phase.
#'
#' @references
#' `r format_bib("tutz_2016")`
#' @seealso [pipeline_survtoclassif_disctime]
#' @family Transformation PipeOps
#' @export
PipeOpPredClassifSurvDiscTime = R6Class("PipeOpPredClassifSurvDiscTime",
  inherit = mlr3pipelines::PipeOp,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param id (character(1))\cr
    #' Identifier of the resulting object.
    initialize = function(id = "trafopred_classifsurv_disctime") {
      super$initialize(
        id = id,
        input = data.table(
          name = c("input", "transformed_data"),
          train = c("NULL", "data.table"),
          predict = c("PredictionClassif", "data.table")
        ),
        output = data.table(
          name = "output",
          train = "NULL",
          predict = "PredictionSurv"
        )
      )
    }
  ),

  active = list(
    #' @field predict_type (`character(1)`)\cr
    #' Returns the active predict type of this PipeOp, which is `"crank"`
    predict_type = function(rhs) {
      assert_ro_binding(rhs)
      "crank"
    }
  ),

  private = list(
    .predict = function(input) {
      pred = input[[1]]
      data = input[[2]]
      assert_true(!is.null(pred$prob))
      # probability of having the event (1) in each respective interval
      # is the discrete-time hazard
      data = cbind(data, dt_hazard = pred$prob[, "1"])

      # From theory, convert hazards to surv as prod(1 - h(t))
      rows_per_id = nrow(data) / length(unique(data$id))
      surv = t(vapply(unique(data$id), function(unique_id) {
        cumprod(1 - data[data$id == unique_id, ][["dt_hazard"]])
      }, numeric(rows_per_id)))

      unique_end_times = sort(unique(data$tend))
      # coerce to distribution and crank
      pred_list = surv_return(times = unique_end_times, surv = surv)

      # select the real tend values by only selecting the last row of each id
      # basically a slightly more complex unique()
      real_tend = data$obs_times[seq_len(nrow(data)) %% rows_per_id == 0]

      ids = unique(data$id)
      # select last row for every id => observed times
      id = disc_status = NULL # to fix note
      data = data[, .SD[.N, list(disc_status)], by = id]

      # create prediction object
      p = PredictionSurv$new(
        row_ids = ids,
        crank = pred_list$crank, distr = pred_list$distr,
        truth = Surv(real_tend, as.integer(as.character(data$disc_status)))
      )

      list(p)
    },

    .train = function(input) {
      self$state = list()
      list(input)
    }
  )
)

register_pipeop("trafopred_classifsurv_disctime", PipeOpPredClassifSurvDiscTime)

#' @title Prediction Object for Competing Risks
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerCompRisks].
#'
#' The `task_type` is set to `"cmprsk"`.
#'
#' For accessing survival and hazard functions, as well as other complex methods
#' from a [LearnerCompRisks] object is not possible atm.
#'
#' @family Prediction
#' @examples
#' library(mlr3)
#' task = tsk("pbc")
#' learner = lrn("cmprsk.aalen")
#' part = partition(task)
#' p = learner$train(task, part$train)$predict(task, part$test)
#' p
#'
#' # CIF list: 1 matrix (obs x times) per competing event
#' names(p$cif) # competing events
#' # CIF matrix for competing event 1 (first 5 test observations and 20 time points)
#' p$cif[["1"]][1:5, 1:20]
#' # CIF matrix for competing event 2 (first 5 test observations and 20 time points)
#' p$cif[["2"]][1:5, 1:20]
#'
#' # data.table conversion
#' tab = as.data.table(p)
#' tab$CIF[[1]] # for first test observation, list of CIF vectors
#'
#' @export
PredictionCompRisks = R6Class("PredictionCompRisks",
  inherit = Prediction,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @details
    #' The `cif` input currently is a list of CIF matrices.
    #' In the future the `cif` field should coerce this to a (TODO) CIF container
    #' to get all survival and hazard functions using interpolation.
    #'
    #' @param task ([TaskSurv])\cr
    #'   Task, used to extract defaults for `row_ids` and `truth`.
    #'
    #' @param row_ids (`integer()`)\cr
    #'   Row ids of the predicted observations, i.e. the row ids of the test set.
    #'
    #' @param truth (`survival::Surv()`)\cr
    #'   True (observed) response.
    #'
    #' @param cif (`list()`)\cr
    #'   A `list` of two or more `matrix` objects.
    #'   Each matrix represents a different competing event and it stores the
    #'   **Cumulative Incidence function** for each test observation.
    #'   In each matrix, rows represent observations and columns time points.
    #'   The names of the `list` must correspond to the competing event names
    #'   (`task$cmp_events`).
    #'
    #' @param check (`logical(1)`)\cr
    #'   If `TRUE`, performs argument checks and predict type conversions.
    initialize = function(task = NULL, row_ids = task$row_ids, truth = task$truth(),
                          cif = NULL, check = TRUE) {
      pdata = list(row_ids = row_ids, truth = truth, cif = cif)
      pdata = discard(pdata, is.null)
      class(pdata) = c("PredictionDataCompRisks", "PredictionData")

      if (check) {
        pdata = check_prediction_data(pdata)
      }

      self$task_type = "cmprsk"
      self$man = "mlr3proba::PredictionCompRisks"
      self$data = pdata
      self$predict_types = intersect("cif", names(pdata))
    }
  ),

  active = list(
    #' @field truth (`Surv`)\cr
    #' True (observed) outcome.
    truth = function() {
      self$data$truth
    },

    #' @field cif (`list()`)\cr
    #' Access the stored CIFs.
    cif = function() {
      # TODO: convert to `survdistr` object with methods for easier conversion and interpolation
      self$data$cif
    }
  )
)

#' @export
as.data.table.PredictionCompRisks = function(x, ...) {
  tab = as.data.table(x$data["row_ids"])
  tab$time = x$data$truth[, 1L]
  tab$event = x$data$truth[, 2L]
  n_obs = length(x$row_ids)

  if ("cif" %in% x$predict_types && n_obs > 0) {
    tab$CIF = lapply(1:n_obs, function(i) {
      # we use a list since there is a possibility that each CIF matrix has
      # different number of time points (columns) per competing risk
      cif_list = lapply(x$cif, function(mat) mat[i, , drop = TRUE])
      names(cif_list) = names(x$cif) # preserve the competing risk names/ids
      cif_list
    })
  }

  setcolorder(tab, c("row_ids", "time", "event"))[]
}

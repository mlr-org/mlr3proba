#' @title Prediction Object for Density
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerDens].
#'
#' The `task_type` is set to `"dens"`.
#'
#' @family Prediction
#' @export
#' @examples
#' library(mlr3)
#' task = mlr_tasks$get("precip")
#' learner = mlr_learners$get("dens.hist")
#' p = learner$train(task)$predict(task)
#' head(as.data.table(p))
PredictionDens = R6Class("PredictionDens",
  inherit = Prediction,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([TaskSurv])\cr
    #'   Task, used to extract defaults for `row_ids`.
    #'
    #' @param row_ids (`integer()`)\cr
    #'   Row ids of the predicted observations, i.e. the row ids of the test set.
    #'
    #' @param pdf (`numeric()`)\cr
    #'   Numeric vector of estimated probability density function, evaluated at values in test set.
    #'   One element for each observation in the test set.
    #'
    #' @param cdf (`numeric()`)\cr
    #'   Numeric vector of estimated cumulative distribution function, evaluated at values in test
    #'   set. One element for each observation in the test set.
    #'
    #' @param distr ([Distribution][distr6::Distribution])\cr
    #'   [Distribution][distr6::Distribution] from \CRANpkg{distr6}.
    #'   The distribution from which `pdf` and `cdf` are derived.
    #'
    #' @param check (`logical(1)`)\cr
    #'   If `TRUE`, performs argument checks and predict type conversions.
    initialize = function(task = NULL, row_ids = task$row_ids, pdf = NULL,
                          cdf = NULL, distr = NULL, check = TRUE) {
      pdata = list(row_ids = row_ids, pdf = pdf, cdf = cdf, distr = distr)
      pdata = discard(pdata, is.null)
      class(pdata) = c("PredictionDataDens", "PredictionData")

      if (check) {
        pdata = check_prediction_data(pdata)
      }

      self$task_type = "dens"
      self$man = "mlr3proba::PredictionDens"
      self$data = pdata
      self$predict_types = intersect(c("pdf", "cdf", "distr"), names(pdata))
    }
  ),

  active = list(
    #' @field pdf (`numeric()`)\cr
    #' Access the stored predicted probability density function.
    pdf = function() {
      self$data$pdf %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field cdf (`numeric()`)\cr
    #' Access the stored predicted cumulative distribution function.
    cdf = function() {
      self$data$cdf %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field distr ([Distribution][distr6::Distribution])\cr
    #' Access the stored estimated distribution.
    distr = function() {
      self$data$distr %??% NA_real_
    }
  )
)


#' @export
as.data.table.PredictionDens = function(x, ...) { # nolint
  tab = as.data.table(x$data[c("row_ids", "pdf", "cdf")])
  setnames(tab, "row_ids", "row_id")
  if ("distr" %in% x$predict_types) {
    tab$distr = list(list(x$distr))
  }
  tab
}

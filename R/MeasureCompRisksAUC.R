#' @template cmprsk_measure
#' @templateVar title Blanche's AUC
#' @templateVar fullname MeasureCompRisksAUC
#'
#' @aliases MeasureCompRisksAUC mlr_measures_cmprsk.auc
#'
#' @description
#' Calculates the cause-specific ROC-AUC(t) at a **specific time point**,
#' see Blanche et al. (2013).
#' Can also return the mean AUC(t) over all competing causes.
#'
#' @details
#' Calls [riskRegression::Score()] with:
#' - `metric = "auc"`
#' - `cens.method = "ipcw"`
#' - `cens.model = "km"`
#'
#' Note that the IPC weights (estimated via the Kaplan-Meier) are calculated
#' using the test data.
#'
#' @section Parameter details:
#' - `cause` (`numeric(1)`)\cr
#'  Integer number indicating which cause to use (Default: `1`).
#'  If `"mean"`, then the mean AUC(t) over all causes is returned.
#' - `time_horizon` (`numeric(1)`)\cr
#'  Single time point at which to return the score.
#'  If `NULL`, we issue a warning and the median time from the test set is used.
#'
#' @references
#' `r format_bib("blanche_2013")`
#'
#' @examplesIf mlr3misc::require_namespaces(c("riskRegression"), quietly = TRUE)
#' t = tsk("pbc")
#' l = lrn("cmprsk.aalen")
#' p = l$train(t)$predict(t)
#'
#' p$score(msr("cmprsk.auc", time_horizon = 42))
#'
#' @export
MeasureCompRisksAUC = R6Class(
  "MeasureCompRisksAUC",
  inherit = MeasureCompRisks,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        cause = p_int(lower = 1, default = 1, special_vals = list("mean")),
        time_horizon = p_dbl(lower = 0, default = NULL, special_vals = list(NULL))
      )

      param_set$set_values(cause = 1)

      super$initialize(
        id = "cmprsk.auc",
        param_set = param_set,
        range = c(0, 1),
        minimize = FALSE,
        #properties = "requires_task", (only if we want `cen.model = cox`)
        packages = "riskRegression",
        label = "Blanche's Time-dependent IPCW ROC-AUC score",
        man = "mlr3proba::mlr_measures_cmprsk.auc"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, ...) {
      pv = self$param_set$values

      # data with (time, event) columns for IPCW calculation
      # uses test set data as it needs to match predicted CIF rows/observations
      data = data.table(time = prediction$truth[, 1L], event = prediction$truth[, 2L])
      lhs = "Hist(time, event)"
      form = formulate(lhs, rhs = "1", env = getNamespace("prodlim"))

      # single time point for AUC or median time
      if (is.null(pv$time)) {
        warning("No time horizon specified. We use median time from the test set")
        time_horizon = median(data$time)
      } else {
        time_horizon = assert_number(pv$time_horizon, lower = 0, finite = TRUE, na.ok = FALSE)
      }

      # list of predicted CIF matrices
      cif = prediction$cif

      cause = pv$cause
      if (test_integerish(cause)) {
        # check if cause exists
        if (cause %nin% names(cif)) {
          stopf("Given cause (%i) is not included in the CIF causes", cause)
        }

        # get cause-specific CIF
        cif_mat = cif[[as.character(cause)]]

        # get CIF on the time horizon
        mat = interpolate_cif(cif_mat, new_times = time_horizon)

        # calculate AUC(t) score
        res = riskRegression_score(
          mat_list = list(mat),
          metric = "auc",
          data = data,
          formula = form,
          times = time_horizon,
          cause = cause
        )

        times = NULL # fix: no global binding
        res$AUC$score[times == time_horizon][["AUC"]]
      } else {
        # iterate through cause-specific CIFs, get AUC(t), return the mean
        AUCs = sapply(names(cif), function(cause) {
          # get cause-specific CIF
          cif_mat = cif[[cause]]

          # get CIF on the time horizon
          mat = interpolate_cif(cif_mat, new_times = time_horizon)

          # calculate AUC(t) score
          res = riskRegression_score(
            mat_list = list(mat),
            metric = "auc",
            data = data,
            formula = form,
            times = time_horizon,
            cause = cause
          )

          times = NULL # fix: no global binding
          res$AUC$score[times == time_horizon][["AUC"]]
        })

        # return mean (weighted?)
        mean(AUCs)
      }
    }
  )
)

register_measure("cmprsk.auc", MeasureCompRisksAUC)

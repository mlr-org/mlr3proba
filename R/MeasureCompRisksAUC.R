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
#' Notes on the `riskRegression` implementation:
#' 1. IPCW weights are estimated using the **test data only**.
#' 2. No extrapolation is supported: if `time_horizon` exceeds the maximum observed
#' time on the test data, an error is thrown.
#'
#' @section Parameter details:
#' - `cause` (`numeric(1)`)\cr
#'  Integer number indicating which cause to use (Default: `1`).
#'  If `"mean"`, then the mean AUC(t) over all causes is returned.
#' - `time_horizon` (`numeric(1)`)\cr
#'  Single time point at which to return the score.
#'  If `NULL`, the **median time point** from the test set is used.
#'
#' @references
#' `r format_bib("blanche_2013")`
#'
#' @examplesIf mlr3misc::require_namespaces("riskRegression", quietly = TRUE)
#' task = tsk("pbc")
#' learner = lrn("cmprsk.aalen")
#' p = learner$train(task)$predict(task)
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

      # Prepare test set data (for IPCW)
      # uses test set observations as it needs to match exactly the number of
      # rows (observations) in the predicted CIF matrix
      data = data.table(
        time = prediction$truth[, 1L],
        event = prediction$truth[, 2L]
      )
      form = formulate(lhs = "Hist(time, event)", rhs = "1", env = getNamespace("prodlim"))

      # Define evaluation time (single time point for AUC)
      time_horizon = if (is.null(pv$time_horizon)) {
        median(data$time)
      } else {
        assert_number(pv$time_horizon, lower = 0, finite = TRUE, na.ok = FALSE)
      }

      # list of predicted CIF matrices
      cif = prediction$cif

      cause = pv$cause
      if (test_int(cause)) {
        # check if cause exists
        if (cause %nin% names(cif)) {
          stopf("Invalid cause. Use one of: %s", paste(names(cif), collapse = ", "))
        }

        # get cause-specific CIF
        cif_mat = cif[[as.character(cause)]]

        # get CIF on the time horizon
        mat = .interp_cif(cif_mat, eval_times = time_horizon)

        # calculate AUC(t) score
        res = .riskRegr_score(
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
          mat = .interp_cif(cif_mat, eval_times = time_horizon)

          # calculate AUC(t) score
          res = .riskRegr_score(
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

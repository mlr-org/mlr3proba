#' @template cmprsk_measure
#' @templateVar title Blanche's AUC
#' @templateVar fullname MeasureCompRisksAUC
#'
#' @aliases MeasureCompRisksAUC mlr_measures_cmprsk.auc
#'
#' @description
#' Calculates the cause-specific time-dependent ROC-AUC at a **specific time point**,
#' as described in Blanche et al. (2013).
#'
#' By default, this measure returns a **cause-independent AUC(t)** score,
#' calculated as a weighted average of the cause-specific AUCs.
#' The weights correspond to the relative event frequencies of each cause,
#' following Equation (7) in Heyard et al. (2020).
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
#' 3. The choice of `time_horizon` is critical: if, at that time, no events of a
#' given cause have occurred and all predicted CIFs are zero, `riskRegression`
#' will return `NaN` for that cause-specific AUC (and subsequently for the
#' summary AUC).
#'
#' @section Parameter details:
#' - `cause` (`numeric(1)|"mean"`)\cr
#'  Integer number indicating which cause to use.
#'  Default value is `"mean"` which returns a weighted mean of the cause-specific AUCs.
#' - `time_horizon` (`numeric(1)`)\cr
#'  Single time point at which to return the score.
#'  If `NULL`, the **median time point** from the test set is used.
#'
#' @references
#' `r format_bib("blanche_2013", "heyard_2020")`
#'
#' @templateVar msr_id auc
#' @template example_cmprsk
#' @export
MeasureCompRisksAUC = R6Class(
  "MeasureCompRisksAUC",
  inherit = MeasureCompRisks,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        cause = p_int(lower = 1, init = "mean", special_vals = list("mean")),
        time_horizon = p_dbl(lower = 0, default = NULL, special_vals = list(NULL))
      )

      super$initialize(
        id = "cmprsk.auc",
        param_set = param_set,
        range = c(0, 1),
        minimize = FALSE,
        properties = "na_score",
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
      cif_list = prediction$cif
      causes = names(cif_list)

      cause = pv$cause
      if (test_int(cause)) {
        cause = as.character(cause)

        # check if cause exists
        if (cause %nin% causes) {
          stopf("Invalid cause. Use one of: %s", paste(causes, collapse = ", "))
        }

        # get cause-specific CIF
        cif_mat = cif_list[[cause]]

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
        # iterate through cause-specific CIFs, get AUC(t)
        aucs = vapply(causes, function(cause) {
          # get cause-specific CIF
          cif_mat = cif_list[[cause]]

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
        }, numeric(1L))

        event = data[event != 0, event] # remove censored obs (if they exist)
        w = prop.table(table(event)) # observed proportions per cause
        sum(w[names(aucs)] * aucs) # weighted mean
      }
    }
  )
)

register_measure("cmprsk.auc", MeasureCompRisksAUC)

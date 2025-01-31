#' @template surv_measure
#' @templateVar title Concordance Statistics
#' @templateVar fullname MeasureSurvCindex
#' @templateVar eps 1e-3
#' @template param_eps
#'
#' @description
#' Calculates weighted concordance statistics, which, depending on the chosen
#' weighting method (`weight_meth`) and tied times parameter (`tiex`), are
#' equivalent to several proposed methods.
#' By default, no weighting is applied and this is equivalent to Harrell's C-index.
#'
#' @details
#' For the Kaplan-Meier estimate of the **training survival** distribution (\eqn{S}),
#' and the Kaplan-Meier estimate of the **training censoring** distribution (\eqn{G}),
#' we have the following options for time-independent concordance statistics
#' (C-indexes) given the weighted method:
#'
#' `weight_meth`:
#'
#' - `"I"` = No weighting. (Harrell)
#' - `"GH"` = Gonen and Heller's Concordance Index
#' - `"G"` = Weights concordance by \eqn{1/G}.
#' - `"G2"` = Weights concordance by \eqn{1/G^2}. (Uno et al.)
#' - `"SG"` = Weights concordance by \eqn{S/G} (Shemper et al.)
#' - `"S"` = Weights concordance by \eqn{S} (Peto and Peto)
#'
#' The last three require training data. `"GH"` is only applicable to [LearnerSurvCoxPH].
#'
#' The implementation is slightly different from [survival::concordance].
#' Firstly this implementation is faster, and secondly the weights are computed
#' on the training dataset whereas in [survival::concordance] the weights are
#' computed on the same testing data.
#'
#' @section Parameter details:
#' - `t_max` (`numeric(1)`)\cr
#' Cutoff time (i.e. time horizon) to evaluate concordance up to.
#' - `p_max` (`numeric(1)`)\cr
#' The proportion of censoring to evaluate concordance up to in the given dataset.
#' When `t_max` is specified, this parameter is ignored.
#' - `weight_meth` (`character(1)`)\cr
#' Method for weighting concordance. Default `"I"` is Harrell's C. See details.
#' - `tiex` (`numeric(1)`)\cr
#' Weighting applied to tied rankings, default is to give them half (0.5) weighting.
#'
#' @references
#' `r format_bib("peto_1972", "harrell_1982", "goenen_2005", "schemper_2009", "uno_2011")`
#'
#' @template param_range
#' @template param_minimize
#' @template param_packages
#' @template param_predict_type
#' @template param_measure_properties
#'
#' @examples
#' library(mlr3)
#' task = tsk("rats")
#' learner = lrn("surv.coxph")
#' part = partition(task) # train/test split
#' learner$train(task, part$train)
#' p = learner$predict(task, part$test)
#'
#' # Harrell's C-index
#' p$score(msr("surv.cindex")) # same as `p$score()`
#'
#' # Uno's C-index
#' p$score(msr("surv.cindex", weight_meth = "G2"),
#'         task = task, train_set = part$train)
#'
#' # Harrell's C-index evaluated up to a specific time horizon
#' p$score(msr("surv.cindex", t_max = 97))
#'
#' # Harrell's C-index evaluated up to the time corresponding to 30% of censoring
#' p$score(msr("surv.cindex", p_max = 0.3))
#'
#' @export
MeasureSurvCindex = R6Class("MeasureSurvCindex",
  inherit = MeasureSurv,
  public = list(
    #' @description This is an abstract class that should not be constructed directly.
    initialize = function() {
      ps = ps(
        t_max = p_dbl(0),
        p_max = p_dbl(0, 1),
        weight_meth = p_fct(levels = c("I", "G", "G2", "SG", "S", "GH"), default = "I"),
        tiex = p_dbl(0, 1, default = 0.5),
        eps = p_dbl(0, 1, default = 1e-3)
      )
      ps$set_values(weight_meth = "I", tiex = 0.5, eps = 1e-3)

      super$initialize(
        id = "surv.cindex",
        range = 0:1,
        minimize = FALSE,
        packages = character(),
        predict_type = "crank",
        properties = character(),
        label = "Concordance Index",
        man = "mlr3proba::mlr_measures_surv.cindex",
        param_set = ps
      )

      invisible(self)
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...) {
      ps = self$param_set$values

      # calculate t_max (cutoff time horizon)
      if (is.null(ps$t_max) && !is.null(ps$p_max)) {
        truth = prediction$truth
        unique_times = unique(sort(truth[, "time"]))
        surv = survival::survfit(truth ~ 1)
        indx = which(1 - (surv$n.risk / surv$n) > ps$p_max)
        if (length(indx) == 0L) {
          t_max = NULL # t_max calculated in `cindex()`
        } else {
          # first time point that surpasses the specified
          # `p_max` proportion of censoring
          t_max = surv$time[indx[1L]]
        }
      } else {
        t_max = ps$t_max
      }

      if (ps$weight_meth == "GH") {
        return(gonen(prediction$crank, ps$tiex))
      } else if (ps$weight_meth == "I") {
        return(cindex(prediction$truth, prediction$crank, t_max, ps$weight_meth, ps$tiex))
      } else {
        if (is.null(task) | is.null(train_set)) {
          stop("'task' and 'train_set' required for all weighted C-indexes (except GH).")
        }
        return(cindex(prediction$truth, prediction$crank, t_max, ps$weight_meth,
                      ps$tiex, task$truth(train_set), ps$eps))
      }
    }
  )
)

gonen = function(crank, tiex) {
  assert_numeric(crank, any.missing = FALSE)
  assert_number(tiex)

  c_gonen(sort(crank), tiex)
}

register_measure("surv.cindex", MeasureSurvCindex)

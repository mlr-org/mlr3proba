#' @template surv_measure
#' @templateVar title Concordance Statistics
#' @templateVar fullname MeasureSurvCindex
#'
#' @description
#' Calculates weighted concordance statistics, which, depending on the chosen weighting method
#' and tied times solution, are equivalent to several proposed methods.
#'
#' For the Kaplan-Meier estimate of the training survival distribution, S, and the Kaplan-Meier
#' estimate of the training censoring distribution, G:
#'
#' `weight_meth`:
#'
#'  * `"I"` = No weighting. (Harrell)
#'  * `"GH"` = Gonen and Heller's Concordance Index
#'  * `"G"` = Weights concordance by G^-1.
#'  * `"G2"` = Weights concordance by G^-2. (Uno et al.)
#'  * `"SG"` = Weights concordance by S/G (Shemper et al.)
#'  * `"S"` = Weights concordance by S (Peto and Peto)
#'
#'  The last three require training data. `"GH"` is only applicable to [LearnerSurvCoxPH].
#'
#'  @details
#'  The implementation is slightly different from [survival::concordance]. Firstly this
#'  implementation is faster, and secondly the weights are computed on the training dataset whereas
#'  in [survival::concordance] the weights are computed on the same testing data.
#'
#' @references
#' `r format_bib("peto_1972", "harrell_1982", "goenen_2005", "schemper_2009", "uno_2011")`
#'
#' @template param_id
#' @template param_range
#' @template param_minimize
#' @template param_packages
#' @template param_predict_type
#' @template param_measure_properties
#' @export
MeasureSurvCindex = R6Class("MeasureSurvCindex",
  inherit = MeasureSurv,
  public = list(
    #' @description This is an abstract class that should not be constructed directly.
    #' @param cutoff (`numeric(1)`)\cr
    #'   Cut-off time to evaluate concordance up to.
    #' @param weight_meth (`character(1)`) \cr
    #'   Method for weighting concordance. Default `"I"` is Harrell's C. See details.
    #' @param tiex (`numeric(1)`) \cr
    #'   Weighting applied to tied rankings, default is to give them half weighting.
    initialize = function() {
      ps = ps(
        cutoff = p_dbl(),
        weight_meth = p_fct(levels = c("I", "G", "G2", "SG", "S", "GH"), default = "I"),
        tiex = p_dbl(0, 1, default = 0.5)
      )
      ps$values = list(weight_meth = "I", tiex = 0.5)

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
      if (ps$weight_meth == "GH") {
        return(gonen(prediction$crank, ps$tiex))
      } else if (ps$weight_meth == "I") {
        return(cindex(prediction$truth, prediction$crank, ps$cutoff, ps$weight_meth, ps$tiex))
      } else {
        if (is.null(task) | is.null(train_set)) {
          stop("'task' and 'train_set' required for all weighted c-index (except GH).")
        }
        return(cindex(prediction$truth, prediction$crank, ps$cutoff, ps$weight_meth,
          ps$tiex, task$truth(train_set)))
      }
    }
  )
)

gonen = function(crank, tiex) {
  assert_numeric(crank, any.missing = FALSE)
  assert_number(tiex)

  c_gonen(sort(crank), tiex)
}

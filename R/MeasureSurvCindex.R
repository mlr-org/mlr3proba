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
#'  The last three require training data.
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
    initialize = function(cutoff = NULL, weight_meth = c("I", "G", "G2", "SG", "S", "GH"), tiex = 0.5) {

      weight_meth = match.arg(weight_meth)

      id = switch(weight_meth,
                  "I" = "surv.harrell_c",
                  "G" = "surv.Gweight_c",
                  "G2" = "surv.uno_c",
                  "SG" = "surv.schemper_c",
                  "S" = "surv.peto_c",
                  "GH" = "surv.gonen_c")

      super$initialize(
        id = id,
        range = 0:1,
        minimize = FALSE,
        packages = character(),
        predict_type = "crank",
        properties = character(),
        man = "mlr3proba::mlr_measures_surv.cindex",
      )

      assertNumeric(cutoff, null.ok = TRUE)
      assertNumeric(tiex)

      private$.cutoff = cutoff
      private$.tiex = tiex
      private$.weight_meth = match.arg(weight_meth)
    }
  ),

  active = list(
    #' @field cutoff `(numeric(1))`
    #' Cut-off time to evaluate concordance up to.
    cutoff = function(cutoff){
      if (missing(cutoff)) {
        return(private$.cutoff)
      } else {
        private$.cutoff = assertNumeric(cutoff, null.ok = TRUE)
      }
    },

    #' @field weight_meth `(numeric(1))`
    #' Method for weighting concordance.
    weight_meth = function(weight_meth){
      if (missing(weight_meth)) {
        return(private$.weight_meth)
      } else {
        private$.weight_meth = assertChoice(weight_meth, c("I", "G", "G2", "SG", "S", "GH"))
      }
    },

    #' @field tiex `(numeric(1))`
    #' Cut-off time to evaluate concordance up to.
    tiex = function(tiex){
      if (missing(tiex)) {
        return(private$.tiex)
      } else {
        private$.tiex = assertNumeric(tiex)
      }
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...){
      if (self$weight_meth == "GH") {
        return(c_gonen(prediction$crank, self$tiex))
      } else if (self$weight_meth == "I") {
       return(cindex(prediction$truth, prediction$crank, self$cutoff, self$weight_meth,
                     self$tiex))
      } else {
        if (is.null(task) | is.null(train_set)) {
          stop("'task' and 'train_set' required for all weighted c-index (except GH).")
        }
        return(cindex(prediction$truth, prediction$crank, self$cutoff, self$weight_meth,
                      self$tiex, task$truth(train_set)))
      }
    },
    .cutoff = numeric(),
    .weight_meth = numeric(),
    .tiex = numeric()
  )
)

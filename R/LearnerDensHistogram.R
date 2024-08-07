#' @template dens_learner
#' @templateVar title Histogram
#' @templateVar fullname LearnerDensHistogram
#' @templateVar caller [graphics::hist()]
#'
#' @export
LearnerDensHistogram = R6::R6Class("LearnerDensHistogram",
  inherit = LearnerDens,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "dens.hist",
        param_set = ps(
          breaks = p_uty(default = "Sturges", tags = "train")
        ),
        feature_types = c("integer", "numeric"),
        predict_types = c("pdf", "cdf", "distr"),
        packages = "distr6",
        label = "Histogram Density Estimator",
        man = "mlr3proba::mlr_learners_dens.hist"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")
      fit = invoke(.histogram, dat = task$data()[[1L]], .args = pars)
      set_class(list(distr = fit$distr, hist = fit$hist), "dens.hist")
    },

    .predict = function(task) {
      newdata = task$data()[[1L]]
      list(pdf = self$model$distr$pdf(newdata), cdf = self$model$distr$cdf(newdata),
           distr = self$model$distr)
    }
  )
)

#' @include aaa.R
register_learner("dens.hist", LearnerDensHistogram)

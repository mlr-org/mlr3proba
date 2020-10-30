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
        param_set = ParamSet$new(
          params = list(
            ParamUty$new(id = "breaks", default = "Sturges", tags = "train")
        )),
        feature_types = c("integer", "numeric"),
        predict_types = c("pdf", "cdf"),
        packages = "distr6",
        man = "mlr3proba::mlr_learners_dens.hist"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tag = "train")
      fit = invoke(.histogram, dat = task$data()[[1]], .args = pars)
      set_class(list(distr = fit$distr, hist = fit$hist), "dens.hist")
    },

    .predict = function(task) {
      newdata = task$data()[[1]]
      list(pdf = self$model$distr$pdf(newdata), cdf = self$model$distr$cdf(newdata))
    }
  )
)

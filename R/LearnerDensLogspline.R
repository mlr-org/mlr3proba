#' @template dens_learner
#' @templateVar title Logspline
#' @templateVar fullname LearnerDensLogspline
#' @templateVar caller [logspline::logspline()]
#'
#' @references
#' `r format_bib("kooperberg_1992")`
#'
#' @template example_dens
#' @export
LearnerDensLogspline = R6Class("LearnerDensLogspline",
  inherit = LearnerDens,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        lbound = p_dbl(tags = "train"),
        ubound = p_dbl(tags = "train"),
        maxknots = p_dbl(default = 0, lower = 0, tags = "train"),
        knots = p_uty(tags = "train"),
        nknots = p_dbl(default = 0, lower = 0, tags = "train"),
        penalty = p_uty(tags = "train"),
        silent = p_lgl(default = TRUE, tags = "train"),
        mind = p_dbl(default = -1, tags = "train"),
        error.action = p_int(default = 2, lower = 0, upper = 2, tags = "train")
      )


      super$initialize(
        id = "dens.logspline",
        packages = c("logspline", "distr6"),
        feature_types = c("integer", "numeric"),
        predict_types = c("pdf", "cdf"),
        param_set = param_set,
        man = "mlr3proba::mlr_learners_dens.logspline",
        label = "Logspline Density Estimation"
      )
    }
  ),

  private = list(
    .train = function(task) {
      data = task$data()[[1]]
      pars = self$param_set$get_values(tags = "train")

      fit = invoke(logspline::logspline, x = data, .args = pars)

      pdf = function(x) {}
      body(pdf) = substitute({
        invoke(logspline::dlogspline, q = x, fit = fit)
      })

      cdf = function(x) {}
      body(cdf) = substitute({
        invoke(logspline::plogspline, q = x, fit = fit)
      })

      quantile = function(p) {}
      body(quantile) = substitute({
        invoke(logspline::qlogspline, p = p, fit = fit)
      })

      rand = function(n) {}
      body(rand) = substitute({
        invoke(logspline::rlogspline, n = n, fit = fit)
      })

      distr6::Distribution$new(
        name = "Logspline Density Estimator",
        short_name = "LogsplineDens",
        pdf = pdf, cdf = cdf, quantile = quantile, rand = rand, type = set6::Reals$new()
      )
    },

    .predict = function(task) {
      newdata = task$data()[[1]]

      list(pdf = self$model$pdf(newdata), cdf = self$model$cdf(newdata))
    }
  )
)

#' @include aaa.R
register_learner("dens.logspline", LearnerDensLogspline)

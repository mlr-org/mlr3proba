#' @template dens_learner
#' @templateVar title Smoothing Splines
#' @templateVar fullname LearnerDensSpline
#' @templateVar caller [gss::ssden()]
#'
#' @references
#' `r format_bib("gu_2003")`
#'
#' @template example_dens
#' @export
LearnerDensSpline = R6Class("LearnerDensSpline",
  inherit = LearnerDens,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        type = p_uty(tags = "train"),
        alpha = p_dbl(default = 1.4, tags = "train"),
        weights = p_uty(tags = "train"),
        na.action = p_uty(default = stats::na.omit, tags = "train"),
        id.basis = p_uty(tags = "train"),
        nbasis = p_int(tags = "train"),
        seed = p_dbl(tags = "train"),
        domain = p_uty(tags = "train"),
        quad = p_uty(tags = "train"),
        qdsz.depth = p_dbl(tags = "train"),
        bias = p_uty(tags = "train"),
        prec = p_dbl(default = 1e-7, tags = "train"),
        maxiter = p_int(default = 30, lower = 1, tags = "train"),
        skip.iter = p_lgl(tags = "train")
      )

      super$initialize(
        id = "dens.spline",
        packages = c("gss", "distr6"),
        feature_types = c("integer", "numeric"),
        predict_types = c("pdf", "cdf"),
        param_set = ps,
        properties = "missings",
        man = "mlr3proba::mlr_learners_dens.spline",
        label = "Density Smoothing Splines"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")
      data = task$data()[[1]]

      fit = invoke(gss::ssden, formula = ~data, .args = pars)

      pdf = function(x) {}
      body(pdf) = substitute({
        invoke(gss::dssden, object = fit, x = x)
      })

      cdf = function(x) {}
      body(cdf) = substitute({
        invoke(gss::pssden, object = fit, q = x)
      })

      quantile = function(p) {}
      body(quantile) = substitute({
        invoke(gss::qssden, object = fit, p = p)
      })

      distr6::Distribution$new(
        name = "Smoothing Spline Density Estimator",
        short_name = "splineDens",
        pdf = pdf,
        cdf = cdf,
        quantile = quantile,
        type = set6::Reals$new()
      )
    },

    .predict = function(task) {
      newdata = task$data()[[1]]
      list(pdf = self$model$pdf(newdata), cdf = self$model$cdf(newdata))
    }
  )
)

#' @include aaa.R
register_learner("dens.spline", LearnerDensSpline)

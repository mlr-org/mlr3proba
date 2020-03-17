#' @template dens_learner
#' @templateVar title Smoothing Splines
#' @templateVar fullname LearnerDensSpline
#' @templateVar caller [gss::ssden()]
#'
#' @export
LearnerDensSpline <- R6::R6Class("LearnerDensSpline", inherit = LearnerDens,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(){
    super$initialize(
      id = "dens.spline",
      param_set = ParamSet$new(
        params = list(
          ParamUty$new(id = "type", tags = "train"),
          ParamDbl$new(id = "alpha", default = 1.4, tags = "train"),
          ParamUty$new(id = "weights", tags = "train"),
          ParamUty$new(id = "na.action", default = na.omit, tags = "train"),
          ParamDbl$new(id = "nbasis",  tags = "train"),
          ParamDbl$new(id = "seed",  tags = "train"),
          ParamUty$new(id = "domain", tags = "train"),
          ParamUty$new(id = "quad", tags = "train"),
          ParamDbl$new(id = "qdsz.depth",  tags = "train"),
          ParamUty$new(id = "bias", tags = "train"),
          ParamDbl$new(id = "prec", default = 1e-7, tags = "train"),
          ParamInt$new(id = "maxiter", default = 30, lower = 1, tags = "train"),
          ParamLgl$new(id = "skip.iter",  tags = "train")
        )),
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      properties = "missings",
      predict_types = c("pdf", "cdf"),
      packages = c("gss", "distr6")
    )}
  ),

  private = list(
    .train = function(task){

      pars = self$param_set$get_values(tags = "train")

      data = task$truth()

      fit = invoke(gss::ssden, formula = ~data, .args = pars)

      pdf <- function(x1){}
      body(pdf) <- substitute({

        invoke(gss::dssden, object = fit, x = x1)

      })

      cdf <- function(x1){}
      body(cdf) <- substitute({

        invoke(gss::pssden, object = fit, q = x1)

      })

      quantile <- function(x1){}
      body(quantile) <- substitute({

        invoke(gss::qssden, object = fit, p = x1)

      })


      Distribution$new(name = "Smoothing Spline Density Estimator",
                       short_name = "splineDens",
                       pdf = pdf, cdf = cdf, quantile = quantile)
    },

    .predict = function(task){
      newdata = task$truth()

      PredictionDens$new(task = task, pdf = self$model$pdf(newdata),
                         cdf = self$model$cdf(newdata))
    }
  )
)

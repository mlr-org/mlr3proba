#' @template dens_learner
#' @templateVar title Logspline
#' @templateVar fullname LearnerDensLogspline
#' @templateVar caller [logspline::logspline()]
#'
#' @export
LearnerDensLogspline<- R6::R6Class("LearnerDensLogspline", inherit = LearnerDens,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(){
    super$initialize(
      id = "dens.logspline",
      param_set = ParamSet$new(list(
        ParamDbl$new(id = "lbound", tags = "train"),
        ParamDbl$new(id = "ubound", tags = "train"),
        ParamDbl$new(id = "maxknots", default = 0,  lower = 0, tags = "train"),
        ParamUty$new(id = "knots", tags = "train"),
        ParamDbl$new(id = "nknots", default = 0, lower = 0, tags = "train"),
        ParamUty$new(id = "penalty", tags = "train"),
        ParamDbl$new(id = "mind", default = -1, tags ="train"),
        ParamInt$new(id = "error.action", default = 2, lower = 0, upper = 2, tags = "train")
      )),
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      predict_types = c("pdf", "cdf"),
      packages = c("logspline", "distr6")
    )}
  ),

  private = list(
    .train = function(task){

      data = task$truth()

      pars = self$param_set$get_values(tag="train")

      fit = invoke(logspline::logspline, x = data, .args = pars)

      pdf <- function(x1){}
      body(pdf) <- substitute({
        invoke(logspline::dlogspline, q = x1, fit = fit)
      })

      cdf <- function(x1){}
      body(cdf) <- substitute({
        invoke(logspline::plogspline, q = x1, fit = fit)
      })

      quantile <- function(x1){}
      body(quantile) <- substitute({
        invoke(logspline::qlogspline, p = x1, fit = fit)
      })

      rand <- function(x1){}
      body(rand) <- substitute({
        invoke(logspline::rlogspline, n = x1, fit = fit)
      })

      Distribution$new(name = "Logspline Density Estimator",
                       short_name = "LogsplineDens",
                       pdf = pdf, cdf = cdf, quantile = quantile, rand = rand)

    },

    .predict = function(task){
      PredictionDens$new(task = task,
                         pdf = self$model$pdf(task$truth()),
                         cdf = self$model$cdf(task$truth()))
    }
  )
)

#' @template dens_learner
#' @templateVar title Penalized
#' @templateVar fullname LearnerDensPenalized
#' @templateVar caller [pendensity::pendensity()]
#'
#' @export
LearnerDensPenalized <- R6::R6Class("LearnerDensPenalized", inherit = LearnerDens,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(){
    super$initialize(
      id = "dens.pen",
      param_set = ParamSet$new(
        params = list(
          ParamDbl$new(id = "no.base",  default= 41, tags = "train"),
          ParamDbl$new(id = "max.iter", default = 1, tags = "train"),
          ParamDbl$new(id = "lambda0", default = 500, tags = "train"),
          ParamDbl$new(id = "q", default = 3, tags = "train"),
          ParamLgl$new(id = "sort", default = TRUE, tags = "train"),
          ParamUty$new(id = "with.border",  tags = "train"),
          ParamDbl$new(id = "m", default = 3, tags = "train"),
          ParamDbl$new(id = "eps", default = 0.01, tags = "train"),
          ParamFct$new(id = "base", levels = c("gaussian", "bspline"),
                       default = "gaussian", tags = "train")
        )),
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      predict_types = c("pdf", "cdf"),
      packages = c("pendensity", "distr6")
    )}
  ),

  private = list(
    .train = function(task){

      pars = self$param_set$get_values(tag="train")

      capture.output(fit <- invoke(pendensity::pendensity, form = task$truth() ~ 1, .args = pars))
      #suppress the automated output of pendensity

      pdf <- function(x1){}

      body(pdf) <- substitute({

        invoke(pendensity::dpendensity, x = fit, val=x1)

      })

      cdf <- function(x1){}

      body(cdf) <- substitute({

        invoke(pendensity::ppendensity, x = fit, val=x1)

      })


      Distribution$new(name = paste("Penalized Density", self$param_set$values$base),
                       short_name = paste("PenDens_", self$param_set$values$base),
                       pdf = pdf, cdf = cdf)
    },

    .predict = function(task){

      newdata = task$truth()

      PredictionDens$new(task = task, pdf = self$model$pdf(newdata), cdf = self$model$pdf(newdata))

    }
  )
)


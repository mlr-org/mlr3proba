#' @template dens_learner
#' @templateVar title Local
#' @templateVar fullname LearnerDensLogLocfit
#' @templateVar caller [locfit::density.lf()]
#'
#' @export
LearnerDensLocfit <- R6::R6Class("LearnerDensLocfit", inherit = LearnerDens,
  public = list(initialize = function(id = "dens.locfit"){
      ps = ParamSet$new(
           params = list(
           ParamFct$new(id = "window", levels = c("gaussian", "epanechnikov", "rectangular",
                                               "triangular", "biweight", "uniform",
                                               "optcosine"), default = "gaussian", tags = "train"),
           ParamDbl$new(id = "width", tags = "train"),
           ParamDbl$new(id = "from", tags = "train"),
           ParamDbl$new(id = "to", tags = "train"),
           ParamUty$new(id = "cut",  tags = "train"),
           ParamDbl$new(id = "deg", default = 0, tags = "train"),
           ParamUty$new(id = "family", default = "density", tags = "train"),
           ParamUty$new(id = "link", default = "identity", tags = "train")
          ))

          ps$values = list(window = "gaussian", family = "density", link = "identity")

          super$initialize(
          id = id,
          param_set = ps,
          feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
          predict_types = "pdf",
          packages = c("locfit", "distr6")
          )},

          train_internal = function(task){

          pars = self$param_set$get_values(tag="train")

          data = task$target()

          pdf <- function(x1){}

          body(pdf) <- substitute({

          invoke(locfit::density.lf, x = data, ev = x1, .args = pars)$y

          })

          Distribution$new(name = paste("LocFit Density", self$param_set$values$window),
                       short_name = paste0("LocFitDens",self$param_set$values$window),
                       pdf = pdf)
          },

        predict_internal = function(task){

        newdata = task$target()

        PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

    }

  ))


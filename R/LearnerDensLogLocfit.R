#' @template dens_learner
#' @templateVar title Local
#' @templateVar fullname LearnerDensLogLocfit
#' @templateVar caller [locfit::density.lf()]
#'
#' @export
LearnerDensLogLocfit <- R6::R6Class("LearnerDensLogLocfit", inherit = LearnerDens,
  public = list(initialize = function(id = "dens.logLF"){
    super$initialize(
      id = id,
      param_set = ParamSet$new(
        params = list(
          ParamFct$new(id = "window", levels = c("gaussian", "epanechnikov", "rectangular",
                                                 "triangular", "biweight", "uniform",
                                                 "optcosine"),
                       default = "gaussian", tags = "train"),
          ParamDbl$new(id = "width", tags = "train"),
          ParamDbl$new(id = "from", tags = "train"),
          ParamDbl$new(id = "to", tags = "train"),
          ParamUty$new(id = "cut", default = 0L, tags = "train"),
          ParamDbl$new(id = "deg", default = 0, tags = "train"),
          ParamUty$new(id = "family", default = "density", tags = "train"),
          ParamUty$new(id = "link", default = "ident", tags = "train")
        )),
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      predict_types = "pdf",
      packages = c("locfit", "distr6")
    )},

    train_internal = function(task){

      pars = self$param_set$get_values(tag="train")

      data = as.numeric(unlist(task$data(cols = task$target_names)))

      pdf <- function(x1){}

      body(pdf) <- substitute({

        invoke(locfit::density.lf, x = data, ev = x1, .args = pars)$y

      })

      Distribution$new(name = paste("LocFit Density", self$param_set$values$window),
                       short_name = paste0("LocFitDens",self$param_set$values$window),
                       pdf = pdf)
    },

    predict_internal = function(task){

      newdata = as.numeric(unlist(task$data(cols = task$target_names)))

      PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

    }

  ))


#' @template dens_learner
#' @templateVar title Smoothing Splines
#' @templateVar fullname LearnerDensPenGss
#' @templateVar caller [gss::ssden()]
#'
#' @export
LearnerDensPenGss <- R6::R6Class("LearnerDensPenGss", inherit = LearnerDens,
  public = list(initialize = function(id = "dens.penGSS"){
    super$initialize(
      id = id,
      param_set = ParamSet$new(
        params = list(
          ParamUty$new(id = "type", tags = "train"),
          ParamUty$new(id = "subset", default = NULL,  tags = "train"),
          ParamUty$new(id = "data", tags = "train"),
          ParamDbl$new(id = "alpha", default =1.4, tags = "train"),
          ParamUty$new(id = "weights", default =NULL, tags = "train"),
          ParamUty$new(id = "na.action", default =na.omit, tags = "train"),
          ParamUty$new(id = "nbasis", default =NULL, tags = "train"),
          ParamUty$new(id = "seed", default =NULL, tags = "train"),
          ParamUty$new(id = "domain", default = as.list(NULL), tags = "train"),
          ParamUty$new(id = "quad", default =NULL, tags = "train"),
          ParamUty$new(id = "qdsz.depth", default =NULL, tags = "train"),
          ParamUty$new(id = "bias", default =NULL, tags = "train"),
          ParamDbl$new(id = "prec", default = 1e-7, tags = "train"),
          ParamDbl$new(id = "maxiter", default = 30, tags = "train"),
          ParamLgl$new(id = "skip.iter", default = FALSE, tags = "train")
         )),
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      predict_types = "pdf",
      packages = c("gss", "distr6")
    )},

    train_internal = function(task){

      pars = self$param_set$get_values(tags = "train")

      data = as.numeric(unlist(task$data(cols = task$target_names)))

      pdf <- function(x1){}
      body(pdf) <- substitute({

      invoke(.DensGss, dat = data, test = x1, .args = pars)

      })


      Distribution$new(name = "gss Penalized Density",
                       short_name = "gssPenDens",
                       pdf = pdf)



    },

    predict_internal = function(task){

      newdata = as.numeric(unlist(task$data(cols = task$target_names)))

      # pars = self$param_set$get_values(tags = "predict")

      # pdf  = invoke(gss::dssden, x = newdata, object = self$model)

      PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

    }
  ))

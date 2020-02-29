#' @template dens_learner
#' @templateVar title Gaussian Univariate Kernel
#' @templateVar fullname LearnerDensKDEgk
#' @templateVar caller [GenKern::KernSec()]
#'
#' @export
LearnerDensKDEgk <- R6::R6Class("LearnerDensKDEgk", inherit = LearnerDens,
  public = list(initialize = function(id = "dens.kdeGK"){
    super$initialize(
      id = id,
      param_set = ParamSet$new(
        params = list(
          ParamDbl$new(id = "xbandwidth", default = 1,  tags = "train"),
          ParamDbl$new(id = "xgridsize", default = 100, tags = "train")
        )),
      feature_types =  c("integer", "numeric", "character", "factor", "ordered"),
      predict_types = "pdf",
      packages = c("GenKern", "distr6")
    )},

    train_internal = function(task){

      pars = self$param_set$get_values(tag="train")
      data = as.numeric(unlist(task$data(cols = task$target_names)))

      pdf = function(x1){}
      body(pdf) = substitute({

        invoke(.DensGenKern, x = data, range.x = x1, .args = pars)

      })

      Distribution$new(name = "GenKern KDE Gaussian",
                       short_name = "GenKernKDEGaus",
                       pdf = pdf)


    },

    predict_internal = function(task){

      newdata = as.numeric(unlist(task$data(cols = task$target_names)))

      PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

    }
  )
)


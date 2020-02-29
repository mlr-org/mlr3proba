#' @template dens_learner
#' @templateVar title Plug-In Kernel
#' @templateVar fullname LearnerDensKDEpd
#' @templateVar caller [plugdensity::plugin.density()]
#'
#' @export
LearnerDensKDEpd <- R6::R6Class("LearnerDensKDEpd", inherit = LearnerDens,
  public = list(initialize = function(id = "dens.kdePD"){
    super$initialize(
      id = id,
      param_set = ParamSet$new(
        params = list(
          ParamUty$new(id = "na.rm", default = FALSE, tags = "train"),
          ParamDbl$new(id = "nout", default =201, tags = "train")
        )),
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      predict_types = "pdf",
      packages = c("plugdensity", "distr6")
    )},

    train_internal = function(task){

      data = as.numeric(unlist(task$data(cols = task$target_names)))

      pdf <- function(x1){}

      body(pdf) <- substitute({

        invoke(plugdensity::plugin.density, x = data, xout = x1)$y
      })


      Distribution$new(name = "plugdensity KDE",
                       short_name = "plugdensityKDE",
                       pdf = pdf)
    },

    predict_internal = function(task){

      newdata = as.numeric(unlist(task$data(cols = task$target_names)))

      PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

    }
  ))


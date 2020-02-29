#' @template dens_learner
#' @templateVar title Penalized
#' @templateVar fullname LearnerDensPenPD
#' @templateVar caller [pendensity::pendensity()]
#'
#' @export
LearnerDensPenPD <- R6::R6Class("LearnerDensPenPD", inherit = LearnerDens,
  public = list(initialize = function(id = "dens.penPD"){
    super$initialize(
      id = id,
      param_set = ParamSet$new(
        params = list(
          ParamDbl$new(id = "no.base",  default= 41, tags = "train"),
          ParamDbl$new(id = "max.iter", default = 1, tags = "train"),
          ParamDbl$new(id = "lambda0", default = 500, tags = "train"),
          ParamDbl$new(id = "q", default = 3, tags = "train"),
          ParamLgl$new(id = "sort", default = TRUE, tags = "train"),
          ParamUty$new(id = "with.border", default = NULL, tags = "train"),
          ParamDbl$new(id = "m", default = 3, tags = "train"),
          ParamDbl$new(id = "eps", default = 0.01, tags = "train"),
          ParamFct$new(id = "base", levels = c("gaussian", "bspline"),
                       default = "gaussian", tags = "train")
        )),
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      predict_types = "pdf",
      packages = c("pendensity", "distr6")
    )},

    train_internal = function(task){

      pars = self$param_set$get_values(tag="train")

      # data = as.data.frame(unlist(task$data(cols = task$target_names)))
      data =  data = as.numeric(unlist(task$data(cols = task$target_names)))

      pdf <- function(x1){}

      body(pdf) <- substitute({

        invoke(.PDDens, dat = data, test = x1, .args = pars)$dens

      })

      Distribution$new(name = paste("Pendensity Density", self$param_set$values$base),
                       short_name = paste0(self$param_set$values$base),
                       pdf = pdf)
    },

    predict_internal = function(task){

      newdata = as.numeric(unlist(task$data(cols = task$target_names)))

      PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

    }

  )
)


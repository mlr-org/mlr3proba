#' @template dens_learner
#' @templateVar title Mixed Data Kernel
#' @templateVar fullname LearnerDensKDEbwnp
#' @templateVar caller [np::npudens()]
#'
#' @export
LearnerDensKDEbwnp <- R6::R6Class("LearnerDensKDEbwnp", inherit = LearnerDens,
  public = list(initialize = function(id = "dens.kdeBWNP"){
    super$initialize(
      id = id,
      param_set = ParamSet$new(
        params = list(
          ParamFct$new(id = "ckertype", default = "gaussian",
                       levels = c("gaussian", "epanechnikov"),
                       tags = c("train", "predict")),
          ParamUty$new(id = "bandwidth.compute", default = TRUE, tags = "train"),
          ParamFct$new(id = "bwmethod", default = "cv.ml",
                       levels = c("cv.ml", "cv.ls", "normal-referenc"),
                       tags = "train"),
          ParamFct$new(id = "bwtype", default = "fixed",
                       levels = c("fixed", "generalized_nn", "adaptive_nn"),
                       tags = "train"),
          ParamDbl$new(id = "ckeorder", default= 2, tags = "train"),
          ParamLgl$new(id = "remin", default = TRUE,  tags = "train"),
          ParamDbl$new(id = "itmax", default= 10000, tags = "train"),
          ParamDbl$new(id = "ftol", default= 1.490116e-07, tags = "train"),
          ParamDbl$new(id = "tol", default= 1.490116e-04, tags = "train"),
          ParamDbl$new(id = "small", default= 1.490116e-05, tags = "train"),
          ParamDbl$new(id = "lbc.dir", default= 0.5, tags = "train"),
          ParamDbl$new(id = "dfc.dir", default= 0.5, tags = "train"),
          ParamDbl$new(id = "cfac.dir", default=2.5*(3.0-sqrt(5)), tags = "train"),
          ParamDbl$new(id = "initc.dir", default= 1.0, tags = "train"),
          ParamDbl$new(id = "lbd.dir", default = 0.1, tags = "train"),
          ParamDbl$new(id = "hbd.dir", default = 1, tags = "train"),
          ParamDbl$new(id = "dfac.dir", default = 0.25*(3.0-sqrt(5)), tags = "train"),
          ParamDbl$new(id = "initd.dir", default= 1.0, tags = "train"),
          # ParamDbl$new(id = "lbc.init", default= 0.1, tags = "train"),
          ParamDbl$new(id = "hbc.init", default= 2.0, tags = "train"),
          ParamDbl$new(id = "cfac.init", default= 0.5, tags = "train"),
          ParamDbl$new(id = "lbd.init", default= 0.1, tags = "train"),
          ParamDbl$new(id = "hbd.init", default= 0.9, tags = "train"),
          ParamDbl$new(id = "dfac.init", default= 0.37, tags = "train"),
          ParamDbl$new(id = "bws", tags = "predict")
        )),
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      predict_types = "pdf",
      packages = c("np", "distr6")
    )},
    train_internal = function(task){

      pars = self$param_set$get_values(tag="train")

      data = as.data.frame(unlist(task$data(cols = task$target_names)))

      # pdf <- function(x1){}
      #
      # body(pdf) <- substitute({


      target = task$truth()

      invoke(np::npudensbw, dat = data, edat = target, .args = pars)

      # })
      #
      #
      # Distribution$new(name = paste("Gaussian KDE"),
      #                  short_name = paste0("GausKDE"),
      #                  pdf= pdf)
    },

    predict_internal = function(task){

      pars = self$param_set$get_values(tags = "predict")

      newdata = as.data.frame(unlist(task$data(cols = task$target_names)))

      pdf = as.numeric(invoke(np::npudens,  bws = self$model, edat = newdata, .args = pars)$dens)

      PredictionDens$new(task = task, pdf = pdf)

    }

  )
)

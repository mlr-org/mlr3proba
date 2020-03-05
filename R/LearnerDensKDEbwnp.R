#' @template dens_learner
#' @templateVar title Mixed Data Kernel
#' @templateVar fullname LearnerDensKDEbwnp
#' @templateVar caller [np::npudens()]
#'
#' @export
LearnerDensKDEbwnp <- R6::R6Class("LearnerDensKDEbwnp", inherit = LearnerDens,
  public = list(initialize = function(id = "dens.kdeBWNP"){

      ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "bws", tags = "train"),
          ParamFct$new(id = "ckertype", default = "gaussian",
                       levels = c("gaussian", "epanechnikov", "uniform"),
                       tags = c("train", "predict")),
          ParamUty$new(id = "bandwidth.compute", default = TRUE, tags = "train"),
          ParamLgl$new(id = "bwscaling", default = FALSE, tags = "train"),
          ParamFct$new(id = "bwmethod", default = "cv.ml",
                       levels = c("cv.ml", "cv.ls", "normal-referenc"),
                       tags = "train"),
          ParamFct$new(id = "bwtype", default = "fixed",
                       levels = c("fixed", "generalized_nn", "adaptive_nn"),
                       tags = "train"),
          ParamInt$new(id = "ckerorder", default= 2, lower = 2, upper = 8, tags = "train"),
          ParamLgl$new(id = "remin", default = TRUE,  tags = "train"),
          ParamInt$new(id = "itmax", lower = 1,  default= 10000, tags = "train"),
          ParamInt$new(id = "nmulti", lower = 1,  tags = "train"),
          ParamDbl$new(id = "ftol", default= 1.490116e-07, tags = "train"),
          ParamDbl$new(id = "tol", default= 1.490116e-04, tags = "train"),
          ParamDbl$new(id = "small", default= 1.490116e-05, tags = "train"),
          ParamDbl$new(id = "lbc.dir", default= 0.5, tags = "train"),
          ParamDbl$new(id = "dfc.dir", default= 0.5, tags = "train"),
          ParamUty$new(id = "cfac.dir", default=2.5*(3.0-sqrt(5)), tags = "train"),
          ParamDbl$new(id = "initc.dir", default= 1.0, tags = "train"),
          ParamDbl$new(id = "lbd.dir", default = 0.1, tags = "train"),
          ParamDbl$new(id = "hbd.dir", default = 1, tags = "train"),
          ParamUty$new(id = "dfac.dir", default = 0.25*(3.0-sqrt(5)), tags = "train"),
          ParamDbl$new(id = "initd.dir", default= 1.0, tags = "train"),
          ParamDbl$new(id = "lbc.init", default= 0.1, tags = "train"),
          ParamDbl$new(id = "hbc.init", default= 2.0, tags = "train"),
          ParamDbl$new(id = "cfac.init", default= 0.5, tags = "train"),
          ParamDbl$new(id = "lbd.init", default= 0.1, tags = "train"),
          ParamDbl$new(id = "hbd.init", default= 0.9, tags = "train"),
          ParamDbl$new(id = "dfac.init", default= 0.37, tags = "train"),
          ParamFct$new(id = "ukertype", levels = c("aitchisonaitken", "liracine"), tags = "train"),
          ParamFct$new(id = "okertype", levels = c("wangvanryzin", "liracine"), tags = "train")
        ))
      ps$values = list(ckertype = "gaussian", bwmethod = "cv.ml", bwscaling = FALSE, bwtype = "fixed")
      super$initialize(
      id = id,
      param_set = ps,
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      predict_types = "pdf",
      packages = c("np", "distr6")
    )},
    train_internal = function(task){

      pars = self$param_set$get_values(tag="train")

      data = task$truth()

      bw = self$param_set$values$bws

      bw.comp  = self$param_set$values$bws == FALSE

      pdf <- function(x1){}

      body(pdf) <- substitute({

        if(is.null(bw)){

          return(np::npudens(bws = invoke(np::npudensbw, dat = data, .args = pars), edat = x1)$dens)

        } else{invoke(np::npudens, edat = x1, bws = invoke(np::npudensbw, dat = data, bandwidth.compute = bw.comp, .args = pars))$dens}

        }, list(bw = bw))

      Distribution$new(name = paste("np KDE", self$param_set$values$ckertype),
                       short_name = paste0("npKDE_", self$param_set$values$ckertype),
                       pdf= pdf)
    },


    predict_internal = function(task){

      newdata = task$truth()

      PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

    }

  )
)

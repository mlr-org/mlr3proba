#' @template dens_learner
#' @templateVar title Mixed Data Kernel
#' @templateVar fullname LearnerDensKDEnp
#' @templateVar caller [np::npudens()]
#'
#' @export
LearnerDensKDEnp <- R6::R6Class("LearnerDensKDEnp", inherit = LearnerDens,
  public = list(initialize = function(id = "dens.kdeNP"){
    super$initialize(
      param_set = ParamSet$new(list(
        ParamDbl$new(id = "bws",  tags = "train")
        # ParamFct$new(id = "ckertype", default = "gaussian",
        #              levels = c("gaussian", "epanechnikov", "uniform"),
        #              tags = "train")
        # ParamFct$new(id = "bwmethod", default = "cv.ml",
        #              levels = c("cv.ml", "cv.ls"),
        #              tags = "train"),
        # ParamLgl$new(id = "bandwidth.compute", default = FALSE, tags = "train"),
        # ParamDbl$new(id = "ckerorder", default= 2, tags = "train"),
        # ParamFct$new(id = "bwtype", default= "fixed",
        #              levels = c("fixed", "generalized_nn", "adaptive_nn"), tags = "train"),
        # ParamLgl$new(id = "bwscaling", default= TRUE, tags = "train")
      )),

      #  ps$values = list(ckertype = "gaussian", ckerorder = 2)

      id = id,
      # param_set = ps,
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      predict_types = "pdf",
      packages = c("np", "distr6")
    )},

    train_internal = function(task){

      data = as.numeric(unlist(task$data(cols = task$target_names)))

      pars = self$param_set$get_values(tag="train")

      pdf <- function(x1){}

      body(pdf) <- substitute({


      invoke(np::npudens, tdat = data, edat = x1,  .args = pars)$dens

      })

      Distribution$new(name = paste("np KDE"), #, self$param_set$values$ckertype),
                      short_name = paste0("npKDE"), #self$param_set$values$ckertype),
                       pdf = pdf)

    },

    predict_internal = function(task){

      newdata = as.numeric(unlist(task$data(cols = task$target_names)))

      PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

    }
  )
  )


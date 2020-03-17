#' @template dens_learner
#' @templateVar title Mixed Data Kernel
#' @templateVar fullname LearnerDensMixed
#' @templateVar caller [np::npudens()]
#'
#' @export
LearnerDensMixed <- R6::R6Class("LearnerDensMixed", inherit = LearnerDens,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(){
    ps = ParamSet$new(
      params = list(
        ParamUty$new(id = "bws", tags = "train"),
        ParamFct$new(id = "ckertype", default = "gaussian",
                     levels = c("gaussian", "epanechnikov", "uniform"),
                     tags = c("train")),
        ParamLgl$new(id = "bwscaling", default = FALSE, tags = "train"),
        ParamFct$new(id = "bwmethod", default = "cv.ml",
                     levels = c("cv.ml", "cv.ls", "normal-reference"),
                     tags = "train"),
        ParamFct$new(id = "bwtype", default = "fixed",
                     levels = c("fixed", "generalized_nn", "adaptive_nn"),
                     tags = "train"),
        ParamLgl$new(id = "bandwidth.compute", default = FALSE, tags = "train"),
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
    ps$values = list(ckertype = "gaussian")
    super$initialize(
      id = "dens.mixed",
      param_set = ps,
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      predict_types = "pdf",
      packages = c("np", "distr6")
    )}
  ),

  private = list(
    .train = function(task){

      pdf <- function(x1){}
      body(pdf) <- substitute({
        with_package("np", invoke(np::npudens, tdat = data.frame(data), edat = data.frame(x1), .args = pars)$dens)
      }, list(data = task$truth(),
              pars = self$param_set$get_values(tag="train")))

      Distribution$new(name = paste("Mixed KDE", self$param_set$values$ckertype),
                       short_name = paste0("MixedKDE_", self$param_set$values$ckertype),
                       pdf= pdf)
    },

    .predict = function(task){
      PredictionDens$new(task = task, pdf = self$model$pdf(task$truth()))
    }
  )
)

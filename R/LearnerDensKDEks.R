#' @template dens_learner
#' @templateVar title KS Kernel
#' @templateVar fullname LearnerDensKDEks
#' @templateVar caller [ks::kde()]
#'
#' @export
LearnerDensKDEks <- R6::R6Class("LearnerDensKDEks", inherit = LearnerDens,
  public = list(initialize = function(id = "dens.kdeKS"){
          super$initialize(
                  id = id,
                  param_set = ParamSet$new(
                          params = list(
                                  ParamDbl$new(id = "h", lower = 0,   tags = "train"),
                                  ParamUty$new(id = "H", tags = "train"),
                                  ParamUty$new(id = "gridtype", tags = "train"),
                                  ParamDbl$new(id = "xmin", tags = "train"),
                                  ParamDbl$new(id = "xmax", tags = "train"),
                                  ParamDbl$new(id = "supp", default =3.7, tags= "train"),
                                  ParamDbl$new(id = "binned", tags = "train"),
                                  ParamUty$new(id = "bgridsize", tags = "train"),
                                  ParamLgl$new(id = "positive", default = FALSE, tags = "train"),
                                  ParamUty$new(id = "adj.positive", tags = "train"),
                                  ParamUty$new(id = "w", tags = "train"),
                                  ParamLgl$new(id = "compute.cont", default =TRUE, tags= "train"),
                                  ParamLgl$new(id = "approx.cont", default =TRUE, tags = "train"),
                                  ParamLgl$new(id = "unit.interval",  default=FALSE, tags = "train"),
                                  ParamLgl$new(id = "zero.flag", tags = "train")

                          )),
                  feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
                  predict_types = "pdf",
                  packages = c("ks", "distr6")
          )},

          train_internal = function(task){

                  pars = self$param_set$get_values(tag="train")

                  data = as.numeric(unlist(task$data(cols = task$target_names)))

                  pdf <- function(x1){}

                  body(pdf) <- substitute({

                          invoke(ks::kde, x = data, eval.points = x1, .args =pars)$estimate

                  })

                  Distribution$new(name = "ks KDE",
                                   short_name = "ksKDE",
                                   pdf = pdf)
          },

          predict_internal = function(task){

                  newdata = as.numeric(unlist(task$data(cols = task$target_names)))

                  PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

          }
  )
 )


#' @template dens_learner
#' @templateVar title Kernel
#' @templateVar fullname LearnerDensKDEgk
#' @templateVar caller [GenKern::KernSec]
#'
#' @export
LearnerDensKDEgk = R6::R6Class("LearnerDensKDEgk", inherit = LearnerDens)
LearnerDensKDEgk$set("public", "initialize", function(id = "dens.kdeGK") {
  ps = ParamSet$new(list(ParamUty$new(id = "xbandwidth", tags = "train"),
                         ParamInt$new(id = "xgridsize", lower =0, default = 100, tags = "train")
  ))

  ps$values = list(xgridsize = 100)

  super$initialize(
    id = id,
    param_set = ps,
    predict_types = "pdf",
    feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
    properties = "missings",
    packages = c("GenKern", "distr6")
  )
})
LearnerDensKDEgk$set("public", "train_internal", function(task){
  pdf <- function(x1){}

  body(pdf) <- substitute({

    ntrain = as.numeric(train)

    return(sapply(x1, function(y) GenKern::KernSec(x = ntrain, xbandwidth = xb,
                                                   xgridsize = xg, range.x = y)$yden/rows))


  }, list(rows = task$nrow,
          xb = self$param_set$values$xbandwidth,
          xg = self$param_set$values$xgridsize,
          train = task$truth()))

  Distribution$new(name = "GenKern KDE",
                   short_name = "GenKernKDE",
                   pdf = pdf)

})
LearnerDensKDEgk$set("public", "predict_internal", function(task){
  PredictionDens$new(task = task, pdf = self$model$pdf(task$truth()))
})

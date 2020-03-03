#' @template dens_learner
#' @templateVar title Kernel
#' @templateVar fullname LearnerDensKDEgk
#' @templateVar caller [GenKern::KernSec]
#'
#' @export
LearnerDensKDEgk = R6::R6Class("LearnerDensKDEgk", inherit = LearnerDens,
  public = list(
    initialize = function(id = "dens.kdeGK") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(
          list(
            ParamUty$new(id = "xbandwidth", tags = "train"),
            ParamInt$new(id = "xgridsize", lower =0, default = 100, tags = "train")
          )
        ),
        predict_types = "pdf",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = "missings",
        packages = c("GenKern", "KernSmooth", "distr6")
      )
    },

    train_internal = function(task){
      pars = self$param_set$get_values(tag = "train")
      pdf <- function(x1){}
      body(pdf) <- substitute({
        with_package("KernSmooth",{
          return(sapply(x1, function(y) invoke(GenKern::KernSec,
                                               x = train,
                                               range.x = y,
                                               .args = pars)$yden/rows))
        })
      }, list(rows = task$nrow,
              pars = pars,
              train = task$truth()))

      Distribution$new(name = "GenKern KDE",
                       short_name = "GenKernKDE",
                       pdf = pdf)

    },

    predict_internal = function(task){
      PredictionDens$new(task = task, pdf = self$model$pdf(task$truth()))
    }
  )
)

#' @template dens_learner
#' @templateVar title Nonparametric
#' @templateVar fullname LearnerDensNonparametric
#' @templateVar caller [sm::sm.density()]
#'
#' @export
LearnerDensNonparametric<- R6::R6Class("LearnerDensNonparametric", inherit = LearnerDens,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(){
      super$initialize(
        id = "dens.nonpar",
        param_set = ParamSet$new(
          params = list(
            ParamDbl$new(id = "h",  tags = "train"),
            ParamUty$new(id = "group",  tags = "train"),
            ParamDbl$new(id = "delta", tags = "train"),
            ParamDbl$new(id = "h.weights",  default = 1, tags = "train"),
            ParamUty$new(id = "hmult", default = 1, tags = "train"),
            ParamFct$new(id = "method",  default = "normal", levels = c("normal", "cv", "sj", "df", "aicc"), tags = "train"),
            ParamLgl$new(id = "positive", default = FALSE, tags = "train"),
            ParamUty$new(id = "verbose", default = 1, tags = "train")
          )),
        feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = "pdf",
        properties = "weights",
        packages = c("sm", "distr6")
      )}
  ),

  private = list(
    .train = function(task){

      pars = self$param_set$get_values(tag="train")
      if ("weights" %in% task$properties) {
        pars$weights = task$weights$weight
      }

      pdf <- function(x1){}
      body(pdf) <- substitute({
        invoke(sm::sm.density, x = data, eval.points = x1, display = "none", show.script = FALSE,
               .args = pars)$estimate
      }, list(data = task$truth()))

      Distribution$new(name = "Nonparametric Density",
                       short_name = "NonparDens",
                       pdf = pdf)
    },

    .predict = function(task){
      PredictionDens$new(task = task, pdf = self$model$pdf(task$truth()))
    }
  )
)

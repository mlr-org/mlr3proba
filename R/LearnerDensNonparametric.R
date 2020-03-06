#' @template dens_learner
#' @templateVar title Nonparametric
#' @templateVar fullname LearnerDensNonparametric
#' @templateVar caller [sm::sm.density()]
#'
#' @export
LearnerDensNonparametric<- R6::R6Class("LearnerDensNonparametric", inherit = LearnerDens,
  public = list(initialize = function(id = "dens.nonpar"){
    super$initialize(
      id = id,
      param_set = ParamSet$new(
        params = list(
          ParamDbl$new(id = "h",  tags = "train"),
          ParamUty$new(id = "model", default = "none", tags = "train"),
          ParamDbl$new(id = "weights", tags ="train"),
          ParamUty$new(id = "groups",  tags = "train"),
          ParamLgl$new(id = "add", default = TRUE, tags = "train"),
          ParamLgl$new(id = "band",  tags = "train"),
          ParamDbl$new(id = "delta", tags = "train"),
          ParamUty$new(id = "describe", default = FALSE, tags = "train"),
          ParamDbl$new(id = "df", tags = "train"),
          ParamInt$new(id = "diff.ord", default = 1, tags = "train"),
          ParamUty$new(id = "display", default = "none", tags = "train"),
          ParamLgl$new(id = "eval.grid", default = TRUE, tags = "train"),
          ParamDbl$new(id = "h.weights",  default = 1, tags = "train"),
          ParamUty$new(id = "hmult", default = 1, tags = "train"),
          ParamFct$new(id = "methods",  default = "normal", levels = c("normal", "cv", "sj", "df", "aicc"), tags = "train"),
          ParamDbl$new(id = "nbins",  tags = "train"),
          ParamDbl$new(id = "nboot", default = 100, tags = "train"),
          ParamDbl$new(id = "period",  tags = "train"),
          ParamInt$new(id = "poly.index", default = 1, tags = "train"),
          ParamLgl$new(id = "positive", default = FALSE, tags = "train"),
          ParamDbl$new(id = "se.breaks", tags = "train"),
          ParamLgl$new(id = "show.script", default = FALSE, tags = "train"),
          ParamUty$new(id = "structure.2d", tags = "train"),
          ParamLgl$new(id = "test", default = TRUE, tags = "train"),
          ParamUty$new(id = "verbose", default = 1, tags = "train")
          )),
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      predict_types = "pdf",
      packages = c("sm", "distr6")
    )},

    train_internal = function(task){

      pars = self$param_set$get_values(tag="train")
      data = as.numeric(unlist(task$data(cols = task$target_names)))

      saved_ctrl = sm::sm.options()
      on.exit(invoke(sm::sm.options, .args = saved_ctrl))
      sm::sm.options()

      pdf <- function(x1){}

      body(pdf) <- substitute({

        invoke(sm::sm.density, x = data, eval.points = x1,  display = "none", .args = pars)$estimate

      })

      Distribution$new(name = "sm KDE Gaussian",
                       short_name = "smKDEGaus",
                       pdf = pdf)
    },

    predict_internal = function(task){

      newdata = as.numeric(unlist(task$data(cols = task$target_names)))

      PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

    }
  ))


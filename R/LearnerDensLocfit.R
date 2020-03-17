#' @template dens_learner
#' @templateVar title Local
#' @templateVar fullname LearnerDensLocfit
#' @templateVar caller [locfit::density.lf()]
#'
#' @export
LearnerDensLocfit <- R6::R6Class("LearnerDensLocfit", inherit = LearnerDens,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(){
    super$initialize(
      id = "dens.locfit",
      param_set = ParamSet$new(
        params = list(
          ParamFct$new(id = "window", levels = c("tcub", "rect", "trwt",
                                                 "tria", "epan", "bisq",
                                                 "gaus"), default = "gaus", tags = "train"),
          ParamDbl$new(id = "width", tags = "train"),
          ParamDbl$new(id = "from", tags = "train"),
          ParamDbl$new(id = "to", tags = "train"),
          ParamDbl$new(id = "cut",  tags = "train"),
          ParamDbl$new(id = "deg", default = 0, tags = "train"),
          ParamFct$new(id = "link", default = "ident", tags = "train",
                       levels = c("ident","log","logit","inverse","sqrt","arcsin")),
          ParamFct$new(id = "kern", default = "tcub", tags = "train",
                       levels = c("rect","trwt","tria","epan","bisq","gauss","tcub")),
          ParamFct$new(id = "kt", default = "sph", tags = "train",
                       levels = c("sph","prod")),
          ParamLgl$new(id = "renorm", default = FALSE, tags = "train"),
          ParamInt$new(id = "maxk", default = 100, lower = 0, tags = "train"),
          ParamFct$new(id = "itype", levels = c("prod","mult","mlin","haz"), tags = "train"),
          ParamInt$new(id = "mint", default = 20, lower = 1, tags = "train"),
          ParamInt$new(id = "maxit", default = 20, lower = 1, tags = "train")
        )),
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      predict_types = "pdf",
      packages = c("locfit", "distr6")
    )}
  ),

  private = list(
    .train = function(task){

      pars = self$param_set$get_values(tag="train")

      data = task$truth()

      pdf <- function(x1){}

      body(pdf) <- substitute({

        invoke(locfit::density.lf, x = data, ev = x1, .args = pars)$y

      })

      Distribution$new(name = paste("LocFit Density", self$param_set$values$window),
                       short_name = paste0("LocFitDens",self$param_set$values$window),
                       pdf = pdf)
    },

    .predict = function(task){

      newdata = task$truth()

      PredictionDens$new(task = task, pdf = self$model$pdf(newdata))
    }
  )
)


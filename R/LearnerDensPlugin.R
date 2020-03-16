#' @template dens_learner
#' @templateVar title Plug-In Kernel
#' @templateVar fullname LearnerDensPlugin
#' @templateVar caller [plugdensity::plugin.density()]
#'
#' @export
LearnerDensPlugin <- R6::R6Class("LearnerDensPlugin", inherit = LearnerDens,
  public = list(initialize = function(id = "dens.plug"){
    super$initialize(
      id = id,
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      predict_types = "pdf",
      properties = "missings",
      packages = c("plugdensity", "distr6")
    )},

    .train = function(task){

      pdf <- function(x1){}
      body(pdf) <- substitute({
        invoke(plugdensity::plugin.density, x = data, xout = x1, na.rm = TRUE)$y
      }, list(data = task$truth()))


      Distribution$new(name = "Plugin KDE",
                       short_name = "PluginKDE",
                       pdf = pdf)
    },

    predict_internal = function(task){
      PredictionDens$new(task = task, pdf = self$model$pdf(task$truth()))
    }
  ))


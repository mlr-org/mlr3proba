#' @template dens_learner
#' @templateVar title Plug-In Kernel
#' @templateVar fullname LearnerDensPlugin
#' @templateVar caller [plugdensity::plugin.density()]
#'
#' @export
LearnerDensPlugin <- R6::R6Class("LearnerDensPlugin", inherit = LearnerDens,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(){
    super$initialize(
      id = "dens.plug",
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      predict_types = "pdf",
      properties = "missings",
      packages = c("plugdensity", "distr6")
    )}
  ),

  private = list(
    .train = function(task){

      pdf <- function(x1){}
      body(pdf) <- substitute({
        invoke(plugdensity::plugin.density, x = data, xout = x1, na.rm = TRUE)$y
      }, list(data = task$truth()))


      Distribution$new(name = "Plugin KDE",
                       short_name = "PluginKDE",
                       pdf = pdf)
    },

    .predict = function(task){
      PredictionDens$new(task = task, pdf = self$model$pdf(task$truth()))
    }
  )
)


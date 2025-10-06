#' @template dens_learner
#' @templateVar title Plug-in Kernel
#' @templateVar fullname LearnerDensPlugin
#' @templateVar caller [plugdensity::plugin.density()]
#'
#' @details
#' Kernel density estimation by "plug-in" bandwidth selection.
#'
#' @references
#' `r format_bib("engel_1994")`
#'
#' @template example_dens
#' @export
LearnerDensPlugin = R6Class("LearnerDensPlugin",
  inherit = LearnerDens,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        na.rm = p_lgl(default = FALSE, tags = "train")
      )

      super$initialize(
        id = "dens.plug",
        packages = c("plugdensity", "distr6"),
        feature_types = "numeric",
        predict_types = "pdf",
        param_set = param_set,
        properties = "missings",
        man = "mlr3proba::mlr_learners_dens.plug",
        label = "Kernel Density Estimation by Plug-In Bandwidth Selection"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")
      data = task$data()[[1]]

      pdf = function(x) {}
      body(pdf) = substitute({
        invoke(plugdensity::plugin.density, x = data, xout = x, na.rm = pars$na.rm)$y
      }, list(data = data))

      distr6::Distribution$new(
        name = "Plugin KDE",
        short_name = "PluginKDE",
        pdf = pdf,
        type = set6::Reals$new()
      )
    },

    .predict = function(task) {
      newdata = task$data()[[1]]
      list(pdf = self$model$pdf(newdata))
    }
  )
)

#' @include aaa.R
register_learner("dens.plug", LearnerDensPlugin)

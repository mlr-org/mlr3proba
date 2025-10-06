#' @template dens_learner
#' @templateVar title Nonparametric
#' @templateVar fullname LearnerDensNonparametric
#' @templateVar caller [sm::sm.density()]
#'
#' @references
#' `r format_bib("bowman_1997")`
#'
#' @template example_dens
#' @export
LearnerDensNonparametric = R6Class("LearnerDensNonparametric",
  inherit = LearnerDens,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        h = p_dbl(tags = "train"),
        group = p_uty(tags = "train"),
        delta = p_dbl(tags = "train"),
        h.weights = p_dbl(default = 1, tags = "train"),
        hmult = p_uty(default = 1, tags = "train"),
        method = p_fct(default = "normal",
          levels = c("normal", "cv", "sj", "df", "aicc"), tags = "train"),
        positive = p_lgl(default = FALSE, tags = "train"),
        verbose = p_uty(default = 1, tags = "train")
      )

      super$initialize(
        id = "dens.nonpar",
        packages = c("sm", "distr6"),
        feature_types = c("integer", "numeric"),
        predict_types = "pdf",
        param_set = param_set,
        properties = "weights",
        man = "mlr3proba::mlr_learners_dens.nonpar",
        label = "Nonparametric Density Estimation"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")
      pars$weights = private$.get_weights(task)
      data = task$data()[[1]]

      pdf = function(x) {}
      body(pdf) = substitute({
        invoke(sm::sm.density,
          x = data, eval.points = x, display = "none", show.script = FALSE,
          .args = pars)$estimate
      }, list(data = data))

      distr6::Distribution$new(
        name = "Nonparametric Density",
        short_name = "NonparDens",
        type = set6::Reals$new(),
        pdf = pdf
      )
    },

    .predict = function(task) {
      newdata = task$data()[[1]]
      list(pdf = self$model$pdf(newdata))
    }
  )
)

#' @include aaa.R
register_learner("dens.nonpar", LearnerDensNonparametric)

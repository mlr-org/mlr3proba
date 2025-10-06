#' @template dens_learner
#' @templateVar title KS Kernel
#' @templateVar fullname LearnerDensKDEks
#' @templateVar caller [ks::kde()]
#'
#' @references
#' `r format_bib("gramacki_2017")`
#'
#' @template example_dens
#' @export
LearnerDensKDEks = R6Class("LearnerDensKDEks",
  inherit = LearnerDens,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        h = p_dbl(lower = 0, tags = "train"),
        H = p_uty(tags = "train"),
        gridsize = p_uty(tags = "train"),
        gridtype = p_uty(tags = "train"),
        xmin = p_dbl(tags = "train"),
        xmax = p_dbl(tags = "train"),
        supp = p_dbl(default = 3.7, tags = "train"),
        binned = p_dbl(tags = "train"),
        bgridsize = p_uty(tags = "train"),
        positive = p_lgl(default = FALSE, tags = "train"),
        adj.positive = p_uty(tags = "train"),
        w = p_uty(tags = "train"),
        compute.cont = p_lgl(default = TRUE, tags = "train"),
        approx.cont = p_lgl(default = TRUE, tags = "train"),
        unit.interval = p_lgl(default = FALSE, tags = "train"),
        verbose = p_lgl(default = FALSE, tags = "train"),
        density = p_lgl(default = FALSE, tags = "train")
      )

      super$initialize(
        id = "dens.kde_ks",
        packages = c("ks", "distr6"),
        feature_types = c("integer", "numeric"),
        predict_types = "pdf",
        param_set = param_set,
        man = "mlr3proba::mlr_learners_dens.kde_ks",
        label = "Kernel Density Estimator (ks package)"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")
      data = task$data()[[1]]

      pdf = function(x){}
      body(pdf) = substitute({
        invoke(ks::kde, x = data, eval.points = x, .args = pars)$estimate
      })

      distr6::Distribution$new(
        name = "ks KDE",
        short_name = "ksKDE",
        pdf = pdf,
        type = set6::Reals$new()
      )
    },

    .predict = function(task) {
      list(pdf = self$model$pdf(task$data()[[1]]))
    }
  )
)

#' @include aaa.R
register_learner("dens.kde_ks", LearnerDensKDEks)

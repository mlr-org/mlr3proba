#' @template dens_learner
#' @templateVar title Mixed Data Types
#' @templateVar fullname LearnerDensMixed
#' @templateVar caller [np::npudens()]
#'
#' @references
#' `r format_bib("li_2003")`
#'
#' @template example_dens
#' @export
LearnerDensMixed = R6Class("LearnerDensMixed",
  inherit = LearnerDens,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        bws = p_uty(tags = "train"),
        ckertype = p_fct(
          default = "gaussian",
          levels = c("gaussian", "epanechnikov", "uniform"),
          tags = "train"),
        bwscaling = p_lgl(default = FALSE, tags = "train"),
        bwmethod = p_fct(
          default = "cv.ml",
          levels = c("cv.ml", "cv.ls", "normal-reference"),
          tags = "train"),
        bwtype = p_fct(
          default = "fixed",
          levels = c("fixed", "generalized_nn", "adaptive_nn"),
          tags = "train"),
        bandwidth.compute = p_lgl(default = FALSE, tags = "train"),
        ckerorder = p_int(default = 2, lower = 2, upper = 8, tags = "train"),
        remin = p_lgl(default = TRUE, tags = "train"),
        itmax = p_int(lower = 1, default = 10000, tags = "train"),
        nmulti = p_int(lower = 1, tags = "train"),
        ftol = p_dbl(default = 1.490116e-07, tags = "train"),
        tol = p_dbl(default = 1.490116e-04, tags = "train"),
        small = p_dbl(default = 1.490116e-05, tags = "train"),
        lbc.dir = p_dbl(default = 0.5, tags = "train"),
        dfc.dir = p_dbl(default = 0.5, tags = "train"),
        cfac.dir = p_uty(default = 2.5 * (3.0 - sqrt(5)), tags = "train"),
        initc.dir = p_dbl(default = 1.0, tags = "train"),
        lbd.dir = p_dbl(default = 0.1, tags = "train"),
        hbd.dir = p_dbl(default = 1, tags = "train"),
        dfac.dir = p_uty(default = 0.25 * (3.0 - sqrt(5)), tags = "train"),
        initd.dir = p_dbl(default = 1.0, tags = "train"),
        lbc.init = p_dbl(default = 0.1, tags = "train"),
        hbc.init = p_dbl(default = 2.0, tags = "train"),
        cfac.init = p_dbl(default = 0.5, tags = "train"),
        lbd.init = p_dbl(default = 0.1, tags = "train"),
        hbd.init = p_dbl(default = 0.9, tags = "train"),
        dfac.init = p_dbl(default = 0.37, tags = "train"),
        ukertype = p_fct(levels = c("aitchisonaitken", "liracine"), tags = "train"),
        okertype = p_fct(levels = c("wangvanryzin", "liracine"), tags = "train")
      )

      super$initialize(
        id = "dens.mixed",
        packages = c("np", "distr6"),
        feature_types = c("integer", "numeric"),
        predict_types = "pdf",
        param_set = param_set,
        man = "mlr3proba::mlr_learners_dens.mixed",
        label = "Kernel Density Estimator with Mixed Data Types"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")
      data = task$data()[[1]]

      pdf = function(x) {}
      body(pdf) = substitute({
        with_package("np", invoke(np::npudens,
          tdat = data.frame(data),
          edat = data.frame(x), .args = pars)$dens)
      })

      kernel = if (is.null(pars$ckertype)) "gaussian" else pars$ckertype

      distr6::Distribution$new(
        name = paste("Mixed KDE", kernel),
        short_name = paste0("MixedKDE_", kernel),
        pdf = pdf, type = set6::Reals$new()
      )
    },

    .predict = function(task) {
      newdata = task$data()[[1]]
      list(pdf = self$model$pdf(newdata))
    }
  )
)

#' @include aaa.R
register_learner("dens.mixed", LearnerDensMixed)

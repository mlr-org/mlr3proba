#' @title Survival Task Generator for Package 'coxed'
#'
#' @name mlr_task_generators_coxed
#'
#' @description
#' A [mlr3::TaskGenerator] calling [coxed::sim.survdata()].
#'
#' This generator creates a survival dataset using \CRANpkg{coxed}, and exposes
#' some parameters from the `sim.survdata()` function.
#' We don't include the parameters `X` (user-specified variables), `covariate`,
#' `low`, `high`, `compare`, `beta` and `hazard.fun` for this generator.
#' The latter means that no user-specified hazard function can be used and the
#' generated datasets always use the *flexible-hazard* method from the package.
#'
#' @templateVar id coxed
#' @template task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' if (requireNamespace("coxed", quietly = TRUE)) {
#'   library(mlr3)
#'
#'   # time horizon = 365 days, censoring proportion = 60%, 6 covariates normally
#'   # distributed with mean = 1 and sd = 2, independent censoring, no time-varying
#'   # effects
#'   gen = tgen("coxed", T = 365, type = "none", censor = 0.6, xvars = 6,
#'               mu = 1, sd = 2, censor.cond = FALSE)
#'   gen$generate(50)
#'
#'   # same as above, but with time-varying coefficients (interval censoring data)
#'   gen$param_set$set_values(type = "tvc")
#'   gen$generate(50)
#' }
TaskGeneratorCoxed = R6::R6Class("TaskGeneratorCoxed",
  inherit = TaskGenerator,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        T = p_dbl(lower = 1, default = 100), # time-horizon
        type = p_fct(default = "none", levels = c("none", "tvc", "tvbeta")), # time-varying effects
        knots = p_int(lower = 1, default = 8), # for flexible-hazard method
        spline = p_lgl(default = TRUE), # for flexible-hazard method
        xvars = p_int(lower = 1, default = 3), # number of covariates to generate
        mu = p_uty(default = 0), # mean for `xvars`
        sd = p_uty(default = 0.5), # sd for `xvars`
        censor = p_dbl(lower = 0, upper = 1, default = 0.1), # censoring proportion
        censor.cond = p_lgl(default = FALSE) # conditional censoring
      )

      super$initialize(
        id = "coxed",
        task_type = "surv",
        packages = "coxed",
        param_set = param_set,
        label = "Survival Data Generator from package 'coxed'",
        man = "mlr3proba::mlr_task_generators_coxed"
      )
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help(self$man)
    }
  ),

  private = list(
    .generate = function(n) {
      require_namespaces("coxed")

      pv = self$param_set$values
      data = invoke(coxed::sim.survdata, N = n, .args = pv)[[1]]
      data = map_at(data, "failed", as.integer)

      if (is.null(pv$type) || pv$type != "tvc") {
        task = TaskSurv$new(id = self$id, backend = data, time = "y",
                            event = "failed", type = "right")
      } else { # time-varying coefficients need interval type of censoring
        task = TaskSurv$new(id = self$id, backend = data, time = "start",
                            time2 = "end", event = "failed", type = "interval")
      }

      task
    }
  )
)

register_task_generator("coxed", TaskGeneratorCoxed)

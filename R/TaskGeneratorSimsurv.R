#' @title Survival Task Generator for Package 'simsurv'
#'
#' @name mlr_task_generators_simsurv
#'
#' @description
#' A [mlr3::TaskGenerator] calling [simsurv::simsurv()] from package \CRANpkg{simsurv}.
#'
#' This generator currently only exposes a small subset of the flexibility of \CRANpkg{simsurv},
#' and just creates a small dataset with the following numerical covariates:
#'
#' - `treatment`: Bernoulli distributed with hazard ratio `0.5`.
#' - `height`: Normally distributed with hazard ratio `1`.
#' - `weight`: normally distributed with hazard ratio `1`.
#'
#' See [simsurv::simsurv()] for an explanation of the hyperparameters.
#' Initial values for hyperparameters are `lambdas` = 0.1, `gammas` = 1.5 and `maxt` = 5.
#' The last one, by default generates samples which are administratively censored at \eqn{\tau = 5}, so increase this value if you want to change this.
#'
#' @templateVar id simsurv
#' @template task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' if (requireNamespace("simsurv", quietly = TRUE)) {
#'   # generate 20 samples with Weibull survival distribution
#'   gen = tgen("simsurv")
#'   task = gen$generate(20)
#'   head(task)
#'
#'   # generate 100 samples with exponential survival distribution and tau = 40
#'   gen = tgen("simsurv", dist = "exponential", gammas = NULL, maxt = 40)
#'   task = gen$generate(100)
#'   head(task)
#' }
TaskGeneratorSimsurv = R6Class("TaskGeneratorSimsurv",
  inherit = TaskGenerator,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        dist = p_fct(levels = c("weibull", "exponential", "gompertz"), default = "weibull"),
        lambdas = p_dbl(0, special_vals = list(NULL), tags = "required"),
        gammas = p_dbl(0, special_vals = list(NULL), tags = "required"),
        maxt = p_dbl(0, tags = "required")
      )
      ps$set_values(lambdas = 0.1, gammas = 1.5, maxt = 5)

      super$initialize(
        id = "simsurv",
        task_type = "surv",
        packages = "simsurv",
        param_set = ps,
        label = "Survival Data Generator from package 'simsurv'",
        man = "mlr3proba::mlr_task_generators_simsurv"
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
      require_namespaces("simsurv")

      pv = self$param_set$values
      covs = data.table(
        treatment = stats::rbinom(n, 1L, 0.5),
        height = stats::rnorm(n, 170, 10),
        weight = stats::rnorm(n, 80, 10)
      )
      betas = c(treatment = -0.5, height = -0.00004, weight = -0.00005)

      data = setDT(invoke(simsurv::simsurv, x = covs, betas = betas, .args = pv)) # nolint
      data = rcbind(data, covs)
      TaskSurv$new(id = "simsurv", backend = remove_named(data, "id"),
                   time = "eventtime", event = "status")
    }
  )
)

register_task_generator("simsurv", TaskGeneratorSimsurv)

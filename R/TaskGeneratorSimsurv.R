#' @title Survival Task Generator for Package 'simsurv'
#'
#' @name mlr_task_generators_simsurv
#'
#' @description
#' A [mlr3::TaskGenerator] calling [simsurv::simsurv()] from package \CRANpkg{simsurv}.
#'
#' This generator currently only exposes a small subset of the flexibility of \CRANpkg{simsurv},
#' and just creates a small data set with the following numerical covariates:
#'
#' * `treatment`: Bernoulli distributed with log hazard ratio `-0.5`.
#' * `height`: Normally distributed with log hazard ratio `1`.
#' * `weight`: normally distributed with log hazard ratio `0`.
#'
#' See [simsurv::simsurv()] for an explanation of the hyperparameters.
#'
#' @templateVar id simsurv
#' @template section_dictionary_task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' if (requireNamespace("simsurv", quietly = TRUE)) {
#' generator = mlr3::mlr_task_generators$get("simsurv")
#' task = generator$generate(20)
#' task$head()
#' }
TaskGeneratorSimsurv = R6Class("TaskGeneratorSimsurv",
  inherit = TaskGenerator,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamFct$new("dist", levels = c("weibull", "exponential", "gompertz"), default = "weibull"),
        ParamDbl$new("lambdas", lower = 0, default = 0.1, tags = "required"),
        ParamDbl$new("gammas", lower = 0, default = 1.5, tags = "required"),
        ParamDbl$new("maxt", lower = 0, default = 5, tags = "required")
      ))
      ps$values = list(lambdas = 0.1, gammas = 1.5, maxt = 5)

      super$initialize(id = "simsurv", task_type = "classif", packages = "mlbench", param_set = ps,
                       man = "mlr3::mlr_task_generators_simsurv")
    }
  ),

  private = list(
    .generate = function(n) {
      require_namespaces("simsurv")

      pv = self$param_set$values
      covs = data.table(
        treatment = stats::rbinom(n, 1L, 0.5),
        height = stats::rnorm(n, 180, 15),
        weight = stats::rnorm(n, 80, 10)
      )
      betas = c(treatment = -0.5, height = 1, weight = 0)

      data = setDT(invoke(simsurv::simsurv, x = covs, betas = , .args = pv)) # nolint
      data = rcbind(data, covs)
      TaskSurv$new("simsurv", remove_named(data, "id"), time = "eventtime", event = "status")
    }
  )
)

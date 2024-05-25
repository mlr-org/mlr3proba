#' @title Density Task Generator for Package 'distr6'
#'
#' @name mlr_task_generators_simdens
#'
#' @description
#' A [mlr3::TaskGenerator] calling [distr6::distrSimulate()].
#' See [distr6::distrSimulate()] for an explanation of the hyperparameters.
#' See [distr6::listDistributions()] for the names of the available distributions.
#'
#' @templateVar id simdens
#' @template task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' # generate 20 samples from a standard Normal distribution
#' dens_gen = tgen("simdens")
#' dens_gen$param_set
#'
#' task = dens_gen$generate(20)
#' head(task)
#'
#' # generate 50 samples from a Binomial distribution with specific parameters
#' dens_gen = tgen("simdens", distribution = "Bernoulli", pars = list(prob = 0.8))
#' task = dens_gen$generate(50)
#' task$data()[["x"]]
TaskGeneratorSimdens = R6::R6Class("TaskGeneratorSimdens",
  inherit = TaskGenerator,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        distribution = p_fct(default = "Normal", levels = distr6::listDistributions(simplify = TRUE)),
        pars = p_uty()
      )
      super$initialize(
        id = "simdens",
        task_type = "dens",
        packages = "distr6",
        param_set = param_set,
        label = "Density Generator for package 'distr6'",
        man = "mlr3::mlr_task_generators_simdens"
      )
    }
  ),

  private = list(
    .generate = function(n) {
      data = invoke(distr6::distrSimulate, n = n, .args = self$param_set$values)
      TaskDens$new(sprintf("%s_%i", self$id, n), data)
    }
  )
)

register_task_generator("simdens", TaskGeneratorSimdens)

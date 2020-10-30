#' @title Density Task Generator for Package 'distr6'
#'
#' @name mlr_task_generators_simdens
#'
#' @description
#' A [mlr3::TaskGenerator] calling [distr6::distrSimulate()].
#' See [distr6::distrSimulate()] for an explanation of the hyperparameters.
#'
#' @templateVar id simdens
#' @template section_dictionary_task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' generator = mlr3::mlr_task_generators$get("simdens")
#' task = generator$generate(20)
#' task$head()
TaskGeneratorSimdens = R6::R6Class("TaskGeneratorSimdens",
  inherit = TaskGenerator,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ParamSet$new(list(
        ParamFct$new("distribution", default = "Normal", levels = distr6::listDistributions(T)),
        ParamUty$new("pars")
      ))
      super$initialize(
        id = "simdens",
        task_type = "dens",
        packages = "distr6",
        param_set = param_set,
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

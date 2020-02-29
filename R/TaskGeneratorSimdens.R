#' @title Density Task Generator for Package 'distr6'
#'
#' @usage NULL
#' @aliases mlr_task_generators_simdens
#' @format [R6::R6Class] inheriting from [mlr3::TaskGenerator].
#'
#' @section Construction:
#' ```
#' TaskGeneratorSimdens$new()
#' mlr_task_generators$get("simdens")
#' tgen("simdens")
#' ```
#'
#' @description
#' A [mlr3::TaskGenerator] calling [distr6::distrSimulate()] from package \CRANpkg{simsurv}.
#' See [distr6::distrSimulate()] for an explanation of the hyperparameters.
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
    initialize = function() {
      param_set = ParamSet$new(list(
        ParamFct$new("distribution", default = "Normal", levels = distr6::listDistributions(T)),
        ParamUty$new("pars")
      ))
      super$initialize(id = "simdens",
                       task_type = "dens",
                       packages = "distr6",
                       param_set = param_set)
    }
  ),

  private = list(
    .generate = function(n) {
      data = invoke(distr6::distrSimulate, n = n, .args = self$param_set$values)
      data = data.frame(unimportant = runif(n), y = data)
      TaskDens$new(sprintf("%s_%i", self$id, n), data, target = "y")
    }
  )
)

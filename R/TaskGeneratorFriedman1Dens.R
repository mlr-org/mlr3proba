# TaskGeneratorFriedman1Dens = R6::R6Class("TaskGeneratorFriedman1Dens",
#                                  inherit = TaskGenerator,
#                                  public = list(
#                                    initialize = function() {
#                                      param_set = ParamSet$new(list(
#                                        ParamDbl$new("sd", lower = 0L, default = 1)
#                                      ))
#                                      super$initialize(id = "friedman1dens", "regr", "mlbench", param_set)
#                                    }
#                                  ),
#
#                                  private = list(
#                                    .generate = function(n) {
#                                      data = invoke(mlbench::mlbench.friedman1, n = n, .args = self$param_set$values)
#                                      colnames(data$x) = c(sprintf("important%i", 1:5), sprintf("unimportant%i", 1:5))
#                                      data = insert_named(as.data.table(data$x), list(y = data$y))
#                                      TaskDensity$new(sprintf("%s_%i", self$id, n), as.data.frame(data), target = "y")
#                                    }
#                                  )
# )

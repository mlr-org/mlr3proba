# MeasureDensity = R6Class("MeasureDensity", inherit = Measure, cloneable = FALSE,
#   public = list(
#     initialize = function(id, range, minimize = NA, aggregator = NULL, properties = character(), predict_type = "prob", task_properties = character(0L), na_score = FALSE, packages = character(0L)) {
#       super$initialize(id, task_type = "density", range = range, minimize = minimize, aggregator = aggregator,
#         properties = properties, predict_type = predict_type, task_properties = task_properties, na_score = na_score, packages = packages)
#     }
#   )
# )

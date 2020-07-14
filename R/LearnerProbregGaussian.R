# LearnerProbregGaussian = R6::R6Class("LearnerProbregGaussian", inherit = LearnerProbreg)
# LearnerProbregGaussian$set("public", "initialize", function(id = "probregr.gaussian") {
#   super$initialize(
#     id = id,
#     predict_types = "prob",
#     feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
#     properties = c("missings", "importance", "selected_features"),
#     packages = "distr6"
#   )
# })
# LearnerProbregGaussian$set("public", ".train", function(task) {
#   set_class(list(mean = mean(task$truth()), var = var(task$truth()),
#   features = task$feature_names),
#             "probreg.Gaussian_model")
# })
# LearnerProbregGaussian$set("public", ".predict", function(task) {
#   prob = VectorDistribution$new(distribution = Normal,
#                          params = data.table::data.table(mean = rep(self$model$mean,
#                          length(task$truth())),
#                                                          var = rep(self$model$var,
#                                                          length(task$truth()))))
#   PredictionProbreg$new(task = task, prob = prob)
# })
# LearnerProbregGaussian$set("public", "importance", function() {
#   if (is.null(self$model)) {
#     stopf("No model stored")
#   }
#   fn = self$model$features
#   named_vector(fn, 0)
# })
# LearnerProbregGaussian$set("public", "selected_features", function() {
#   character(0L)
# })

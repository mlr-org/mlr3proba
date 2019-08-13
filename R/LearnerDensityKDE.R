LearnerDensityKDE = R6::R6Class("LearnerDensityKDE", inherit = LearnerDensity)
LearnerDensityKDE$set("public", "initialize", function(id = "density.KDE") {
  super$initialize(
    id = id,
    param_set = ParamSet$new(),
    param_vals = list()
    predict_types = ,
    feature_types = ,
    properties = ,
    packages = ,
  )
})
LearnerDensityKDE$set("public", "train", function(task) NULL)
LearnerDensityKDE$set("public", "predict", function(task) NULL)

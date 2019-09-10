LearnerDensityKDE = R6::R6Class("LearnerDensityKDE", inherit = LearnerDensity)
LearnerDensityKDE$set("public", "initialize", function(id = "density.KDE") {
  super$initialize(
    id = id,
    param_set = ParamSet$new(list(ParamFct$new("kernel",levels = as.character(subset(distr6::listKernels(),
                                                                                     select="ShortName")[[1]]),
                                               tags = "train"),
                                  ParamDbl$new("bandwidth", lower = 0, tags = "train"))),
    param_vals = list(kernel = "Epan", bandwidth = 1),
    predict_types = "prob",
    feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
    properties = c("missings", "importance", "selected_features"),
    packages = "distr6"
  )
})
LearnerDensityKDE$set("public", "train_internal", function(task){
  pdf <- function(x1){}
  body(pdf) <- substitute({
    x1 <- matrix(x1, nrow = length(x1), ncol = rows)
    tru_mat <- matrix(truth, nrow = nrow(x1), ncol = rows, byrow = TRUE)
    if(nrow(x1) == 1)
      return(1/(rows * bw) * sum(kernel$pdf((x1 - tru_mat)/bw)))
    else
      return(1/(rows * bw) * colSums(apply((x1 - tru_mat)/bw,1,kernel$pdf)))
  }, list(rows = task$nrow,
          bw = self$param_set$values$bandwidth,
          kernel = get(as.character(subset(listKernels(),
                                           ShortName == self$param_set$values$kernel,
                                           ClassName)))$new(),
          truth = task$truth()))

  distribution = Distribution$new(name = "KDE Gaussian Estimate", short_name = "KDEGauss", pdf = pdf)
  set_class(list(distribution = distribution, features = task$feature_names), "density.KDE_model")

})
LearnerDensityKDE$set("public", "predict_internal", function(task){
  newdata = task$truth()
  prob = self$model$distribution$pdf(newdata)
  PredictionDensity$new(task = task, prob = prob)
})
LearnerDensityKDE$set("public", "importance", function(){
  if (is.null(self$model)) {
    stopf("No model stored")
  }
  fn = self$model$features
  named_vector(fn, 0)
})
LearnerDensityKDE$set("public", "selected_features", function(){
  character(0L)
})

LearnerDensHistogram <- R6::R6Class("LearnerDensHistogram", inherit = LearnerDens,
  public = list(initialize = function(id = "dens.hist"){
    super$initialize(
      id = id,
      param_set = ParamSet$new(
        params = list(
          ParamUty$new(id = "breaks", default = "Sturges", tags = "train")
        )),
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      predict_types = c("pdf","cdf"),
      packages = c("graphics", "distr6")
      )
    },

    train_internal = function(task){

      pars = self$param_set$get_values(tag="train")

      data = as.numeric(unlist(task$data(cols = task$target_names)))

      fit = invoke(.histogram, dat = data, .args = pars)

      set_class(list(distr = fit$distr, hist = fit$hist), "dens.hist")
    },

    predict_internal = function(task){
      newdata = as.numeric(unlist(task$data(cols = task$target_names)))
      PredictionDens$new(task = task, pdf = self$model$distr$pdf(newdata), cdf = self$model$distr$cdf(newdata))
    }
  ))



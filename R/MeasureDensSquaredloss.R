#' @template dens_measure
#' @templateVar title Squared loss
#' @templateVar inherit [MeasureDens]
#' @templateVar fullname MeasureDensSquared
#'
#' @description
#' Calculates the squared-loss for density estimation.
#'
#' @family Density estimation measures
#' @export
MeasureDensSquared = R6::R6Class("MeasureDensSquared",
  inherit = MeasureDens,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "dens.squared",
        range = c(- Inf, Inf),
        minimize = TRUE,
        predict_type = "pdf",
        properties = c("requires_task", "requires_learner"),
        man = "mlr3proba::mlr_measures_dens.squared"
      )
    }
  ),
  active = list(
    #' @field eps
    #' Returns `eps` parameter, see `initialize`.
    eps = function(eps) {
      if (missing(eps)) {
        return(private$.eps)
      } else {
        assertNumeric(eps)
        private$.eps = eps
      }
    }
  ),

  private = list(
    .eps = numeric(0),
    .score = function(prediction, learner, task, ...) {
      # return NA if learner not compatible
      # change `c("dens.kde")` to list of compatible learners
      data = task$truth()

      #calling the bandwidth
      bw = switch(learner$id,
             "dens.kde" = learner$param_set$values$bandwidth,
             "dens.kde_ks" = learner$param_set$values$h,
             "dens.nonpar" = learner$param_set$values$h,
             "dens.kde_kd" = learner$param_set$values$bw,
             "dens.mixed" = learner$param_set$values$bws
             )

      #vectorized the data
      dat <- sapply(data, function (x, y) ((x - y) / bw), y = data)

      if (learner$id == "dens.kde") {
        kernel = ifelse(is.null(learner$param_set$values$kernel), "Epan", learner$param_set$values$kernel)
      } else if (learner$id %in% c("dens.kde_ks", "dens.nonpar")) {
        kernel = "Norm"
      } else if (learner$id == "dens.mixed") {
        kernel =  ifelse(is.null(learner$param_set$values$ckertype), "gaussian",
                         learner$param_set$values$ckertype)
      } else if (learner$id == "dens.kde_kd") {
        kernel =  ifelse(is.null(learner$param_set$values$type_kernel), "n",
                         learner$param_set$values$type_kernel)
      }

      if (kernel %in% c("e", "epan", "Epan", "epanechnikov")) {
        squarednorm = distr6::Epanechnikov$new()$pdfSquared2Norm(x = dat)
      } else if (kernel %in% c("n", "gaussian", "Norm", "NormalKernel")) {
        squarednorm = distr6::NormalKernel$new()$pdfSquared2Norm(x = dat)
      } else if (kernel %in% c("t", "Tri", "TriangularKernel")) {
        squarednorm = distr6::TriangularKernel$new()$pdfSquared2Norm(x = dat)
      } else if (kernel %in% c("Sigm", "Sigmoid")) {
        squarednorm = distr6::NormalKernel$new()$pdfSquared2Norm(x = dat)
      } else if (kernel %in% c("Quartic", "Quart", "b")) {
        squarednorm = distr6::Quartic$new()$pdfSquared2Norm(x = dat)
      } else if (kernel %in% c("cos", "Cosine")) {
        squarednorm = distr6::Cosine$new()$pdfSquared2Norm(x = dat)
      } else if (kernel %in% c("logis", "Logistic")) {
        squarednorm = distr6::LogisticKernel$new()$pdfSquared2Norm(x = dat)
      } else if (kernel %in% c("Silv", "Silverman")) {
        squarednorm = distr6::Silverman$new()$pdfSquared2Norm(x = dat)
      } else if (kernel %in% c("Tric", "Tricube")) {
        squarednorm = distr6::Tricube$new()$pdfSquared2Norm(x = dat)
      } else if (kernel %in% c("Triw", "Triweight", "t")) {
        squarednorm = distr6::Triweight$new()$pdfSquared2Norm(x = dat)
      } else if (kernel %in% c("Unif", "UniformKernel", "uniform")) {
        squarednorm = distr6::UniformKernel$new()$pdfSquared2Norm(x = dat)
      } else {
        squarednorm = NA
      }

      #squarednorm =  distr6::UniformKernel$new()$pdf(0.1)

      pdf = prediction$pdf

      if (is.null(bw)) {
        score = NA
      } else {
        score = -2 * mean(pdf) + mean(squarednorm)/(length(data)^2 * bw)
      }

      return(score)

    }
  )
)

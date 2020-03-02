#' @template dens_learner
#' @templateVar title Kernel
#' @templateVar fullname LearnerDensKDE
#' @templateVar caller kernels implemented in \CRANpkg{distr6}
#'
#' @export
LearnerDensKDE = R6::R6Class("LearnerDensKDE", inherit = LearnerDens)
LearnerDensKDE$set("public", "initialize", function(id = "dens.kde") {
  ps = ParamSet$new(list(ParamFct$new("kernel",
                                      levels = subset(listKernels(),
                                                      select="ShortName")[[1]],
                                      default = "Norm",
                                      tags = "train"),
                               ParamUty$new("bandwidth",  tags = "train")))

  ps$values = list(kernel = "Norm")

  super$initialize(
    id = id,
    param_set = ps,
    predict_types = "pdf",
    feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
    properties = "missings",
    packages = "distr6"
  )
})
LearnerDensKDE$set("public", "train_internal", function(task){

  pdf <- function(x1){}

  body(pdf) <- substitute({

    ntrain = as.numeric(train)

    bws <- if(is.null(bw)){

      ExROT(kernel, rows)

    } else {bw}

    return(pdf <- (1/(rows * bws) * sum(kernel$pdf((x1 - ntrain)/bws))))

  }, list(rows = task$nrow,
          bw = self$param_set$values$bandwidth,
          kernel = get(as.character(subset(listKernels(),
                                           ShortName == self$param_set$values$kernel,
                                           ClassName)))$new(),
          train = task$truth()))

  Distribution$new(name = paste(self$param_set$values$kernel, "KDE"),
                   short_name = paste0(self$param_set$values$kernel, "_KDE"),
                   pdf = pdf)

})
LearnerDensKDE$set("public", "predict_internal", function(task){
  PredictionDens$new(task = task, pdf = self$model$pdf(task$truth()))
})

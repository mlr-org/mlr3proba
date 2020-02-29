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
                         ParamDbl$new("bandwidth", lower = 0, tags = "train", default = 1)))

  ps$values = list(kernel = "Norm", bandwidth = 1)

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
  #x1 is new data
  body(pdf) <- substitute({
    # x1 <- matrix(x1, nrow = length(x1), ncol = rows)
    ntrain = as.numeric(train)
    # truth <- matrix(truth, nrow = nrow(x1), ncol = rows, byrow = TRUE)
    # if(nrow(x1) == 1)
    return(1/(rows * bw) * sum(kernel$pdf((x1 - ntrain)/bw)))
    # else
    # return(1/(rows * bw) * colSums(apply((x1 - tru_mat)/bw,1,kernel$pdf)))

  }, list(rows = task$nrow,
          bw = self$param_set$values$bandwidth,
          kernel = get(as.character(subset(listKernels(),
                                           ShortName == self$param_set$values$kernel,
                                           ClassName)))$new(),
          train = task$truth()))

  Distribution$new(name = paste("KDE", self$param_set$values$kernel),
                   short_name = paste0("KDE",self$param_set$values$kernel), pdf = pdf)

})
LearnerDensKDE$set("public", "predict_internal", function(task){
  PredictionDens$new(task = task, pdf = self$model$pdf(task$truth()))
})

#' @template dens_learner
#' @templateVar title Kerdiest Kernel
#' @templateVar fullname LearnerDensKDEkd
#' @templateVar caller [kerdiest::kde()]
#'
#' @export
LearnerDensKDEkd <- R6::R6Class("LearnerDensKDEkd", inherit = LearnerDens,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(){
        ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "bw",  lower = 0, tags = "train"),
          ParamFct$new("type_kernel", levels = c("n", "e", "t", "b"),
                       default = "n", tags = "train")
          ))
      ps$values = list(type_kernel = "n")
      super$initialize(
        id = "dens.kdeKD",
        param_set = ps,
        feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = "pdf",
        packages = c("kerdiest", "distr6")
    )}
  ),

  private = list(
    .train = function(task){

      pars = self$param_set$get_values(tag="train")

      data = task$truth()

      pdf <- function(x1){}

      body(pdf) <- substitute({

        invoke(kerdiest::kde, vec_data = data, y = x1, .args = pars)$Estimated_values

      })

      Distribution$new(name = paste("kerdiest KDE",self$param_set$values$type_kernel),
                       short_name = paste0("kerdiestKDEKern_",self$param_set$values$type_kernel),
                       pdf = pdf)
    },

    .predict = function(task){

      newdata = task$truth()

      PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

    }
  )
)


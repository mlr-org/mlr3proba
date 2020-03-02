#' @template dens_learner
#' @templateVar title Kerdiest Kernel
#' @templateVar fullname LearnerDensKDEkd
#' @templateVar caller [kerdiest::kde()]
#'
#' @export
LearnerDensKDEkd <- R6::R6Class("LearnerDensKDEkd", inherit = LearnerDens,
  public = list(initialize = function(id = "dens.kdeKD"){
        ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "bw",  lower = 0, tags = "train"),
          ParamFct$new("type_kernel", levels = c("n", "e", "t", "b"),
                       default = "n", tags = "train")
          ))
      ps$values = list(type_kernel = "n")
      super$initialize(
        id = id,
        param_set = ps,
        feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = "pdf",
        packages = c("kerdiest", "distr6")
    )},

    train_internal = function(task){

      pars = self$param_set$get_values(tag="train")

      data = as.numeric(unlist(task$data(cols = task$target_names)))

      pdf <- function(x1){}

      body(pdf) <- substitute({

        invoke(kerdiest::kde, vec_data = data, y = x1, .args = pars)$Estimated_values

      })

      Distribution$new(name = paste("kerdiest KDE",self$param_set$values$type_kernel),
                       short_name = paste0("kerdiestKDE",self$param_set$values$type_kernel),
                       pdf = pdf)
    },

    predict_internal = function(task){

      newdata = as.numeric(unlist(task$data(cols = task$target_names)))

      PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

    }
  )
)


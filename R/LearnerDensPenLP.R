#' @template dens_learner
#' @templateVar title Logspline
#' @templateVar fullname LearnerDensPenLP
#' @templateVar caller [logspline::logspline()]
#'
#' @export
LearnerDensPenLP <- R6::R6Class("LearnerDensPenLP", inherit = LearnerDens,
  public = list(initialize = function(id = "dens.penLP"){
    super$initialize(
      id = id,
      param_set = ParamSet$new(list(
        ParamDbl$new(id = "maxknots", default = 0, tags = "train"),
        ParamDbl$new(id = "lbound", tags = "train"),
        ParamDbl$new(id = "ubound", tags = "train"),
        ParamUty$new(id = "knots", tags = "train"),
        ParamDbl$new(id = "nknots", default = 0, tags = "train"),
        ParamDbl$new(id = "mind", default = -1, tags ="train"),
        ParamLgl$new(id = "silent", default = TRUE, tags = "train"),
        ParamUty$new(id = "error.action", default = 2, tags = "train")
      )),
     # param_set = ps,
      feature_types =  c("logical", "integer", "numeric", "character", "factor", "ordered"),
      predict_types = "pdf",
      packages = c("logspline", "distr6")
    )},

    train_internal = function(task){

      data = as.numeric(unlist(task$data(cols = task$target_names)))

      pars = self$param_set$get_values(tag="train")

      pdf <- function(x1){}

      body(pdf) <- substitute({

       invoke(.DensLogspline, dat = data, test = x1, .args = pars)

        #  nknots = nk,
        # silent = s, maxknots = mk,
        # mind = m, error.action = ea))

      })
      # mk = self$param_set$values$maxknots,
      # nk =  self$param_set$values$nknots,
      # s =  self$param_set$values$silent,
      # m =  self$param_set$values$mind,
      # ea =  self$param_set$values$error.action))

      Distribution$new(name = "Logspline Density Penalized",
                       short_name = "LogsplineDens",
                       pdf = pdf)

    },

    predict_internal = function(task){

      newdata = as.numeric(unlist(task$data(cols = task$target_names)))

      PredictionDens$new(task = task, pdf = self$model$pdf(newdata))

    }
  ))

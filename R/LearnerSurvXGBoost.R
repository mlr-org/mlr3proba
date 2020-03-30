#' @template surv_learner
#' @templateVar title CoxPH models based on gradient boosted trees
#' @templateVar fullname LearnerSurvXGBoostCoxPH
#' @templateVar caller [xgboost::xgboost()]
#' @templateVar distr by [stats::predict()]
#'
#' @description Parameter \code{verbose} is set to \code{0}.
#' @description Parameter \code{objective} is set to \code{"survival:cox"}
#' @description Parameter \code{eval_metric} is set to \code{"cox-nloglik"}
#' @description Parameter \code{nrounds} is set to \code{100}, but this is arbitrary.
#' Please modify.
#'
#'
#' @references
#' \cite{mlr3proba}{chen_2016}
#'
#' @export
LearnerSurvXGBoostCoxPH = R6Class("LearnerSurvXGBoostCoxPH", inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamInt$new(id = "nrounds", tag = "train"),
          ParamDbl$new(id = "eta", default = 0.3, lower = 0, upper = 1, tags = "train"),
          ParamInt$new(id = "gamma", default = 0, lower = 0, tags = "train"),
          ParamInt$new(id = "max_depth", default = 6L, lower = 1L, tags = "train"),
          ParamInt$new(id = "min_child_weight", default = 1L, lower = 1L, tags = "train"),
          ParamInt$new(id = "max_delta_step", default = 0, lower = 0, tags = "train"),
          ParamInt$new(id = "subsample", default = 1, lower = 0, upper = 1, tags = "train"),
          ParamInt$new(id = "colsample_bytree", default = 1, lower = 0, upper = 1, tags = "train"),
          ParamDbl$new(id = "colsample_bynode", default = 1, lower = 0, upper = 1, tags = "train"),
          ParamDbl$new(id = "lambda", default = 1, lower = 1, tags = "train"),
          ParamInt$new(id = "alpha", default = 0, lower = 0, tags = "train"),
          ParamInt$new(id = "verbose", default = 1L, lower = 0L, upper = 2L, tags = "train"),
          ParamInt$new(id = "print_every_n", default = 1L, lower=1L, tags = "train"),
          # ParamInt$new(id = "early_stopping_rounds", default = NULL, tags = "train"),
          ParamLgl$new(id = "outputmargin", default = FALSE, tags = "predict"),
          # ParamInt$new(id = "ntreelimit", default = NULL, lower = 1L, tags = "predict"),
          ParamLgl$new(id = "predleaf", default = FALSE, tags = "predict"),
          ParamLgl$new(id = "predcontrib", default = FALSE, tags = "predict"),
          ParamLgl$new(id = "approxcontrib", default = FALSE, tags = "predict"),
          ParamLgl$new(id = "predinteraction", default = FALSE, tags = "predict"),
          ParamLgl$new(id = "reshape", default = FALSE, tags = "predict")
        )
      )

      ps$values = insert_named(ps$values, list(verbose = 0))
      ps$values = insert_named(ps$values, list(nrounds = 100))

      super$initialize(
        id            = "surv.XGBoostCoxPH",
        param_set     = ps,
        predict_types = c("crank", "lp"),
        feature_types = c("integer", "numeric", "factor", "ordered"),
        properties    = c("weights"),
        packages      = c("xgboost")
      )
    }
  ),

  private = list(
    .train = function(task) {

      pv = self$param_set$get_values(tags = "train")
      targets = task$target_names

      data <- task$data()
      time <- data[[task$target_names[[1]]]]
      status <- data[[task$target_names[[2]]]]
      label <- time
      label[status != 1] <- -1L * label[status != 1]
      mm <- model.matrix(
        ~ .,
        data = data[, task$feature_names, with = FALSE])[, -1, drop = FALSE]

      invoke(
        xgboost::xgboost,
        data     = mm,
        label    = label,
        objective = "survival:cox",
        eval_metric = "cox-nloglik",
        .args    = pv
      )

    },

    .predict = function(task) {
      data = task$data()
      newdata = model.matrix(
        ~ .,
        data = data[, task$feature_names, with = FALSE])[, -1, drop = FALSE]

      pv = self$param_set$get_values(tags = "predict")

      p = invoke(predict, self$model, newdata = newdata, .args = pv)
      # define WeightedDiscrete distr6 object from predicted survival function

      PredictionSurv$new(task = task, crank = p, lp = p)

    }
  )
)

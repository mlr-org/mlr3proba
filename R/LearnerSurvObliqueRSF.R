#' @template surv_learner
#' @templateVar title Oblique Random Survival Forest
#' @templateVar fullname LearnerSurvObliqueRSF
#' @templateVar caller [obliqueRSF::ORSF()]
#' @templateVar distr by [obliqueRSF::predict.orsf()]
#'
#' @description Parameter \code{verbose} is set to \code{FALSE}.
#'
#'
#' @references
#' \cite{mlr3proba}{jaeger_2019}
#'
#' @export
LearnerSurvObliqueRSF = R6Class("LearnerSurvObliqueRSF", inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "alpha", default = 0.5, tags = "train"),
          ParamInt$new(id = "ntree", default = 100L, lower = 1L, tags = "train"),
          ParamInt$new(id = "min_events_to_split_node", default = 5L, lower = 1L, tags = "train"),
          ParamInt$new(id = "min_obs_to_split_node", default = 10L, lower = 1L, tags = "train"),
          ParamInt$new(id = "min_obs_in_leaf_node", default = 5L, lower = 1L, tags = "train"),
          ParamInt$new(id = "min_events_in_leaf_node", default = 1L, lower = 1L, tags = "train"),
          ParamInt$new(id = "nsplit", default = 25L, lower = 1, tags = "train"),
          ParamDbl$new(id = "gamma", default = 0.5, lower = 1e-16, tags = "train"),
          ParamDbl$new(id = "max_pval_to_split_node", lower = 0, upper = 1, default = 0.5, tags = "train"),
          ParamInt$new(id = "mtry", lower = 1, tags = "train"),
          ParamInt$new(id = "dfmax", lower = 1, tags = "train"),
          ParamLgl$new(id = "use.cv", default = FALSE, tags = "train"),
          ParamLgl$new(id = "verbose", default = TRUE, tags = "train"),
          ParamInt$new(id = "radom_seed", tags = "train")
        )
      )

      ps$values = insert_named(ps$values, list(verbose = FALSE))

      super$initialize(
        id            = "surv.obliqueRSF",
        param_set     = ps,
        predict_types = c("crank", "distr"),
        feature_types = c("integer", "numeric", "factor", "ordered"),
        properties    = c("missings"),
        packages      = c("obliqueRSF", "distr6")
      )
    }
  ),

  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      targets = task$target_names

      invoke(
        obliqueRSF::ORSF,
        data     = task$data(),
        time     = targets[1L],
        status   = targets[2L],
        .args    = pv
      )
    },

    .predict = function(task) {


      newdata = task$data(cols = task$feature_names)
      pv      = self$param_set$get_values(tags = "predict")
      time    = self$model$data[[task$target_names[1]]]
      status  = self$model$data[[task$target_names[2]]]
      utime   = unique(time[status == 1])

      p = invoke(predict, self$model, newdata = newdata, times = utime, .args = pv)
      cdf = 1 - p
      # define WeightedDiscrete distr6 object from predicted survival function
      x = rep(list(data = data.frame(x = utime, cdf = 0)), task$nrow)
      for(i in seq_len(task$nrow))
        x[[i]]$cdf = cdf[i, ]

      distr = distr6::VectorDistribution$new(
        distribution = "WeightedDiscrete",
        params       = x,
        decorators   = c("CoreStatistics", "ExoticStatistics"))

      crank = as.numeric(sapply(x, function(y) sum(y[, 1] * c(y[, 2][1], diff(y[, 2])))))

      PredictionSurv$new(task = task, crank = crank, distr = distr)

    }
  )
)

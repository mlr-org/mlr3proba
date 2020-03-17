#' @template surv_learner
#' @templateVar title Rpart Survival Forest
#' @templateVar fullname LearnerSurvRpart
#' @templateVar caller [rpart::rpart()]
#' @templateVar distr using [pec::pecRpart()] and [pec::predictSurvProb()]
#'
#' @description
#' Parameter `xval` is set to 0 in order to save some computation time.
#'
#' @references
#' \cite{mlr3proba}{breiman_1984}
#'
#' @export
LearnerSurvRpart = R6Class("LearnerSurvRpart", inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamDbl$new("parms", default = 1, tags = "train"),
        ParamInt$new("minbucket", lower = 1L, tags = "train"),
        ParamInt$new("minsplit", default = 20L, lower = 1L, tags = "train"),
        ParamDbl$new("cp", default = 0.01, lower = 0, upper = 1, tags = "train"),
        ParamInt$new("maxcompete", default = 4L, lower = 0L, tags = "train"),
        ParamInt$new("maxsurrogate", default = 5L, lower = 0L, tags = "train"),
        ParamInt$new("maxdepth", default = 30L, lower = 1L, upper = 30L, tags = "train"),
        ParamInt$new(id = "usesurrogate", default = 2L, lower = 0L, upper = 2L, tags = "train"),
        ParamInt$new(id = "surrogatestyle", default = 0L, lower = 0L, upper = 1L, tags = "train"),
        ParamInt$new("xval", default = 10L, lower = 0L, tags = "train"),
        ParamUty$new("cost", tags = "train")
      ))
      ps$values = list(xval = 0L)

      super$initialize(
        id = "surv.rpart",
        param_set = ps,
        predict_types = c("crank", "distr"),
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = c("weights", "missings", "importance", "selected_features"),
        packages = c("rpart","distr6","survival")
      )
    },

    #' @description
    #' The importance scores are extracted from the model slot `variable.importance`.
    #' @return Named `numeric()`.
    importance = function() {
      if (is.null(self$model$fit$rpart)) {
        stopf("No model stored")
      }
      # importance is only present if there is at least on split
      sort(self$model$fit$rpart$variable.importance %??% set_names(numeric()), decreasing = TRUE)
    },

    #' @description
    #' Selected features are extracted from the model slot `frame$var`.
    #' @return `character()`.
    selected_features = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      unique(setdiff(self$model$fit$rpart$frame$var, "<leaf>"))
    }
  ),

  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      if ("weights" %in% task$properties) {
        pv = insert_named(pv, list(weights = task$weights$weight))
      }

      # The model is fit via the pec package to return the required models for composition to a
      # survival distribution.
      fit = invoke(pec::pecRpart, formula = task$formula(), data = task$data(), method = "exp", .args = pv)

      set_class(list(fit = fit, times = sort(unique(task$truth()[,1]))), "surv.rpart")
    },

    .predict = function(task) {
      newdata = task$data(cols = task$feature_names)

      cdf = 1 - invoke(pec::predictSurvProb, .args = list(object = self$model$fit, newdata = newdata,
                                                          times = self$model$times))

      # We assume that any NAs in prediction are due to the observation being dead. This certainly
      # looks like the case when looking through predictions - i.e. predictions are made for survival
      # for an observation until the first '0' is predicted, then NA is returned.
      cdf[is.na(cdf)] = 1
      # surv2 = t(apply(surv,1,function(x){
      #   if(any(is.na(x))){
      #     if(round(x[which(is.na(x))[1]-1]) == 0)
      #       x[is.na(x)] = 0
      #   }
      #   return(x)
      # }))


      # define WeightedDiscrete distr6 object from predicted survival function
      x = rep(list(data = data.frame(x = self$model$times, cdf = 0)), task$nrow)
      for(i in 1:task$nrow)
        x[[i]]$cdf = cdf[i, ]

      distr = distr6::VectorDistribution$new(distribution = "WeightedDiscrete", params = x,
                                             decorators = c("CoreStatistics", "ExoticStatistics"))

      crank = as.numeric(sapply(x, function(y) sum(y[,1] * c(y[,2][1], diff(y[,2])))))

      # note the ranking of lp and crank is identical
      PredictionSurv$new(task = task, crank = crank, distr = distr)
    }
  )
)

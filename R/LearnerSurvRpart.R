#' @title Survival Tree Learner
#'
#' @usage NULL
#' @aliases mlr_learners_surv.rpart
#' @format [R6::R6Class] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @section Construction:
#' ```
#' LearnerSurvRpart$new()
#' mlr_learners$get("surv.rpart")
#' lrn("surv.rpart")
#' ```
#'
#' @description
#' A [LearnerSurv] for a regression tree implemented in [rpart::rpart()] in package \CRANpkg{rpart}.
#' Parameter `xval` is set to 0 in order to save some computation time.
#'
#' @references
#' Breiman, L. (1984).
#' Classification and Regression Trees.
#' New York: Routledge.
#' \doi{10.1201/9781315139470}.
#'
#' @template seealso_learner
#' @export
LearnerSurvRpart = R6Class("LearnerSurvRpart", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("minsplit", default = 20L, lower = 1L, tags = "train"),
        ParamDbl$new("cp", default = 0.01, lower = 0, upper = 1, tags = "train"),
        ParamInt$new("maxcompete", default = 4L, lower = 0L, tags = "train"),
        ParamInt$new("maxsurrogate", default = 5L, lower = 0L, tags = "train"),
        ParamInt$new("maxdepth", default = 30L, lower = 1L, upper = 30L, tags = "train"),
        ParamInt$new("xval", default = 10L, lower = 0L, tags = "train")
      ))
      ps$values = list(xval = 0L)

      super$initialize(
        id = "surv.rpart",
        param_set = ps,
        predict_types = "risk",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = c("weights", "missings", "importance", "selected_features"),
        packages = c("rpart","distr6","survival")
      )
    },

    train_internal = function(task) {
      pv = self$param_set$get_values(tags = "train")
      if ("weights" %in% task$properties) {
        pv = insert_named(pv, list(weights = task$weights$weight))
      }
      fit = invoke(pec::pecRpart, formula = task$formula(), data = task$data(), method = "exp", .args = pv)

      set_class(list(fit = fit, times = sort(unique(task$truth()[,1]))), "surv.rpart")
    },

    predict_internal = function(task) {
      newdata = task$data(cols = task$feature_names)
      risk = unname(predict(self$model$fit$rpart, newdata = newdata, type = "vector"))
      surv = invoke(pec::predictSurvProb, .args = list(object = self$model$fit, newdata = newdata,
                                                          times = self$model$times))
      surv[is.na(surv)] = 0
      # surv2 = t(apply(surv,1,function(x){
      #   if(any(is.na(x))){
      #     if(round(x[which(is.na(x))[1]-1]) == 0)
      #       x[is.na(x)] = 0
      #   }
      #   return(x)
      # }))

      distr = suppressAll(apply(surv, 1, function(x)
        WeightedDiscrete$new(data.frame(x = self$model$times, cdf = 1 - x),
                             decorators = c(CoreStatistics, ExoticStatistics))))

      PredictionSurv$new(task = task, risk = risk, distr = distr)
    },

    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      # importance is only present if there is at least on split
      sort(self$model$variable.importance %??% set_names(numeric()), decreasing = TRUE)
    },

    selected_features = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      unique(setdiff(self$model$fit$rpart$frame$var, "<leaf>"))
    }
  )
)

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
#' A [LearnerSurv] for a regression tree implemented in [rpart::rpart()] in package \CRANpkg{rpart},
#' through [pec::pecRpart()] in package \CRANpkg{pec}.
#' Parameter `xval` is set to 0 in order to save some computation time.
#'
#' @details
#' The \code{distr} return type is defined by first predicting the survival function with [pec::predictSurvProb()].\cr
#' The \code{crank} return type is defined by the expectation of the survival distribution.
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
        predict_types = c("crank", "distr"),
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

      # The model is fit via the pec package to return the required models for composition to a
      # survival distribution.
      fit = invoke(pec::pecRpart, formula = task$formula(), data = task$data(), method = "exp", .args = pv)

      set_class(list(fit = fit, times = sort(unique(task$truth()[,1]))), "surv.rpart")
    },

    predict_internal = function(task) {
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
      distr_crank = suppressAll(apply(cdf, 1, function(x){
        distr = distr6::WeightedDiscrete$new(data.frame(x = self$model$times, cdf = x),
                             decorators = c(distr6::CoreStatistics, distr6::ExoticStatistics))

        # crank defined as mean of survival distribution.
        crank = distr$mean()

        return(list(distr = distr, crank = crank))
      }))

      PredictionSurv$new(task = task,
                         crank = as.numeric(unlist(distr_crank)[seq.int(2, length(distr_crank)*2, 2)]),
                         distr = unname(unlist(distr_crank)[seq.int(1, length(distr_crank)*2, 2)]))
    },

    importance = function() {
      if (is.null(self$model$fit$rpart)) {
        stopf("No model stored")
      }
      # importance is only present if there is at least on split
      sort(self$model$fit$rpart$variable.importance %??% set_names(numeric()), decreasing = TRUE)
    },

    selected_features = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      unique(setdiff(self$model$fit$rpart$frame$var, "<leaf>"))
    }
  )
)

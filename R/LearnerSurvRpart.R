#' @template surv_learner
#' @templateVar title Rpart Survival Trees
#' @templateVar fullname LearnerSurvRpart
#' @templateVar caller [rpart::rpart()]
#' @templateVar crank using [rpart::predict.rpart()]
#'
#' @description
#' Parameter `xval` is set to 0 in order to save some computation time.
#' Parameter `model` has been renamed to `keep_model`.
#'
#' @references
#' `r format_bib("breiman_1984")`
#'
#' @export
LearnerSurvRpart = R6Class("LearnerSurvRpart",
  inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        parms          = p_dbl(default = 1, tags = "train"),
        minbucket      = p_int(lower = 1L, tags = "train"),
        minsplit       = p_int(default = 20L, lower = 1L, tags = "train"),
        cp             = p_dbl(default = 0.01, lower = 0, upper = 1, tags = "train"),
        maxcompete     = p_int(default = 4L, lower = 0L, tags = "train"),
        maxsurrogate   = p_int(default = 5L, lower = 0L, tags = "train"),
        maxdepth       = p_int(default = 30L, lower = 1L, upper = 30L, tags = "train"),
        usesurrogate   = p_int(default = 2L, lower = 0L, upper = 2L, tags = "train"),
        surrogatestyle = p_int(default = 0L, lower = 0L, upper = 1L, tags = "train"),
        xval           = p_int(default = 10L, lower = 0L, tags = "train"),
        cost           = p_uty(tags = "train"),
        keep_model     = p_lgl(default = FALSE, tags = "train")
      )
      ps$values = list(xval = 0L)

      super$initialize(
        id = "surv.rpart",
        param_set = ps,
        predict_types = c("crank", "distr"),
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = c("weights", "missings", "importance", "selected_features"),
        packages = c("rpart", "distr6", "survival"),
        label = "Survival Tree",
        man = "mlr3proba::mlr_learners_surv.rpart"
      )
    },

    #' @description
    #' The importance scores are extracted from the model slot `variable.importance`.
    #' @return Named `numeric()`.
    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      # importance is only present if there is at least on split
      sort(self$model$variable.importance %??% set_names(numeric()), decreasing = TRUE)
    },

    #' @description
    #' Selected features are extracted from the model slot `frame$var`.
    #' @return `character()`.
    selected_features = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      unique(setdiff(self$model$frame$var, "<leaf>"))
    }
  ),

  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      names(pv) = replace(names(pv), names(pv) == "keep_model", "model")
      if ("weights" %in% task$properties) {
        pv = insert_named(pv, list(weights = task$weights$weight))
      }

      invoke(rpart::rpart,
        formula = task$formula(), data = task$data(),
        method = "exp", .args = pv)
    },

    .predict = function(task) {
      preds = invoke(predict, object = self$model, newdata = task$data(cols = task$feature_names))
      list(crank = preds)
    }
  )
)

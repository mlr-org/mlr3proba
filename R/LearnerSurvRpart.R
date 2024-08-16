#' @templateVar title Rpart Survival Trees
#' @templateVar fullname LearnerSurvRpart
#' @templateVar caller [rpart::rpart()]
#' @templateVar crank using [rpart::predict.rpart()]
#' @templateVar id surv.rpart
#' @template surv_learner
#'
#' @section Initial parameter values:
#'
#' - `xval` is set to 0 in order to save some computation time.
#' - `model` has been renamed to `keep_model`.
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
        minbucket      = p_int(1L, tags = "train"),
        minsplit       = p_int(1L, default = 20L, tags = "train"),
        cp             = p_dbl(0, 1, default = 0.01, tags = "train"),
        maxcompete     = p_int(0L, default = 4L, tags = "train"),
        maxsurrogate   = p_int(0L, default = 5L, tags = "train"),
        maxdepth       = p_int(1L, 30L, default = 30L, tags = "train"),
        usesurrogate   = p_int(0L, 2L, default = 2L, tags = "train"),
        surrogatestyle = p_int(0L, 1L, default = 0L, tags = "train"),
        xval           = p_int(0L, default = 10L, tags = "train"),
        cost           = p_uty(tags = "train"),
        keep_model     = p_lgl(default = FALSE, tags = "train"),
        use_weights = p_lgl(default = FALSE, tags = "train")
      )

      ps$set_values(xval = 0L)

      super$initialize(
        id = "surv.rpart",
        param_set = ps,
        predict_types = c("crank"),
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

      if (isTRUE(pv$use_weights)) {
        pv$weights = task$weights_learner$weight
      }
      pv$use_weights = NULL

      invoke(rpart::rpart, formula = task$formula(), data = task$data(), method = "exp", .args = pv)
    },

    .predict = function(task) {
      newdata = ordered_features(task, self)
      p = invoke(predict, object = self$model, newdata = newdata)

      list(crank = p)
    }
  )
)

register_learner("surv.rpart", LearnerSurvRpart)

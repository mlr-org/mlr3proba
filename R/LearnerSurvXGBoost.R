#' @template surv_learner
#' @templateVar title CoxPH models based on gradient boosted trees
#' @templateVar fullname LearnerSurvXgboost
#' @templateVar caller [xgboost::xgboost()]
#' @templateVar distr by [xgboost::predict.xgb.Booster()]
#'
#' @description
#' #' eXtreme Gradient Boosting regression for Cox PH model.
#' Calls [xgboost::xgb.train()] from package \CRANpkg{xgboost}.
#'
#' We changed the following defaults for this learner:
#' * Verbosity is reduced by setting `verbose` to `0`.
#' * Number of boosting iterations `nrounds` is set to `1`.
#' * The `objective` is set to `survival:cox`.
#' * The `eval_metric` is set to `cox-nloglik`
#'
#' @references
#' \cite{mlr3proba}{chen_2016}
#'
#' @export
LearnerSurvXgboost = R6Class("LearnerSurvXgboost",
  inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {

      ps = ParamSet$new(list(
        ParamFct$new("booster", default = "gbtree", levels = c("gbtree", "gblinear", "dart"), tags = "train"),
        ParamUty$new("watchlist", default = NULL, tags = "train"),
        ParamDbl$new("eta", default = 0.3, lower = 0, upper = 1, tags = "train"),
        ParamDbl$new("gamma", default = 0, lower = 0, tags = "train"),
        ParamInt$new("max_depth", default = 6L, lower = 0L, tags = "train"),
        ParamDbl$new("min_child_weight", default = 1, lower = 0, tags = "train"),
        ParamDbl$new("subsample", default = 1, lower = 0, upper = 1, tags = "train"),
        ParamDbl$new("colsample_bytree", default = 1, lower = 0, upper = 1, tags = "train"),
        ParamDbl$new("colsample_bylevel", default = 1, lower = 0, upper = 1, tags = "train"),
        ParamDbl$new("colsample_bynode", default = 1, lower = 0, upper = 1, tags = "train"),
        ParamInt$new("num_parallel_tree", default = 1L, lower = 1L, tags = "train"),
        ParamDbl$new("lambda", default = 1, lower = 0, tags = "train"),
        ParamDbl$new("lambda_bias", default = 0, lower = 0, tags = "train"),
        ParamDbl$new("alpha", default = 0, lower = 0, tags = "train"),
        ParamUty$new("objective", default = "reg:linear", tags = c("train", "predict")),
        ParamUty$new("eval_metric", default = "rmse", tags = "train"),
        ParamDbl$new("base_score", default = 0.5, tags = "train"),
        ParamDbl$new("max_delta_step", lower = 0, default = 0, tags = "train"),
        ParamDbl$new("missing", default = NA, tags = c("train", "predict"), special_vals = list(NA, NA_real_, NULL)),
        ParamInt$new("monotone_constraints", default = 0L, lower = -1L, upper = 1L, tags = "train"),
        ParamDbl$new("tweedie_variance_power", lower = 1, upper = 2, default = 1.5, tags = "train"),
        ParamInt$new("nthread", lower = 1L, tags = "train"),
        ParamInt$new("nrounds", lower = 1L, tags = "train"),
        ParamUty$new("feval", default = NULL, tags = "train"),
        ParamInt$new("verbose", default = 1L, lower = 0L, upper = 2L, tags = "train"),
        ParamInt$new("print_every_n", default = 1L, lower = 1L, tags = "train"),
        ParamInt$new("early_stopping_rounds", default = NULL, lower = 1L, special_vals = list(NULL), tags = "train"),
        ParamLgl$new("maximize", default = NULL, special_vals = list(NULL), tags = "train"),
        ParamFct$new("sample_type", default = "uniform", levels = c("uniform", "weighted"), tags = "train"),
        ParamFct$new("normalize_type", default = "tree", levels = c("tree", "forest"), tags = "train"),
        ParamDbl$new("rate_drop", default = 0, lower = 0, upper = 1, tags = "train"),
        ParamDbl$new("skip_drop", default = 0, lower = 0, upper = 1, tags = "train"),
        ParamLgl$new("one_drop", default = FALSE, tags = "train"),
        ParamFct$new("tree_method", default = "auto", levels = c("auto", "exact", "approx", "hist", "gpu_hist"), tags = "train"),
        ParamFct$new("grow_policy", default = "depthwise", levels = c("depthwise", "lossguide"), tags = "train"),
        ParamInt$new("max_leaves", default = 0L, lower = 0L, tags = "train"),
        ParamInt$new("max_bin", default = 256L, lower = 2L, tags = "train"),
        ParamUty$new("callbacks", default = list(), tags = "train"),
        ParamDbl$new("sketch_eps", default = 0.03, lower = 0, upper = 1, tags = "train"),
        ParamDbl$new("scale_pos_weight", default = 1, tags = "train"),
        ParamUty$new("updater", tags = "train"), # Default depends on the selected booster
        ParamLgl$new("refresh_leaf", default = TRUE, tags = "train"),
        ParamFct$new("feature_selector", default = "cyclic", levels = c("cyclic", "shuffle", "random", "greedy", "thrifty"), tags = "train"),
        ParamInt$new("top_k", default = 0, lower = 0, tags = "train"),
        ParamFct$new("predictor", default = "cpu_predictor", levels = c("cpu_predictor", "gpu_predictor"), tags = "train")
      ))
      # param deps
      ps$add_dep("print_every_n", "verbose", CondEqual$new(1L))
      ps$add_dep("sample_type", "booster", CondEqual$new("dart"))
      ps$add_dep("normalize_type", "booster", CondEqual$new("dart"))
      ps$add_dep("rate_drop", "booster", CondEqual$new("dart"))
      ps$add_dep("skip_drop", "booster", CondEqual$new("dart"))
      ps$add_dep("one_drop", "booster", CondEqual$new("dart"))
      ps$add_dep("tree_method", "booster", CondAnyOf$new(c("gbtree", "dart")))
      ps$add_dep("grow_policy", "tree_method", CondEqual$new("hist"))
      ps$add_dep("max_leaves", "grow_policy", CondEqual$new("lossguide"))
      ps$add_dep("max_bin", "tree_method", CondEqual$new("hist"))
      ps$add_dep("sketch_eps", "tree_method", CondEqual$new("approx"))
      ps$add_dep("feature_selector", "booster", CondEqual$new("gblinear"))
      ps$add_dep("top_k", "booster", CondEqual$new("gblinear"))
      ps$add_dep("top_k", "feature_selector", CondAnyOf$new(c("greedy", "thrifty")))

      ps$values = list(nrounds = 1L, verbose = 0L, eval_metric = "cox-nloglik")

      super$initialize(
        id = "surv.xgboost",
        param_set = ps,
        predict_types = c("crank", "lp"),
        feature_types = c("logical", "integer", "numeric"),
        properties = c("weights", "missings", "importance"),
        packages = c("xgboost")
      )
    },

    #' @description
    #' The importance scores are calculated with [xgboost::xgb.importance()].
    #'
    #' @return Named `numeric()`.
    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }

      imp = xgboost::xgb.importance(
        feature_names = self$model$features,
        model = self$model
      )
      set_names(imp$Gain, imp$Feature)
    }
  ),

  private = list(
    .train = function(task) {

      pars = self$param_set$get_values(tags = "train")
      pars[["objective"]] = "survival:cox"
      targets = task$target_names

      data = task$data(cols = task$feature_names)
      target = task$data(cols = task$target_names)
      label = target[[targets[1]]]
      status = target[[targets[2]]]
      label[status != 1] = -1L * label[status != 1]
      data = xgboost::xgb.DMatrix(
        data = data.matrix(data),
        label = label)

      if ("weights" %in% task$properties) {
        xgboost::setinfo(data, "weight", task$weights$weight)
      }

      if (is.null(pars$watchlist)) {
        pars$watchlist = list(train = data)
      }

      invoke(xgboost::xgb.train, data = data, .args = pars)

    },

    .predict = function(task) {
      pars = self$param_set$get_values(tags = "predict")
      model = self$model
      newdata = data.matrix(task$data(cols = task$feature_names))
      newdata = newdata[, model$feature_names, drop = FALSE]
      lp = log(invoke(predict, model, newdata = newdata, .args = pars))

      PredictionSurv$new(task = task, crank = lp, lp = lp)

    }
  )
)

#' @template surv_learner
#' @templateVar title L1 and L2 Penalized Estimation in GLMs
#' @templateVar fullname LearnerSurvPenalized
#' @templateVar caller [penalized::penalized()]
#' @templateVar distr using [penalized::predict()]
#'
#' @description The `penalized` and `unpenalized` arguments in the learner are implemented slightly
#' differently than in [penalized::penalized()]. Here, there is no parameter for `penalized` but
#' instead it is assumed that every variable is penalized unless stated in the `unpenalized` parameter,
#' see examples.
#'
#' @references
#' \cite{mlr3proba}{goeman_2009}
#'
#' @export
#' @template seealso_learner
#' @examples
#' library(mlr3)
#' task = tgen("simsurv")$generate(20)
#' learner = lrn("surv.penalized")
#' resampling = rsmp("cv", folds = 2)
#' resample(task, learner, resampling)
#'
#' # specifying penalized and unpenalized variables
#' task = tgen("simsurv")$generate(20)
#' learner = lrn("surv.penalized", unpenalized = c("height"))
#' learner$train(task)
#' learner$model@penalized
#' learner$model@unpenalized
LearnerSurvPenalized = R6Class("LearnerSurvPenalized", inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "surv.penalized",
        param_set = ParamSet$new(
          params = list(
            ParamUty$new(id = "unpenalized", tags = c("train","predict")),
            ParamUty$new(id = "lambda1", default = 0, tags = "train"),
            ParamUty$new(id = "lambda2", default = 0, tags = "train"),
            ParamLgl$new(id = "positive", default = FALSE, tags = "train"),
            ParamLgl$new(id = "fusedl", default = FALSE, tags = "train"),
            ParamDbl$new(id = "startbeta", tags = "train"),
            ParamDbl$new(id = "startgamma", tags = "train"),
            ParamInt$new(id = "steps", lower = 1L, default = 1L, tags = "train"),
            ParamDbl$new(id = "epsilon", default = 1.0e-10, lower = 0, upper = 1, tags = "train"),
            ParamInt$new(id = "maxiter", lower = 1, tags = "train"),
            ParamLgl$new(id = "standardize", default = FALSE, tags = "train"),
            ParamLgl$new(id = "trace", default = TRUE, tags = "train")
            )
          ),
        feature_types = c("integer", "numeric","factor","ordered"),
        predict_types = c("distr","crank"),
        packages = c("penalized","distr6")
        )
      }
    # importance = function() {
    #   if (is.null(self$model))
    #     stopf("No model stored")
    #
    #   # importance defined by decreasing fitted weights
    #   sort(self$model@weights, decreasing = TRUE)
    #   }
  ),

  private = list(
    .train = function(task) {

      # Checks missing data early to prevent crashing
      if(any(task$missings() > 0))
        stop("Missing data is not supported by ", self$id)

      # Changes the structure of the penalized and unpenalized parameters to be more user friendly.
      # Now the user supplies the column names as a vector and these are added to the formula as
      # required.
      pars = self$param_set$get_values(tags = "train")
      if (length(pars$unpenalized) == 0)
        penalized = formulate(rhs = task$feature_names)
      else {
        penalized = formulate(rhs = task$feature_names[task$feature_names %nin% pars$unpenalized])
        pars$unpenalized = formulate(rhs = pars$unpenalized)
      }

      suppressWarnings(suppressMessages((invoke(penalized::penalized, response = task$truth(), penalized = penalized,
                                                data = task$data(cols = task$feature_names), model = "cox", .args = pars))))
    },

    .predict = function(task) {
      # Again the penalized and unpenalized covariates are automatically converted to the
      # correct formula
      pars = self$param_set$get_values(tags = "predict")
      if(length(pars$unpenalized) == 0)
        penalized = formulate(rhs = task$feature_names)
      else{
        penalized = formulate(rhs = task$feature_names[task$feature_names %nin% pars$unpenalized])
        pars$unpenalized = formulate(rhs = pars$unpenalized)
      }

      surv = invoke(penalized::predict, self$model, penalized = penalized, data = task$data(cols = task$feature_names),
                    .args = pars)

      # define WeightedDiscrete distr6 object from predicted survival function
      x = rep(list(data = data.frame(x = surv@time, cdf = 0)), task$nrow)
      for(i in 1:task$nrow)
        x[[i]]$cdf = 1 - surv@curves[i ,]

      distr = distr6::VectorDistribution$new(distribution = "WeightedDiscrete", params = x,
                                             decorators = c("CoreStatistics", "ExoticStatistics"))

      crank = as.numeric(sapply(x, function(y) sum(y[,1] * c(y[,2][1], diff(y[,2])))))

      PredictionSurv$new(task = task, distr = distr, crank = crank)
    }
  )
)

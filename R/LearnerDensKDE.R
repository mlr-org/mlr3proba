#' @template dens_learner
#' @templateVar title Kernel
#' @templateVar fullname LearnerDensKDE
#' @templateVar caller kernels implemented in \CRANpkg{distr6}
#' @details The default bandwidth uses Silverman's rule-of-thumb for Gaussian kernels, however for
#' non-Gaussian kernels it is recommended to use \CRANpkg{mlr3tuning} to tune the bandwidth with
#' cross-validation. Other density learners can be used for automated bandwidth selection.
#' The default kernel is Epanechnikov (chosen to reduce dependencies).
#'
#' @references
#' `r format_bib("silverman_1986")`
#'
#' @export
LearnerDensKDE = R6::R6Class("LearnerDensKDE",
  inherit = LearnerDens,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamFct$new("kernel",
          levels = subset(distr6::listKernels(),
            select = "ShortName")[[1]],
          default = "Epan", tags = "train"),
        ParamDbl$new("bandwidth",
          lower = 0, tags = "train",
          special_vals = list("silver"))))

      ps$values = list(kernel = "Epan", bandwidth = "silver")

      super$initialize(
        id = "dens.kde",
        param_set = ps,
        predict_types = "pdf",
        feature_types = c("integer", "numeric"),
        properties = "missings",
        packages = "distr6",
        man = "mlr3proba::mlr_learners_dens.kde"
      )
    }
  ),

  private = list(
    .train = function(task) {

      if(self$param_set$values$kernel == "Norm" && !requireNamespace("pracma", quietly = TRUE)) {
        stop("{pracma} is required for Normal kernel, reverting to Epanechnikov.")
        self$param_set$values$kernel == "Epan"
      }

      data = task$data()[[1]]

      kernel = get(as.character(subset(
        distr6::listKernels(),
        ShortName == self$param_set$values$kernel,
        ClassName)))$new()


      bw = ifelse(self$param_set$values$bandwidth == "silver",
        0.9 * min(sd(data), stats::IQR(data, na.rm = TRUE) / 1.349, na.rm = TRUE) *
          length(data)^-0.2,
        self$param_set$values$bandwidth)

      pdf = function(x) {} #nolint

      body(pdf) = substitute({
        if (length(x) == 1) {
          return(1 / (rows * bw) * sum(kernel$pdf((x - train) / bw)))
        } else {
          x = matrix(x, nrow = length(x), ncol = rows)
          train_mat = matrix(train, nrow = nrow(x), ncol = rows, byrow = TRUE)
          return(1 / (rows * bw) * colSums(apply((x - train_mat) / bw, 1, kernel$pdf)))
        }
      }, list(
        rows = task$nrow,
        train = data,
        bw = bw,
        kernel = kernel))

      Distribution$new(
        name = paste(self$param_set$values$kernel, "KDE"),
        short_name = paste0(self$param_set$values$kernel, "_KDE"),
        type = set6::Reals$new(),
        pdf = pdf)
    },

    .predict = function(task) {
      list(pdf = self$model$pdf(task$data()[[1]]))
    }
  )
)

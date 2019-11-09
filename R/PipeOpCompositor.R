#' @title PipeOpCompositor
#'
#' @usage NULL
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Parent class for [`PipeOp`]s that composes (combines) multiple learners. Implements the `$train_internal()`
#' and `$predict_internal()` methods necessary for a `PipeOp`. The inputs and outputs will depend on
#' the exact composition defined in the child classes.
#'
#' @details
#' Ensembling is perhaps the most common form of composition, in which the predictions from multiple learners
#' are composed in some way. Composition is the abstract parent class as it allows a learner to be adapted
#' in an abstract way to create more complex results.
#'
#' @section Construction:
#' Note: This object is typically constructed via a derived class, e.g. [`PipeOpClassifAvg`] or [`PipeOpRegrAvg`].
#' ```
#' PipeOpCompositor$new(innum = 0, id, param_set = ParamSet$new(), param_vals = list(), packages = character(0), prediction_type = "Prediction")
#' ```
#'
#' * `innum` :: `numeric(1)`\cr
#'   Determines the number of input channels.
#'   If `innum` is 0 (default), a vararg input channel is created that can take an arbitrary number of inputs.
#' * `id` :: `character(1)`\cr
#'   Identifier of the resulting  object.
#' * `param_set` :: [`ParamSet`][paradox::ParamSet]\cr
#'   ("Hyper"-)Parameters in form of a [`ParamSet`][paradox::ParamSet] for the resulting [`PipeOp`].
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#' * `packages` :: `character`\cr
#'   Set of packages required for this `PipeOp`. These packages are loaded during `$train()` and `$predict()`, but not attached.
#'   Default `character(0)`.
#' * `prediction_type` :: `character(1)`\cr
#'   The `predict` entry of the `$input` and `$output` type specifications.
#'   Should be `"Prediction"` (default) or one of its subclasses, e.g. `"PredictionClassif"`, and correspond to the type accepted by
#'   `$train_internal()` and `$predict_internal()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpCompositor`] has multiple input channels depending on the `innum` construction argument, named `"input1"`, `"input2"`, ...
#' if `innum` is nonzero; if `innum` is 0, there is only one *vararg* input channel named `"..."`.
#' Input and output channels vary depending on the exact composition method used. Often (but not always)
#' only the [`Prediction`][mlr3::Prediction] is used in inputs and ouputs, producing `NULL` during training.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Internals:
#' The commonality of ensemble methods using [`PipeOpCompositor`] is that they take a `NULL`-input during training and save an empty `$state`. They can be
#' used following a set of [`PipeOpLearner`] [`PipeOp`]s to perform composition of fitted models. See e.g.
#' [`PipeOpClassifAvg`] and [`PipeOpDistrCompositor`] which both inherit from this class.
#'
#' Should it be necessary to use the output of preceding [`Learner`][mlr3::Learner]s
#' during the "training" phase, then [`PipeOpCompositor`] should not be used. In fact, if training time behaviour of a [`Learner`][mlr3::Learner] is important, then
#' one should use a [`PipeOpLearnerCV`] instead of a [`PipeOpLearner`], and the ensemble can be created with a [`Learner`][mlr3::Learner] encapsulated by a [`PipeOpLearner`].
#' See [`LearnerClassifAvg`] and [`LearnerRegrAvg`] for examples.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @family Ensembles
PipeOpCompositor = R6Class("PipeOpCompositor",
                         inherit = PipeOp,
                         public = list(
                           initialize = function(innum = 0, id, param_set = ParamSet$new(), param_vals = list(), packages = character(0), prediction_type = "Prediction") {
                             assert_integerish(innum, lower = 0)
                             inname = if (innum) rep_suffix("input", innum) else "..."
                             super$initialize(id, param_set = param_set, param_vals = param_vals, packages = packages,
                                              input = data.table(name = inname, train = "NULL", predict = prediction_type),
                                              output = data.table(name = "output", train = "NULL", predict = prediction_type))
                           },
                           train_internal = function(inputs) {
                             self$state = list()
                             list(NULL)
                           },
                           predict_internal = function(inputs) {
                             self$state = list()
                             list(NULL)
                           }
                         )
)

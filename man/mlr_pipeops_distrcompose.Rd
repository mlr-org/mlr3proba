% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PipeOpDistrCompositor.R
\name{mlr_pipeops_distrcompose}
\alias{mlr_pipeops_distrcompose}
\alias{PipeOpDistrCompositor}
\title{PipeOpDistrCompositor}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Estimates (or 'composes') a survival distribution from a predicted baseline
survival distribution (\code{distr}) and a linear predictor (\code{lp}) from two \link{PredictionSurv}s.

Compositor Assumptions:
\itemize{
\item The baseline \code{distr} is a discrete estimator, e.g. \link[=LearnerSurvKaplan]{surv.kaplan}.
\item The composed \code{distr} is of a linear form
}
}
\section{Dictionary}{

This \link[mlr3pipelines:PipeOp]{PipeOp} can be instantiated via the
\link[mlr3misc:Dictionary]{dictionary} \link[mlr3pipelines:mlr_pipeops]{mlr3pipelines::mlr_pipeops} or with the associated sugar
function \code{\link[mlr3pipelines:po]{mlr3pipelines::po()}}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{PipeOpDistrCompositor$new()
mlr_pipeops$get("distrcompose")
po("distrcompose")
}\if{html}{\out{</div>}}
}

\section{Input and Output Channels}{

\link{PipeOpDistrCompositor} has two input channels, \code{"base"} and \code{"pred"}.
Both input channels take \code{NULL} during training and \link{PredictionSurv} during prediction.

\link{PipeOpDistrCompositor} has one output channel named \code{"output"}, producing
\code{NULL} during training and a \link{PredictionSurv} during prediction.

The output during prediction is the \link{PredictionSurv} from the \code{"pred"} input
but with an extra (or overwritten) column for the \code{distr} predict type; which
is composed from the \code{distr} of \code{"base"} and the \code{lp} of \code{"pred"}.
If no \code{lp} predictions have been made or exist, then the \code{"pred"} is returned unchanged.
}

\section{State}{

The \verb{$state} is left empty (\code{list()}).
}

\section{Parameters}{

The parameters are:
\itemize{
\item \code{form} :: \code{character(1)} \cr
Determines the form that the predicted linear survival model should take. This is either,
accelerated-failure time, \code{aft}, proportional hazards, \code{ph}, or proportional odds, \code{po}.
Default \code{aft}.
\item \code{overwrite} :: \code{logical(1)} \cr
If \code{FALSE} (default) then if the "pred" input already has a \code{distr}, the compositor does
nothing and returns the given \link{PredictionSurv}. If \code{TRUE}, then the \code{distr} is overwritten
with the \code{distr} composed from \code{lp} - this is useful for changing the prediction
\code{distr} from one model form to another.
\item \code{scale_lp} :: \code{logical(1)} \cr
This option is only applicable to \code{form} equal to \code{"aft"}. If \code{TRUE}, it
min-max scales the linear prediction scores to be in the interval \eqn{[0,1]},
avoiding extrapolation of the baseline \eqn{S_0(t)} on the transformed time
points \eqn{\frac{t}{\exp(lp)}}, as these will be \eqn{\in [\frac{t}{e}, t]},
and so always smaller than the maximum time point for which we have estimated
\eqn{S_0(t)}.
Note that this is just a \strong{heuristic} to get reasonable results in the
case you observe survival predictions to be e.g. constant after the AFT
composition and it definitely provides no guarantee for creating calibrated
distribution predictions (as none of these methods do). Therefore, it is
set to \code{FALSE} by default.
}
}

\section{Internals}{

The respective \code{form}s above have respective survival distributions:
\deqn{aft: S(t) = S_0(\frac{t}{\exp(lp)})}
\deqn{ph: S(t) = S_0(t)^{\exp(lp)}}
\deqn{po: S(t) = \frac{S_0(t)}{\exp(-lp) + (1-\exp(-lp)) S_0(t)}}
where \eqn{S_0} is the estimated baseline survival distribution, and \eqn{lp} is the
predicted linear predictor.

For an example use of the \code{"aft"} composition using Kaplan-Meier as a baseline
distribution, see Norman et al. (2024).
}

\examples{
\dontshow{if (mlr3misc::require_namespaces(c("mlr3pipelines"), quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\dontrun{
  library(mlr3)
  library(mlr3pipelines)
  task = tsk("rats")

  base = lrn("surv.kaplan")$train(task)$predict(task)
  pred = lrn("surv.coxph")$train(task)$predict(task)
  # let's change the distribution prediction of Cox (Breslow-based) to an AFT form:
  pod = po("distrcompose", param_vals = list(form = "aft", overwrite = TRUE))
  pod$train(list(NULL, NULL)) # need to train first, even if nothing happens
  pod$predict(list(base = base, pred = pred))[[1]]
}
\dontshow{\}) # examplesIf}
}
\references{
Norman, A P, Li, Wanlu, Jiang, Wenyu, Chen, E B (2024).
\dQuote{deepAFT: A nonlinear accelerated failure time model with artificial neural network.}
\emph{Statistics in Medicine}.
\doi{10.1002/sim.10152}.
}
\seealso{
\link{pipeline_distrcompositor}

Other survival compositors: 
\code{\link{mlr_pipeops_compose_breslow_distr}},
\code{\link{mlr_pipeops_crankcompose}},
\code{\link{mlr_pipeops_responsecompose}}
}
\concept{survival compositors}
\section{Super class}{
\code{\link[mlr3pipelines:PipeOp]{mlr3pipelines::PipeOp}} -> \code{PipeOpDistrCompositor}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PipeOpDistrCompositor-new}{\code{PipeOpDistrCompositor$new()}}
\item \href{#method-PipeOpDistrCompositor-clone}{\code{PipeOpDistrCompositor$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mlr3pipelines" data-topic="PipeOp" data-id="help"><a href='../../mlr3pipelines/html/PipeOp.html#method-PipeOp-help'><code>mlr3pipelines::PipeOp$help()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3pipelines" data-topic="PipeOp" data-id="predict"><a href='../../mlr3pipelines/html/PipeOp.html#method-PipeOp-predict'><code>mlr3pipelines::PipeOp$predict()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3pipelines" data-topic="PipeOp" data-id="print"><a href='../../mlr3pipelines/html/PipeOp.html#method-PipeOp-print'><code>mlr3pipelines::PipeOp$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3pipelines" data-topic="PipeOp" data-id="train"><a href='../../mlr3pipelines/html/PipeOp.html#method-PipeOp-train'><code>mlr3pipelines::PipeOp$train()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PipeOpDistrCompositor-new"></a>}}
\if{latex}{\out{\hypertarget{method-PipeOpDistrCompositor-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PipeOpDistrCompositor$new(id = "distrcompose", param_vals = list())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{(\code{character(1)})\cr
Identifier of the resulting  object.}

\item{\code{param_vals}}{(\code{list()})\cr
List of hyperparameter settings, overwriting the hyperparameter settings that would
otherwise be set during construction.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PipeOpDistrCompositor-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PipeOpDistrCompositor-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PipeOpDistrCompositor$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}

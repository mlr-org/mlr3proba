% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipelines.R
\name{mlr_graphs_survbagging}
\alias{mlr_graphs_survbagging}
\alias{pipeline_survbagging}
\title{Survival Prediction Averaging Pipeline}
\usage{
pipeline_survbagging(
  learner,
  iterations = 10,
  frac = 0.7,
  avg = TRUE,
  weights = 1,
  graph_learner = FALSE
)
}
\arguments{
\item{learner}{\verb{[mlr3::Learner]|[mlr3pipelines::PipeOp]|[mlr3pipelines::Graph]} \cr
Either a \code{Learner} which will be wrapped in \link[mlr3pipelines:mlr_pipeops_learner]{mlr3pipelines::PipeOpLearner}, a \code{PipeOp} which will
be wrapped in \link[mlr3pipelines:Graph]{mlr3pipelines::Graph} or a \code{Graph} itself. Underlying \code{Learner} should be
\link{LearnerSurv}.}

\item{iterations}{(\code{integer(1)})\cr
Number of bagging iterations. Defaults to 10.}

\item{frac}{(\code{numeric(1)})\cr
Percentage of rows to keep during subsampling. See
\link[mlr3pipelines:mlr_pipeops_subsample]{PipeOpSubsample} for more information. Defaults to 0.7.}

\item{avg}{(\code{logical(1)})\cr
If \code{TRUE} (default) predictions are aggregated with \link{PipeOpSurvAvg}, otherwise returned
as multiple predictions. Can only be \code{FALSE} if \code{graph_learner = FALSE}.}

\item{weights}{(\code{numeric()})\cr
Weights for model avering, ignored if \code{avg = FALSE}. Default is uniform weighting,
see \link{PipeOpSurvAvg}.}

\item{graph_learner}{(\code{logical(1)})\cr
If \code{TRUE} returns wraps the \link[mlr3pipelines:Graph]{Graph} as a
\link[mlr3pipelines:mlr_learners_graph]{GraphLearner} otherwise (default) returns as a \code{Graph}.}
}
\value{
\link[mlr3pipelines:Graph]{mlr3pipelines::Graph} or \link[mlr3pipelines:mlr_learners_graph]{mlr3pipelines::GraphLearner}
}
\description{
Wrapper around \link[mlr3pipelines:mlr_pipeops_subsample]{PipeOpSubsample} and \link{PipeOpSurvAvg} to simplify \link[mlr3pipelines:Graph]{Graph} creation.
}
\details{
Bagging (Bootstrap AGGregatING) is the process of bootstrapping data and aggregating
the final predictions. Bootstrapping splits the data into \code{B} smaller datasets of a given size
and is performed with \link[mlr3pipelines:mlr_pipeops_subsample]{PipeOpSubsample}. Aggregation is
the sample mean of deterministic predictions and a
\link[distr6:MixtureDistribution]{MixtureDistribution} of distribution predictions. This can be
further enhanced by using a weighted average by supplying \code{weights}.
}
\section{Dictionary}{

This \link[mlr3pipelines:Graph]{Graph} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary}
\link[mlr3pipelines:mlr_graphs]{mlr_graphs} or with the associated sugar function \link[mlr3pipelines:ppl]{ppl()}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{mlr_graphs$get("survbagging")
ppl("survbagging")
}\if{html}{\out{</div>}}
}

\examples{
\dontshow{if (mlr3misc::require_namespaces(c("mlr3pipelines"), quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\dontrun{
  library("mlr3")
  library("mlr3pipelines")

  task = tsk("rats")
  pipe = ppl(
    "survbagging",
    learner = lrn("surv.coxph"),
    iterations = 5,
    graph_learner = FALSE
  )
  pipe$train(task)
  pipe$predict(task)
}
\dontshow{\}) # examplesIf}
}
\seealso{
Other pipelines: 
\code{\link{mlr_graphs_crankcompositor}},
\code{\link{mlr_graphs_distrcompositor}},
\code{\link{mlr_graphs_probregr}},
\code{\link{mlr_graphs_responsecompositor}},
\code{\link{mlr_graphs_survaverager}},
\code{\link{mlr_graphs_survtoclassif_IPCW}},
\code{\link{mlr_graphs_survtoclassif_disctime}}
}
\concept{pipelines}

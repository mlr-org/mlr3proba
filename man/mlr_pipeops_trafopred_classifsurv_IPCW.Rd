% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PipeOpPredClassifSurvIPCW.R
\name{mlr_pipeops_trafopred_classifsurv_IPCW}
\alias{mlr_pipeops_trafopred_classifsurv_IPCW}
\alias{PipeOpPredClassifSurvIPCW}
\title{PipeOpPredClassifSurvIPCW}
\description{
Transform \link[mlr3:PredictionClassif]{PredictionClassif} to \link{PredictionSurv}
using the \strong{I}nverse \strong{P}robability of \strong{C}ensoring \strong{W}eights (IPCW)
method by Vock et al. (2016).
}
\section{Dictionary}{

This \link[mlr3pipelines:PipeOp]{PipeOp} can be instantiated via the
\link[mlr3misc:Dictionary]{dictionary} \link[mlr3pipelines:mlr_pipeops]{mlr3pipelines::mlr_pipeops}
or with the associated sugar function \code{\link[mlr3pipelines:po]{mlr3pipelines::po()}}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{PipeOpPredClassifSurvIPCW$new()
mlr_pipeops$get("trafopred_classifsurv_IPCW")
po("trafopred_classifsurv_IPCW")
}\if{html}{\out{</div>}}
}

\section{Input and Output Channels}{

The input is a \link[mlr3:PredictionClassif]{PredictionClassif} and a
\link[data.table:data.table]{data.table} containing observed times,
censoring indicators and row ids, all generated by \link{PipeOpTaskSurvClassifIPCW}
during the prediction phase.

The output is the input \link[mlr3:PredictionClassif]{PredictionClassif} transformed
to a \link{PredictionSurv}.
Each input classification probability prediction corresponds to the
probability of having the event up to the specified cutoff time
\eqn{\hat{\pi}(\bold{X}_i) = P(T_i < \tau|\bold{X}_i)},
see Vock et al. (2016) and \link{PipeOpTaskSurvClassifIPCW}.
Therefore, these predictions serve as \strong{continuous risk scores} that can be
directly interpreted as \code{crank} predictions in the right-censored survival
setting. We also map them to the survival distribution prediction \code{distr},
at the specified cutoff time point \eqn{\tau}, i.e. as
\eqn{S_i(\tau) = 1 - \hat{\pi}(\bold{X}_i)}.
Survival measures that use the survival distribution (eg \link[=mlr_measures_surv.brier]{ISBS})
should be evaluated exactly at the cutoff time point \eqn{\tau}, see example.
}

\references{
Vock, M D, Wolfson, Julian, Bandyopadhyay, Sunayan, Adomavicius, Gediminas, Johnson, E P, Vazquez-Benitez, Gabriela, O'Connor, J P (2016).
\dQuote{Adapting machine learning techniques to censored time-to-event health record data: A general-purpose approach using inverse probability of censoring weighting.}
\emph{Journal of Biomedical Informatics}, \bold{61}, 119--131.
\doi{https://doi.org/10.1016/j.jbi.2016.03.009}, \url{https://www.sciencedirect.com/science/article/pii/S1532046416000496}.
}
\seealso{
\link{pipeline_survtoclassif_IPCW}

Other Transformation PipeOps: 
\code{\link{mlr_pipeops_trafopred_classifsurv_disctime}},
\code{\link{mlr_pipeops_trafopred_regrsurv_pem}},
\code{\link{mlr_pipeops_trafotask_survclassif_IPCW}},
\code{\link{mlr_pipeops_trafotask_survclassif_disctime}},
\code{\link{mlr_pipeops_trafotask_survregr_pem}}
}
\concept{Transformation PipeOps}
\section{Super class}{
\code{\link[mlr3pipelines:PipeOp]{mlr3pipelines::PipeOp}} -> \code{PipeOpPredClassifSurvIPCW}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{predict_type}}{(\code{character(1)})\cr
Returns the active predict type of this PipeOp, which is \code{"crank"}}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PipeOpPredClassifSurvIPCW-new}{\code{PipeOpPredClassifSurvIPCW$new()}}
\item \href{#method-PipeOpPredClassifSurvIPCW-clone}{\code{PipeOpPredClassifSurvIPCW$clone()}}
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
\if{html}{\out{<a id="method-PipeOpPredClassifSurvIPCW-new"></a>}}
\if{latex}{\out{\hypertarget{method-PipeOpPredClassifSurvIPCW-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PipeOpPredClassifSurvIPCW$new(id = "trafopred_classifsurv_IPCW")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{(character(1))\cr
Identifier of the resulting object.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PipeOpPredClassifSurvIPCW-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PipeOpPredClassifSurvIPCW-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PipeOpPredClassifSurvIPCW$clone(deep = FALSE)}\if{html}{\out{</div>}}
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

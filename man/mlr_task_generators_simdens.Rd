% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TaskGeneratorSimdens.R
\name{mlr_task_generators_simdens}
\alias{mlr_task_generators_simdens}
\alias{TaskGeneratorSimdens}
\title{Density Task Generator for Package 'distr6'}
\description{
A \link[mlr3:TaskGenerator]{mlr3::TaskGenerator} calling \code{\link[distr6:distrSimulate]{distr6::distrSimulate()}}.
See \code{\link[distr6:distrSimulate]{distr6::distrSimulate()}} for an explanation of the hyperparameters.
See \code{\link[distr6:listDistributions]{distr6::listDistributions()}} for the names of the available distributions.
}
\section{Dictionary}{

This \link[mlr3:TaskGenerator]{TaskGenerator} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary} \link[mlr3:mlr_task_generators]{mlr_task_generators} or with the associated sugar function \link[mlr3:mlr_sugar]{tgen()}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{mlr_task_generators$get("simdens")
tgen("simdens")
}\if{html}{\out{</div>}}
}

\section{Parameters}{
\tabular{llll}{
   Id \tab Type \tab Default \tab Levels \cr
   distribution \tab character \tab Normal \tab Arcsine, Arrdist, Bernoulli, Beta, BetaNoncentral, Binomial, Categorical, Cauchy, ChiSquared, ChiSquaredNoncentral, \link{...} \cr
   pars \tab untyped \tab - \tab  \cr
}
}

\examples{
# generate 20 samples from a standard Normal distribution
dens_gen = tgen("simdens")
dens_gen$param_set

task = dens_gen$generate(20)
head(task)

# generate 50 samples from a Binomial distribution with specific parameters
dens_gen = tgen("simdens", distribution = "Bernoulli", pars = list(prob = 0.8))
task = dens_gen$generate(50)
task$data()[["x"]]
}
\seealso{
\itemize{
\item \link[mlr3misc:Dictionary]{Dictionary} of \link[mlr3:TaskGenerator]{TaskGenerators}: \link[mlr3:mlr_task_generators]{mlr3::mlr_task_generators}
\item \code{as.data.table(mlr_task_generators)}  for a table of available \link[mlr3:TaskGenerator]{TaskGenerators} in the running session
}

Other TaskGenerator: 
\code{\link{mlr_task_generators_coxed}},
\code{\link{mlr_task_generators_simsurv}}
}
\concept{TaskGenerator}
\section{Super class}{
\code{\link[mlr3:TaskGenerator]{mlr3::TaskGenerator}} -> \code{TaskGeneratorSimdens}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-TaskGeneratorSimdens-new}{\code{TaskGeneratorSimdens$new()}}
\item \href{#method-TaskGeneratorSimdens-help}{\code{TaskGeneratorSimdens$help()}}
\item \href{#method-TaskGeneratorSimdens-clone}{\code{TaskGeneratorSimdens$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="TaskGenerator" data-id="format"><a href='../../mlr3/html/TaskGenerator.html#method-TaskGenerator-format'><code>mlr3::TaskGenerator$format()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="TaskGenerator" data-id="generate"><a href='../../mlr3/html/TaskGenerator.html#method-TaskGenerator-generate'><code>mlr3::TaskGenerator$generate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="TaskGenerator" data-id="print"><a href='../../mlr3/html/TaskGenerator.html#method-TaskGenerator-print'><code>mlr3::TaskGenerator$print()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TaskGeneratorSimdens-new"></a>}}
\if{latex}{\out{\hypertarget{method-TaskGeneratorSimdens-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaskGeneratorSimdens$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TaskGeneratorSimdens-help"></a>}}
\if{latex}{\out{\hypertarget{method-TaskGeneratorSimdens-help}{}}}
\subsection{Method \code{help()}}{
Opens the corresponding help page referenced by field \verb{$man}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaskGeneratorSimdens$help()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TaskGeneratorSimdens-clone"></a>}}
\if{latex}{\out{\hypertarget{method-TaskGeneratorSimdens-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaskGeneratorSimdens$clone(deep = FALSE)}\if{html}{\out{</div>}}
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

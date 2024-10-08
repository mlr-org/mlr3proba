% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MeasureSurvCindex.R
\name{mlr_measures_surv.cindex}
\alias{mlr_measures_surv.cindex}
\alias{MeasureSurvCindex}
\title{Concordance Statistics Survival Measure}
\description{
Calculates weighted concordance statistics, which, depending on the chosen
weighting method (\code{weight_meth}) and tied times parameter (\code{tiex}), are
equivalent to several proposed methods.
By default, no weighting is applied and this is equivalent to Harrell's C-index.
}
\details{
For the Kaplan-Meier estimate of the \strong{training survival} distribution (\eqn{S}),
and the Kaplan-Meier estimate of the \strong{training censoring} distribution (\eqn{G}),
we have the following options for time-independent concordance statistics
(C-indexes) given the weighted method:

\code{weight_meth}:
\itemize{
\item \code{"I"} = No weighting. (Harrell)
\item \code{"GH"} = Gonen and Heller's Concordance Index
\item \code{"G"} = Weights concordance by \eqn{1/G}.
\item \code{"G2"} = Weights concordance by \eqn{1/G^2}. (Uno et al.)
\item \code{"SG"} = Weights concordance by \eqn{S/G} (Shemper et al.)
\item \code{"S"} = Weights concordance by \eqn{S} (Peto and Peto)
}

The last three require training data. \code{"GH"} is only applicable to \link{LearnerSurvCoxPH}.

The implementation is slightly different from \link[survival:concordance]{survival::concordance}.
Firstly this implementation is faster, and secondly the weights are computed
on the training dataset whereas in \link[survival:concordance]{survival::concordance} the weights are
computed on the same testing data.
}
\section{Dictionary}{

This \link[mlr3:Measure]{Measure} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary}
\link[mlr3:mlr_measures]{mlr_measures} or with the associated sugar function \link[mlr3:mlr_sugar]{msr()}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{MeasureSurvCindex$new()
mlr_measures$get("surv.cindex")
msr("surv.cindex")
}\if{html}{\out{</div>}}
}

\section{Parameters}{
\tabular{lllll}{
   Id \tab Type \tab Default \tab Levels \tab Range \cr
   t_max \tab numeric \tab - \tab  \tab \eqn{[0, \infty)}{[0, Inf)} \cr
   p_max \tab numeric \tab - \tab  \tab \eqn{[0, 1]}{[0, 1]} \cr
   weight_meth \tab character \tab I \tab I, G, G2, SG, S, GH \tab - \cr
   tiex \tab numeric \tab 0.5 \tab  \tab \eqn{[0, 1]}{[0, 1]} \cr
   eps \tab numeric \tab 0.001 \tab  \tab \eqn{[0, 1]}{[0, 1]} \cr
}
}

\section{Meta Information}{

\itemize{
\item Type: \code{"surv"}
\item Range: \eqn{[0, 1]}{[0, 1]}
\item Minimize: \code{FALSE}
\item Required prediction: \code{crank}
}
}

\section{Parameter details}{

\itemize{
\item \code{eps} (\code{numeric(1)})\cr
Very small number to substitute zero values in order to prevent errors
in e.g. log(0) and/or division-by-zero calculations.
Default value is 0.001.
}


\itemize{
\item \code{t_max} (\code{numeric(1)})\cr
Cutoff time (i.e. time horizon) to evaluate concordance up to.
\item \code{p_max} (\code{numeric(1)})\cr
The proportion of censoring to evaluate concordance up to in the given dataset.
When \code{t_max} is specified, this parameter is ignored.
\item \code{weight_meth} (\code{character(1)})\cr
Method for weighting concordance. Default \code{"I"} is Harrell's C. See details.
\item \code{tiex} (\code{numeric(1)})\cr
Weighting applied to tied rankings, default is to give them half (0.5) weighting.
}
}

\examples{
library(mlr3)
task = tsk("rats")
learner = lrn("surv.coxph")
part = partition(task) # train/test split
learner$train(task, part$train)
p = learner$predict(task, part$test)

# Harrell's C-index
p$score(msr("surv.cindex")) # same as `p$score()`

# Uno's C-index
p$score(msr("surv.cindex", weight_meth = "G2"),
        task = task, train_set = part$train)

# Harrell's C-index evaluated up to a specific time horizon
p$score(msr("surv.cindex", t_max = 97))
# Harrell's C-index evaluated up to the time corresponding to 30\% of censoring
p$score(msr("surv.cindex", p_max = 0.3))

}
\references{
Peto, Richard, Peto, Julian (1972).
\dQuote{Asymptotically efficient rank invariant test procedures.}
\emph{Journal of the Royal Statistical Society: Series A (General)}, \bold{135}(2), 185--198.

Harrell, E F, Califf, M R, Pryor, B D, Lee, L K, Rosati, A R (1982).
\dQuote{Evaluating the yield of medical tests.}
\emph{Jama}, \bold{247}(18), 2543--2546.

Goenen M, Heller G (2005).
\dQuote{Concordance probability and discriminatory power in proportional hazards regression.}
\emph{Biometrika}, \bold{92}(4), 965--970.
\doi{10.1093/biomet/92.4.965}.

Schemper, Michael, Wakounig, Samo, Heinze, Georg (2009).
\dQuote{The estimation of average hazard ratios by weighted Cox regression.}
\emph{Statistics in Medicine}, \bold{28}(19), 2473--2489.
\doi{10.1002/sim.3623}.

Uno H, Cai T, Pencina MJ, D'Agostino RB, Wei LJ (2011).
\dQuote{On the C-statistics for evaluating overall adequacy of risk prediction procedures with censored survival data.}
\emph{Statistics in Medicine}, n/a--n/a.
\doi{10.1002/sim.4154}.
}
\seealso{
Other survival measures: 
\code{\link{mlr_measures_surv.calib_alpha}},
\code{\link{mlr_measures_surv.calib_beta}},
\code{\link{mlr_measures_surv.chambless_auc}},
\code{\link{mlr_measures_surv.dcalib}},
\code{\link{mlr_measures_surv.graf}},
\code{\link{mlr_measures_surv.hung_auc}},
\code{\link{mlr_measures_surv.intlogloss}},
\code{\link{mlr_measures_surv.logloss}},
\code{\link{mlr_measures_surv.mae}},
\code{\link{mlr_measures_surv.mse}},
\code{\link{mlr_measures_surv.nagelk_r2}},
\code{\link{mlr_measures_surv.oquigley_r2}},
\code{\link{mlr_measures_surv.rcll}},
\code{\link{mlr_measures_surv.rmse}},
\code{\link{mlr_measures_surv.schmid}},
\code{\link{mlr_measures_surv.song_auc}},
\code{\link{mlr_measures_surv.song_tnr}},
\code{\link{mlr_measures_surv.song_tpr}},
\code{\link{mlr_measures_surv.uno_auc}},
\code{\link{mlr_measures_surv.uno_tnr}},
\code{\link{mlr_measures_surv.uno_tpr}},
\code{\link{mlr_measures_surv.xu_r2}}
}
\concept{survival measures}
\section{Super classes}{
\code{\link[mlr3:Measure]{mlr3::Measure}} -> \code{\link[mlr3proba:MeasureSurv]{mlr3proba::MeasureSurv}} -> \code{MeasureSurvCindex}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-MeasureSurvCindex-new}{\code{MeasureSurvCindex$new()}}
\item \href{#method-MeasureSurvCindex-clone}{\code{MeasureSurvCindex$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="aggregate"><a href='../../mlr3/html/Measure.html#method-Measure-aggregate'><code>mlr3::Measure$aggregate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="format"><a href='../../mlr3/html/Measure.html#method-Measure-format'><code>mlr3::Measure$format()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="help"><a href='../../mlr3/html/Measure.html#method-Measure-help'><code>mlr3::Measure$help()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="score"><a href='../../mlr3/html/Measure.html#method-Measure-score'><code>mlr3::Measure$score()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3proba" data-topic="MeasureSurv" data-id="print"><a href='../../mlr3proba/html/MeasureSurv.html#method-MeasureSurv-print'><code>mlr3proba::MeasureSurv$print()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MeasureSurvCindex-new"></a>}}
\if{latex}{\out{\hypertarget{method-MeasureSurvCindex-new}{}}}
\subsection{Method \code{new()}}{
This is an abstract class that should not be constructed directly.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MeasureSurvCindex$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MeasureSurvCindex-clone"></a>}}
\if{latex}{\out{\hypertarget{method-MeasureSurvCindex-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MeasureSurvCindex$clone(deep = FALSE)}\if{html}{\out{</div>}}
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

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MeasureSurvMSE.R
\name{mlr_measures_surv.mse}
\alias{mlr_measures_surv.mse}
\alias{MeasureSurvMSE}
\title{Mean Squared Error Survival Measure}
\description{
Calculates the mean squared error (MSE).

The MSE is defined by
\deqn{\frac{1}{n} \sum ((t - \hat{t})^2)}{1/n \sum ((t - t*)^2)}
where \eqn{t} is the true value and \eqn{\hat{t}}{t*} is the prediction.

Censored observations in the test set are ignored.
}
\section{Dictionary}{

This \link[mlr3:Measure]{Measure} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary}
\link[mlr3:mlr_measures]{mlr_measures} or with the associated sugar function \link[mlr3:mlr_sugar]{msr()}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{MeasureSurvMSE$new()
mlr_measures$get("surv.mse")
msr("surv.mse")
}\if{html}{\out{</div>}}
}

\section{Parameters}{
\tabular{llll}{
   Id \tab Type \tab Default \tab Levels \cr
   se \tab logical \tab FALSE \tab TRUE, FALSE \cr
}
}

\section{Meta Information}{

\itemize{
\item Type: \code{"surv"}
\item Range: \eqn{[0, \infty)}{[0, Inf)}
\item Minimize: \code{TRUE}
\item Required prediction: \code{response}
}
}

\section{Parameter details}{

\itemize{
\item \code{se} (\code{logical(1)})\cr
If \code{TRUE} then returns standard error of the measure otherwise
returns the mean across all individual scores, e.g. the mean of
the per observation scores.
Default is \code{FALSE} (returns the mean).
}
}

\seealso{
Other survival measures: 
\code{\link{mlr_measures_surv.calib_alpha}},
\code{\link{mlr_measures_surv.calib_beta}},
\code{\link{mlr_measures_surv.chambless_auc}},
\code{\link{mlr_measures_surv.cindex}},
\code{\link{mlr_measures_surv.dcalib}},
\code{\link{mlr_measures_surv.graf}},
\code{\link{mlr_measures_surv.hung_auc}},
\code{\link{mlr_measures_surv.intlogloss}},
\code{\link{mlr_measures_surv.logloss}},
\code{\link{mlr_measures_surv.mae}},
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

Other response survival measures: 
\code{\link{mlr_measures_surv.mae}},
\code{\link{mlr_measures_surv.rmse}}
}
\concept{response survival measures}
\concept{survival measures}
\section{Super classes}{
\code{\link[mlr3:Measure]{mlr3::Measure}} -> \code{\link[mlr3proba:MeasureSurv]{mlr3proba::MeasureSurv}} -> \code{MeasureSurvMSE}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-MeasureSurvMSE-new}{\code{MeasureSurvMSE$new()}}
\item \href{#method-MeasureSurvMSE-clone}{\code{MeasureSurvMSE$clone()}}
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
\if{html}{\out{<a id="method-MeasureSurvMSE-new"></a>}}
\if{latex}{\out{\hypertarget{method-MeasureSurvMSE-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MeasureSurvMSE$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MeasureSurvMSE-clone"></a>}}
\if{latex}{\out{\hypertarget{method-MeasureSurvMSE-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MeasureSurvMSE$clone(deep = FALSE)}\if{html}{\out{</div>}}
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

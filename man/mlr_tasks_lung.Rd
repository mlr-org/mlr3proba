% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TaskSurv_zzz.R
\name{mlr_tasks_lung}
\alias{mlr_tasks_lung}
\title{Lung Cancer Survival Task}
\format{
\link[R6:R6Class]{R6::R6Class} inheriting from \link{TaskSurv}.
}
\description{
A survival task for the \link{lung} data set.
}
\details{
Column "sex" has been converted to a factor, all others have been converted to integer.
}
\section{Construction}{


\if{html}{\out{<div class="sourceCode">}}\preformatted{mlr3::mlr_tasks$get("lung")
mlr3::tsk("lung")
}\if{html}{\out{</div>}}
}


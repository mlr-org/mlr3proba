% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Task_zzz.R
\name{mlr_tasks_pbc}
\alias{mlr_tasks_pbc}
\title{Primary Biliary Cholangitis Competing Risks Task}
\format{
\link[R6:R6Class]{R6::R6Class} inheriting from \link{TaskCompRisks}.
}
\description{
A competing risks task for the \link{pbc} data set.
}
\section{Dictionary}{

This \link[mlr3:Task]{Task} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary} \link[mlr3:mlr_tasks]{mlr_tasks} or with the associated sugar function \link[mlr3:mlr_sugar]{tsk()}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{mlr_tasks$get("pbc")
tsk("pbc")
}\if{html}{\out{</div>}}
}

\section{Meta Information}{

\itemize{
\item Task type: \dQuote{cmprsk}
\item Dimensions: 276x19
\item Properties: -
\item Has Missings: \code{FALSE}
\item Target: \dQuote{time}, \dQuote{status}
\item Features: \dQuote{age}, \dQuote{albumin}, \dQuote{alk.phos}, \dQuote{ascites}, \dQuote{ast}, \dQuote{bili}, \dQuote{chol}, \dQuote{copper}, \dQuote{edema}, \dQuote{hepato}, \dQuote{platelet}, \dQuote{protime}, \dQuote{sex}, \dQuote{spiders}, \dQuote{stage}, \dQuote{trig}, \dQuote{trt}
}
}

\section{Pre-processing}{

\itemize{
\item Removed column \code{id}.
\item Kept only complete cases (no missing values).
\item Column \code{age} has been converted to \code{integer}.
\item Columns \code{trt}, \code{stage}, \code{hepato}, \code{edema} and \code{ascites} have been converted
to \code{factor}s.
\item Column \code{trt} has levels \code{Dpenicillmain} and \code{placebo} instead of 1 and 2.
\item Column \code{status} has 0 for censored, 1 for transplant and 2 for death.
\item Column \code{time} as been converted from days to months.
}
}

\seealso{
\itemize{
\item Chapter in the \href{https://mlr3book.mlr-org.com/}{mlr3book}:
\url{https://mlr3book.mlr-org.com/chapters/chapter2/data_and_basic_modeling.html}
\item \link[mlr3misc:Dictionary]{Dictionary} of \link[mlr3:Task]{Tasks}: \link[mlr3:mlr_tasks]{mlr3::mlr_tasks}
\item \code{as.data.table(mlr_tasks)} for a table of available \link[mlr3:Task]{Tasks} in the running session
}

Other Task: 
\code{\link{TaskCompRisks}},
\code{\link{TaskDens}},
\code{\link{TaskSurv}},
\code{\link{mlr_tasks_actg}},
\code{\link{mlr_tasks_faithful}},
\code{\link{mlr_tasks_gbcs}},
\code{\link{mlr_tasks_gbsg}},
\code{\link{mlr_tasks_grace}},
\code{\link{mlr_tasks_lung}},
\code{\link{mlr_tasks_mgus}},
\code{\link{mlr_tasks_precip}},
\code{\link{mlr_tasks_rats}},
\code{\link{mlr_tasks_veteran}},
\code{\link{mlr_tasks_whas}}
}
\concept{Task}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Task_zzz.R
\name{mlr_tasks_faithful}
\alias{mlr_tasks_faithful}
\title{Old Faithful Eruptions Density Task}
\format{
\link[R6:R6Class]{R6::R6Class} inheriting from \link{TaskDens}.
}
\description{
A density task for the \link{faithful} data set.
}
\section{Dictionary}{

This \link[mlr3:Task]{Task} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary} \link[mlr3:mlr_tasks]{mlr_tasks} or with the associated sugar function \link[mlr3:mlr_sugar]{tsk()}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{mlr_tasks$get("faithful")
tsk("faithful")
}\if{html}{\out{</div>}}
}

\section{Meta Information}{

\itemize{
\item Task type: \dQuote{dens}
\item Dimensions: 272x1
\item Properties: -
\item Has Missings: \code{FALSE}
\item Target: -
\item Features: \dQuote{eruptions}
}
}

\section{Preprocessing}{

\itemize{
\item Only the \code{eruptions} column is kept in this task.
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
\code{\link{mlr_tasks_gbcs}},
\code{\link{mlr_tasks_gbsg}},
\code{\link{mlr_tasks_grace}},
\code{\link{mlr_tasks_lung}},
\code{\link{mlr_tasks_mgus}},
\code{\link{mlr_tasks_pbc}},
\code{\link{mlr_tasks_precip}},
\code{\link{mlr_tasks_rats}},
\code{\link{mlr_tasks_veteran}},
\code{\link{mlr_tasks_whas}}
}
\concept{Task}

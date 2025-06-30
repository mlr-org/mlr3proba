#' @section Implementation differences:
#'
#' If comparing the integrated Graf score to other packages, e.g.
#' \CRANpkg{pec}, then `method = 2` should be used. However the results may
#' still be very slightly different as this package uses `survfit` to estimate
#' the censoring distribution, in line with the Graf 1999 paper; whereas some
#' other packages use `prodlim` with `reverse = TRUE` (meaning Kaplan-Meier is
#' not used).

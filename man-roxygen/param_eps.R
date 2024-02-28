#' @section Parameter details:
#' - `eps` (`numeric(1)`)\cr
#' Very small number to substitute zero values in order to prevent errors
#' in e.g. log(0) and/or division-by-zero calculations.
#' Default value is <%= if(exists("eps")) eps %>.

#' Deprecated function(s) in the radiant.model package
#'
#' These functions are provided for compatibility with previous versions of
#' radiant. They will eventually be  removed.
#' @rdname radiant.model-deprecated
#' @name radiant.model-deprecated
#' @param ... Parameters to be passed to the updated functions
#' @export  ann
#' @aliases ann
#' @section Details:
#' \tabular{rl}{
#'   \code{ann} is now a synonym for \code{\link{nn}}\cr
#'   \code{scaledf} is now a synonym for \code{\link{scale_df}}\cr
#' }
#'
ann <- function(...) {
  .Deprecated("nn", package = "radiant.model")
  nn(...)
}
scaledf <- function(...) {
  .Deprecated("scale_df", package = "radiant.model")
  scale_df(...)
}
NULL

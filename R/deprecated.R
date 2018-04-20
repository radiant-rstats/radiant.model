#' Deprecated function(s) in the radiant.model package
#'
#' These functions are provided for compatibility with previous versions of
#' radiant. They will eventually be  removed.
#' @rdname radiant.model-deprecated
#' @name radiant.model-deprecated
#' @param ... Parameters to be passed to the updated functions
#' @docType package
#' @export  ann
#' @aliases ann
#' @section Details:
#' \tabular{rl}{
#'   \code{ann} is now a synonym for \code{\link{nn}}\cr
#' }
#'
ann <- function(...) {
  .Deprecated("nn", package = "radiant.model")
  nn(...)
}
NULL

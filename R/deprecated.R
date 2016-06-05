#' Deprecated function(s) in the radiant.model package
#'
#' These functions are provided for compatibility with previous versions of
#' radiant. They will eventually be  removed.
#' @rdname radiant.model-deprecated
#' @name radiant.model-deprecated
#' @param ... Parameters to be passed to the updated functions
#' @docType package
#' @export  regression glm_reg performance
#' @aliases regression glm_reg performance
#' @section Details:
#' \tabular{rl}{
#'   \code{regression} is now a synonym for \code{\link{regress}}\cr
#'   \code{glm_reg} is now a synonym for \code{\link{logistic}}\cr
#'   \code{performance} is now a synonym for \code{\link{evalbin}}\cr
#' }
#'
regression <- function(...) {
  .Deprecated("regress", package = "radiant.model")
  regress(...)
}
glm_reg <- function(...) {
  .Deprecated("logistic", package = "radiant.model")
  logistic(...)
}
performance <- function(...) {
  .Deprecated("evalbin", package = "radiant.model")
  evalbin(...)
}
NULL

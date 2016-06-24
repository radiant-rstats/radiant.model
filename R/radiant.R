#' Launch Radiant in the default browser
#'
#' @details See \url{http://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @importFrom shiny runApp
#'
#' @export
radiant.model <- function() {
  if (!"package:radiant.model" %in% search())
    if (!require(radiant.model)) stop("Calling radiant.model start function but radiant.model is not installed.")
  runApp(system.file("app", package = "radiant.model"), launch.browser = TRUE)
}

#' Method to evaluate sensitivity of an analysis
#'
#' @param object Object of relevant class for which to evaluate sensitivity
#' @param ... Additional arguments
#'
#' @export
sensitivity <- function(object, ...) UseMethod("sensitivity", object)

#' Method to render DiagrammeR plots
#'
#' @param object DiagrammeR plot
#' @param ... Additional arguments
#'
#' @importFrom DiagrammeR renderDiagrammeR
#'
#' @export
render.DiagrammeR <- function(object, ...) DiagrammeR::renderDiagrammeR(object)

#' Launch radiant.model in default browser or Rstudio Viewer
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @param run Run radiant.model in an external browser ("browser") or in the Rstudio viewer ("viewer")
#'
#' @importFrom rstudioapi viewer
#'
#' @examples
#' \dontrun{
#' radiant.model::radiant.model()
#' radiant.model::radiant.model("viewer")
#' }
#'
#' @export
radiant.model <- function(run = "browser") {
  if (!"package:radiant.model" %in% search()) {
    if (!sshhr(require(radiant.model))) {
      stop("\nCalling radiant.model start function but radiant.model is not installed.")
    }
  }
  run <- if (run == "viewer") {
    message("\nStarting radiant.model in Rstudio Viewer ...")
    rstudioapi::viewer
  } else {
    message("\nStarting radiant.model in default browser ...\n\nUse radiant.model::radiant.model(\"viewer\") to open radiant.model in Rstudio Viewer")
    TRUE
  }
  suppressPackageStartupMessages(
    shiny::runApp(system.file("app", package = "radiant.model"), launch.browser = run)
  )
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
render.DiagrammeR <- function(object, ...) {
  if (exists("r_environment")) {
    DiagrammeR::renderDiagrammeR(object)
  } else {
    object
  }
}

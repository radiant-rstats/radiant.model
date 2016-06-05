#' Launch Radiant in the default browser
#'
#' @details See \url{http://vnijs.github.io/radiant} for documentation and tutorials
#'
#' @importFrom shiny runApp
#'
#' @export
radiant.model <- function() {
  if (!"package:radiant.model" %in% search())
    if (!require(radiant.model)) stop("Calling radiant.model start function but radiant.model is not installed.")
  runApp(system.file("app", package = "radiant.model"), launch.browser = TRUE)
}

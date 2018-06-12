#' Launch radiant.model in the default browser
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.model()
#' }
#' @export
radiant.model <- function() radiant.data::launch(package = "radiant.model", run = "browser")

#' Launch radiant.model in an Rstudio window
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.model_window()
#' }
#' @export
radiant.model_window <- function() radiant.data::launch(package = "radiant.model", run = "window")

#' Launch radiant.model in the Rstudio viewer
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.model_viewer()
#' }
#' @export
radiant.model_viewer <- function() radiant.data::launch(package = "radiant.model", run = "viewer")

#' Method to evaluate sensitivity of an analysis
#'
#' @param object Object of relevant class for which to evaluate sensitivity
#' @param ... Additional arguments
#'
#' @seealso \code{\link{sensitivity.dtree}} to plot results
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
  ## hack for rmarkdown from Report > Rmd and Report > R
  if (exists("r_environment") && !getOption("radiant.rmarkdown", FALSE)) {
    DiagrammeR::renderDiagrammeR(object)
  } else {
    object
  }
}

#' Launch radiant.model in the default browser
#'
#' @description Launch radiant.model in the default web browser
#' @details See \url{https://radiant-rstats.github.io/docs/} for documentation and tutorials
#'
#' @param state Path to state file to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.model()
#' }
#' @export
radiant.model <- function(state, ...) radiant.data::launch(package = "radiant.model", run = "browser", state, ...)

#' Launch radiant.model in an Rstudio window
#'
#' @details See \url{https://radiant-rstats.github.io/docs/} for documentation and tutorials
#'
#' @param state Path to state file to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.model_window()
#' }
#' @export
radiant.model_window <- function(state, ...) radiant.data::launch(package = "radiant.model", run = "window", state, ...)

#' Launch radiant.model in the Rstudio viewer
#'
#' @details See \url{https://radiant-rstats.github.io/docs/} for documentation and tutorials
#'
#' @param state Path to state file to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.model_viewer()
#' }
#' @export
radiant.model_viewer <- function(state, ...) radiant.data::launch(package = "radiant.model", run = "viewer", state, ...)

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
#' @param shiny Check if function is called from a shiny application
#' @param ... Additional arguments
#'
#' @importFrom DiagrammeR renderDiagrammeR
#' @importFrom shiny getDefaultReactiveDomain
#'
#' @export
render.DiagrammeR <- function(object, shiny = shiny::getDefaultReactiveDomain(), ...) {
  ## hack for rmarkdown from Report > Rmd and Report > R
  if (!is.null(shiny) && !getOption("radiant.rmarkdown", FALSE)) {
    DiagrammeR::renderDiagrammeR(object)
  } else {
    object
  }
}

#' One hot encoding of data.frames
#' @param dataset Dataset to endcode
#' @param all Extract all factor levels (e.g., for tree-based models)
#' @param df Return a data.frame (tibble)
#'
#' @examples
#' head(onehot(diamonds, df = TRUE))
#' head(onehot(diamonds, all = TRUE, df = TRUE))
#' @importFrom stats contrasts
#'
#' @export
onehot <- function(dataset, all = FALSE, df = FALSE) {
  if (all) {
    mm <- model.matrix(~ 0 + .,
      data = dataset,
      contrasts.arg = lapply(
        dataset[, vapply(dataset, is.factor, logical(1))],
        contrasts,
        contrasts = FALSE
      )
    )
  } else {
    mm <- model.matrix(~., model.frame(~., dataset))
  }
  if (df) as.data.frame(mm, stringsAsFactors = FALSE) else mm
}

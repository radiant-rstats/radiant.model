#' Naive Bayes using e1071::naiveBayes
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nb.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param rvar The response variable in the logit (probit) model
#' @param evar Explanatory variables in the model
#' @param laplace Positive double controlling Laplace smoothing. The default (0) disables Laplace smoothing.
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param envir Environment to extract data from
#'
#' @return A list with all variables defined in nb as an object of class nb
#'
#' @examples
#' nb(titanic, "survived", c("pclass", "sex", "age")) %>% summary()
#' nb(titanic, "survived", c("pclass", "sex", "age")) %>% str()
#'
#' @seealso \code{\link{summary.nb}} to summarize results
#' @seealso \code{\link{plot.nb}} to plot results
#' @seealso \code{\link{predict.nb}} for prediction
#'
#' @importFrom e1071 naiveBayes
#'
#' @export
nb <- function(
  dataset, rvar, evar, laplace = 0, 
  data_filter = "", envir = parent.frame()
) {

  if (rvar %in% evar) {
    return("Response variable contained in the set of explanatory variables.\nPlease update model specification." %>%
      add_class("nb"))
  }

  df_name <- if (!is_string(dataset)) deparse(substitute(dataset)) else dataset
  dataset <- get_data(dataset, c(rvar, evar), filt = data_filter, envir = envir)

  not_vary <- colnames(dataset)[summarise_all(dataset, does_vary) == FALSE]
  if (length(not_vary) > 0) {
    return(paste0("The following variable(s) show no variation. Please select other variables.\n\n** ", paste0(not_vary, collapse = ", "), " **") %>%
      add_class("nb"))
  }

  vars <- evar
  ## in case : is used
  if (length(vars) < (ncol(dataset) - 1)) {
    vars <- evar <- colnames(dataset)[-1]
  }

  ## make sure the dv is a factor
  if (!is.factor(dataset[[1]])) dataset <- as_factor(dataset[[1]])
  lev <- levels(dataset[[1]])

  ## estimate using e1071
  form <- paste0(rvar, " ~ ", paste0(evar, collapse = "+")) %>% as.formula()
  model <- e1071::naiveBayes(dataset[, -1, drop = FALSE], dataset[[1]], laplace = laplace)

  ## nb does not return residuals
  model$residuals <- NA

  ## nb doesn't indlude model terms, needed for predict_model
  # model$terms <- colnames(dataset)
  # attr(model$term, "dataClasses") <- get_class(dataset)

  ## nb model object does not include the data by default
  model$model <- dataset
  rm(dataset, envir) ## dataset not needed elsewhere

  as.list(environment()) %>% add_class(c("nb", "model"))
}

#' Summary method for the nb function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nb.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{nb}}
#' @param dec Decimals
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- nb(titanic, "survived", c("pclass", "sex", "age"))
#' summary(result)
#'
#' @seealso \code{\link{nb}} to generate results
#' @seealso \code{\link{plot.nb}} to plot results
#' @seealso \code{\link{predict.nb}} for prediction
#'
#' @export
summary.nb <- function(object, dec = 3, ...) {
  if (is.character(object)) return(object)

  cat("Naive Bayes Classifier")
  cat("\nData                 :", object$df_name)
  if (!radiant.data::is_empty(object$data_filter)) {
    cat("\nFilter               :", gsub("\\n", "", object$data_filter))
  }
  cat("\nResponse variable    :", object$rvar)
  cat("\nLevels               :", paste0(object$lev, collapse = ", "), "in", object$rvar)
  cat("\nExplanatory variables:", paste0(object$evar, collapse = ", "))
  cat("\nLaplace              :", object$laplace)
  cat("\nNr obs               :", format_nr(nrow(object$model$model), dec = 0), "\n")

  cat("\nA-priori probabilities:\n")
  apriori <- object$model$apriori %>% {. / sum(.)}
  names(dimnames(apriori))[1] <- object$rvar
  print(round(apriori, 3))

  cat("\nConditional probabilities (categorical) or means & st.dev (numeric):\n")
  for (i in object$model$tables) {
    names(dimnames(i))[1] <- object$rvar
    if (is.null(dimnames(i)[2][[1]])) dimnames(i)[2][[1]] <- c("mean", "st.dev")
    print(round(i, dec))
    cat("\n")
  }
}

#' Plot method for the nb function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nb.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{nb}}
#' @param plots Plots to produce for the specified model. Use "" to avoid showing any plots. Use "vimp" for variable importance or "correlations" to examine conditional independence
#' @param lev The level(s) in the response variable used as the basis for plots (defaults to "All levels")
#' @param nrobs Number of data points to show in scatter plots (-1 for all)
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- nb(titanic, "survived", c("pclass", "sex"))
#' plot(result)
#' result <- nb(titanic, "pclass", c("sex", "age"))
#' plot(result)
#'
#' @seealso \code{\link{nb}} to generate results
#' @seealso \code{\link{summary.nb}} to summarize results
#' @seealso \code{\link{predict.nb}} for prediction
#'
#' @export
plot.nb <- function(x, plots = "correlations", lev = "All levels", nrobs = 1000, ...) {
  if (is.character(x)) return(x)
  if (radiant.data::is_empty(plots[1])) return(invisible())

  rvar <- x$model$model[[1]]

  if ("correlations" %in% plots) {
    if (lev == "All levels") {
      return(sshhr(radiant.basics:::plot.correlation(x$model$model, nrobs = nrobs)))
    } else {
      return(sshhr(radiant.basics:::plot.correlation(filter(select(x$model$model, -1), rvar == lev), nrobs = nrobs)))
    }
  }

  evar <- mutate_all(select(x$model$model, -1), as_numeric)

  if (lev != "All levels") {
    rvar <- factor(
      ifelse(rvar == lev, lev, paste0("not_", lev)),
      levels = c(lev, paste0("not_", lev))
    )
    x$lev <- c(lev, paste0("not_", lev))
  }

  k <- length(x$lev)

  if (k == 2) {
    ## with two variables one of them would be set to 0 by caret::varImp
    ## reporting auc for each variable
    vimp <- data.frame(auc = apply(evar, 2, auc, rvar), vars = colnames(evar), stringsAsFactors = FALSE) %>%
      arrange_at(.vars = "auc")
    vimp$vars <- factor(vimp$vars, levels = vimp$vars)
    p <- visualize(vimp, yvar = "auc", xvar = "vars", type = "bar", custom = TRUE) +
      labs(x = "", y = "Variable Importance (AUC)") +
      coord_flip(ylim = c(0.5, max(vimp$auc))) +
      theme(axis.text.y = element_text(hjust = 0))
  } else {
    cmb <- combn(x$lev, 2)
    vimp <- matrix(NA, ncol(cmb), ncol(evar))

    for (i in 1:ncol(cmb)) {
      ind <- rvar %in% cmb[, i]
      vimp[i, ] <- apply(evar[ind, , drop = FALSE], 2, auc, droplevels(rvar[ind]))
    }
    vimp <- as.data.frame(vimp, stringsAsFactors = FALSE)
    colnames(vimp) <- names(evar)
    vimp$Predict <- apply(cmb, 2, paste0, collapse = " vs ")
    vimp$Predict <- factor(vimp$Predict, levels = unique(rev(vimp$Predict)))
    vimp <- gather(vimp, "vars", "auc", !! colnames(evar), factor_key = TRUE)

    p <- visualize(vimp, yvar = "auc", xvar = "Predict", type = "bar", fill = "vars", custom = TRUE) +
      guides(fill = guide_legend(title = "")) +
      labs(x = "", y = "Variable Importance (AUC)") +
      coord_flip(ylim = c(0.5, max(vimp$auc))) +
      theme(axis.text.y = element_text(hjust = 0))
  }

  sshhr(p)
}

#' Predict method for the nb function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nb.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{nb}}
#' @param pred_data Provide the dataframe to generate predictions (e.g., titanic). The dataset must contain all columns used in the estimation
#' @param pred_cmd Generate predictions using a command. For example, `pclass = levels(pclass)` would produce predictions for the different levels of factor `pclass`. To add another variable, create a vector of prediction strings, (e.g., c('pclass = levels(pclass)', 'age = seq(0,100,20)')
#' @param pred_names Names for the predictions to be stored. If one name is provided, only the first column of predictions is stored. If empty, the level in the response variable of the nb model will be used
#' @param dec Number of decimals to show
#' @param envir Environment to extract data from
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- nb(titanic, "survived", c("pclass", "sex", "age"))
#' predict(result, pred_data = titanic)
#' predict(result, pred_data = titanic, pred_names = c("Yes", "No"))
#' predict(result, pred_cmd = "pclass = levels(pclass)")
#' result <- nb(titanic, "pclass", c("survived", "sex", "age"))
#' predict(result, pred_data = titanic)
#' predict(result, pred_data = titanic, pred_names = c("1st", "2nd", "3rd"))
#' predict(result, pred_data = titanic, pred_names = "")
#'
#' @seealso \code{\link{nb}} to generate the result
#' @seealso \code{\link{summary.nb}} to summarize results
#'
#' @export
predict.nb <- function(
  object, pred_data = NULL, pred_cmd = "",
  pred_names = "", dec = 3, envir = parent.frame(), 
  ...
) {

  if (is.character(object)) return(object)

  ## ensure you have a name for the prediction dataset
  if (is.data.frame(pred_data)) {
    df_name <- deparse(substitute(pred_data))
  } else {
    df_name <- pred_data
  }

  pfun <- function(model, pred, se, conf_lev) {

    ## need to make sure levels in original data and pred are the same
    ## as predict.naiveBayes relies on this ordering
    set_levels <- function(name) {
      if (!is.null(model$model[[name]]) && is.factor(model$model[[name]])) {
        levs <- levels(model$model[[name]])
        levs_pred <- levels(pred[[name]])
        if (is.null(levs_pred) || !all(levs == levs_pred)) {
          pred[[name]] <<- factor(pred[[name]], levels = levs)
        }
      }
    }

    fix <- sapply(colnames(pred), set_levels)
    pred_val <- try(sshhr(predict(model, pred, type = "raw")), silent = TRUE)

    if (!inherits(pred_val, "try-error")) {
      pred_val %<>% as.data.frame(stringsAsFactors = FALSE)
      if (all(radiant.data::is_empty(pred_names))) pred_names <- colnames(pred_val)
      pred_val %<>% select(1:min(ncol(pred_val), length(pred_names))) %>%
        set_colnames(pred_names)
    }

    pred_val
  }

  predict_model(object, pfun, "nb.predict", pred_data, pred_cmd, dec = dec, envir = envir) %>%
    set_attr("radiant_pred_data", df_name)
}

#' Print method for predict.nb
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#'
#' @export
print.nb.predict <- function(x, ..., n = 10)
  print_predict_model(x, ..., n = n, header = "Naive Bayes Classifier", lev = attr(x, "radiant_lev"))

#' Plot method for nb.predict function
#'
#' @param x Return value from predict function predict.nb
#' @param xvar Variable to display along the X-axis of the plot
#' @param facet_row Create vertically arranged subplots for each level of the selected factor variable
#' @param facet_col Create horizontally arranged subplots for each level of the selected factor variable
#' @param color Adds color to a scatter plot to generate a heat map. For a line plot one line is created for each group and each is assigned a different color
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- nb(titanic, "survived", c("pclass", "sex", "age"))
#' pred <- predict(
#'   result,
#'   pred_cmd = c("pclass = levels(pclass)", "sex = levels(sex)", "age = seq(0, 100, 20)")
#' )
#' plot(pred, xvar = "age", facet_col = "sex", facet_row = "pclass")
#' pred <- predict(result, pred_data = titanic)
#' plot(pred, xvar = "age", facet_col = "sex")
#'
#' @seealso \code{\link{predict.nb}} to generate predictions
#'
#' @export
plot.nb.predict <- function(
  x, xvar = "", facet_row = ".", facet_col = ".",
  color = ".class", ...
) {

  ## should work with req in regress_ui but doesn't
  if (radiant.data::is_empty(xvar)) return(invisible())

  if (facet_col != "." && facet_row == facet_col) {
    return("The same variable cannot be used for both Facet row and Facet column")
  }

  if (is.character(x)) return(x)

  pvars <- base::setdiff(attr(x, "radiant_vars"), attr(x, "radiant_evar"))
  rvar <- attr(x, "radiant_rvar")
  x %<>% gather(".class", "Prediction", !! pvars)

  byvar <- c(xvar, color)
  if (facet_row != ".") byvar <- unique(c(byvar, facet_row))
  if (facet_col != ".") byvar <- unique(c(byvar, facet_col))

  tmp <- group_by_at(x, .vars = byvar) %>%
    select_at(.vars = c(byvar, "Prediction")) %>%
    summarise_all(mean)
  p <- ggplot(tmp, aes_string(x = xvar, y = "Prediction", color = color, group = color)) +
    geom_line()

  if (facet_row != "." || facet_col != ".") {
    facets <- ifelse(facet_row == ".", paste("~", facet_col), paste(facet_row, "~", facet_col))
    facet_fun <- ifelse(facet_row == ".", facet_wrap, facet_grid)
    p <- p + facet_fun(as.formula(facets))
  }

  p <- p + guides(color = guide_legend(title = rvar))

  sshhr(p)
}

#' Store predicted values generated in the nb function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nb.html} for an example in Radiant
#'
#' @param dataset Dataset to add predictions to
#' @param object Return value from model function
#' @param name Variable name(s) assigned to predicted values. If empty, the levels of the response variable will be used
#' @param ... Additional arguments
#'
#' @examples
#' result <- nb(titanic, rvar = "survived", evar = c("pclass", "sex", "age"))
#' pred <- predict(result, pred_data = titanic)
#' titanic <- store(titanic, pred, name = c("Yes", "No"))
#'
#' @export
store.nb.predict <- function(dataset, object, name = NULL, ...) {

  ## extract the names of the variables predicted
  pvars <- base::setdiff(attr(object, "radiant_vars"), attr(object, "radiant_evar"))

  ## as.vector removes all attributes from df
  # df <- as.vector(object[, pvars])
  df <- object[, pvars, drop = FALSE] %>% mutate(across(everything(), as.vector))

  if (radiant.data::is_empty(name)) {
    name <- pvars
  } else {
    ## gsub needed because trailing/leading spaces may be added to the variable name
    name <- unlist(strsplit(name, "(\\s*,\\s*|\\s*;\\s*|\\s+)")) %>%
      gsub("\\s", "", .)
    if (length(name) < length(pvars)) {
      df <- df[, 1:length(name), drop = FALSE] %>% set_colnames(name)
    }
  }

  indr <- indexr(dataset, attr(object, "radiant_evar"), "", cmd = attr(object, "radiant_pred_cmd"))
  pred <- as.data.frame(matrix(NA, nrow = indr$nr, ncol = ncol(df)), stringsAsFactors = FALSE)
  pred[indr$ind, ] <- df

  dataset[, name] <- pred
  dataset
}

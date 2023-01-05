#' Random Forest using Ranger
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/rforest.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param rvar The response variable in the model
#' @param evar Explanatory variables in the model
#' @param type Model type (i.e., "classification" or "regression")
#' @param lev Level to use as the first column in prediction output
#' @param mtry Number of variables to possibly split at in each node. Default is the (rounded down) square root of the number variables
#' @param num.trees Number of trees to create
#' @param min.node.size Minimal node size
#' @param sample.fraction Fraction of observations to sample. Default is 1 for sampling with replacement and 0.632 for sampling without replacement
#' @param replace Sample with (TRUE) or without (FALSE) replacement. If replace is NULL it will be reset to TRUE if the sample.fraction is equal to 1 and will be set to FALSE otherwise
#' @param num.threads Number of parallel threads to use. Defaults to 12 if available
#' @param wts Case weights to use in estimation
#' @param seed Random seed to use as the starting point
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param rows Rows to select from the specified dataset
#' @param arr Expression to arrange (sort) the data on (e.g., "color, desc(price)")
#' @param envir Environment to extract data from
#' @param ... Further arguments to pass to ranger
#'
#' @return A list with all variables defined in rforest as an object of class rforest
#'
#' @examples
#' rforest(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>% summary()
#' rforest(titanic, "survived", c("pclass", "sex")) %>% str()
#' rforest(titanic, "survived", c("pclass", "sex"), max.depth = 1)
#' rforest(diamonds, "price", c("carat", "clarity"), type = "regression") %>% summary()
#'
#' @seealso \code{\link{summary.rforest}} to summarize results
#' @seealso \code{\link{plot.rforest}} to plot results
#' @seealso \code{\link{predict.rforest}} for prediction
#'
#' @importFrom ranger ranger
#' @importFrom lubridate is.Date
#'
#' @export
rforest <- function(dataset, rvar, evar, type = "classification", lev = "",
                    mtry = NULL, num.trees = 100, min.node.size = 1,
                    sample.fraction = 1, replace = NULL,
                    num.threads = 12, wts = "None", seed = NA,
                    data_filter = "", arr = "", rows = NULL, envir = parent.frame(), ...) {
  if (rvar %in% evar) {
    return("Response variable contained in the set of explanatory variables.\nPlease update model specification." %>%
      add_class("rforest"))
  }

  vars <- c(rvar, evar)

  if (is.empty(wts, "None")) {
    wts <- NULL
  } else if (is_string(wts)) {
    wtsname <- wts
    vars <- c(rvar, evar, wtsname)
  }

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, vars, filt = data_filter, arr = arr, rows = rows, envir = envir) %>%
    mutate_if(is.Date, as.numeric)

  if (!is.empty(wts)) {
    if (exists("wtsname")) {
      wts <- dataset[[wtsname]]
      dataset <- select_at(dataset, .vars = base::setdiff(colnames(dataset), wtsname))
    }
    if (length(wts) != nrow(dataset)) {
      return(
        paste0("Length of the weights variable is not equal to the number of rows in the dataset (", format_nr(length(wts), dec = 0), " vs ", format_nr(nrow(dataset), dec = 0), ")") %>%
          add_class("rforest")
      )
    }
  }

  not_vary <- colnames(dataset)[summarise_all(dataset, does_vary) == FALSE]
  if (length(not_vary) > 0) {
    return(paste0("The following variable(s) show no variation. Please select other variables.\n\n** ", paste0(not_vary, collapse = ", "), " **") %>%
      add_class("rforest"))
  }

  rv <- dataset[[rvar]]

  if (type == "classification") {
    if (lev == "") {
      if (is.factor(rv)) {
        lev <- levels(rv)[1]
      } else {
        lev <- as.character(rv) %>%
          as.factor() %>%
          levels() %>%
          .[1]
      }
    }
    if (lev != levels(rv)[1]) {
      dataset[[rvar]] <- relevel(dataset[[rvar]], lev)
    }
    probability <- TRUE
  } else {
    probability <- FALSE
  }

  vars <- evar
  ## in case : is used
  if (length(vars) < (ncol(dataset) - 1)) {
    vars <- evar <- colnames(dataset)[-1]
  }

  if (is.empty(replace)) {
    replace <- ifelse(sample.fraction < 1, FALSE, TRUE)
  }

  ## use decay http://stats.stackexchange.com/a/70146/61693
  rforest_input <- list(
    formula = as.formula(paste(rvar, "~ . ")),
    mtry = mtry,
    num.trees = num.trees,
    min.node.size = min.node.size,
    probability = probability,
    importance = "permutation",
    sample.fraction = sample.fraction,
    replace = replace,
    num.threads = num.threads,
    case.weights = wts,
    data = dataset,
    ...
  )
  extra_args <- list(...)

  ## based on https://stackoverflow.com/a/14324316/1974918
  seed <- gsub("[^0-9]", "", seed)
  if (!is.empty(seed)) {
    if (exists(".Random.seed")) {
      gseed <- .Random.seed
      on.exit(.Random.seed <<- gseed)
    }
    set.seed(seed)
  }

  model <- do.call(ranger::ranger, rforest_input)

  ## rforest doesn't return residuals
  if (type == "regression") {
    model$residuals <- dataset[[rvar]] - model$predictions
  } else {
    model$residuals <- NULL
  }

  ## rforest model object does not include the data by default
  model$model <- dataset

  rm(dataset, envir, rforest_input) ## dataset not needed elsewhere

  ## needed to work with prediction functions
  check <- ""

  as.list(environment()) %>% add_class(c("rforest", "model"))
}

#' Summary method for the rforest function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/rforest.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{rforest}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- rforest(titanic, "survived", "pclass", lev = "Yes")
#' summary(result)
#'
#' @seealso \code{\link{rforest}} to generate results
#' @seealso \code{\link{plot.rforest}} to plot results
#' @seealso \code{\link{predict.rforest}} for prediction
#'
#' @export
summary.rforest <- function(object, ...) {
  if (is.character(object)) {
    return(object)
  }
  cat("Random Forest (Ranger)\n")
  if (object$type == "classification") {
    cat("Type                 : Classification")
  } else {
    cat("Type                 : Regression")
  }
  cat("\nData                 :", object$df_name)
  if (!is.empty(object$data_filter)) {
    cat("\nFilter               :", gsub("\\n", "", object$data_filter))
  }
  if (!is.empty(object$arr)) {
    cat("\nArrange              :", gsub("\\n", "", object$arr))
  }
  if (!is.empty(object$rows)) {
    cat("\nSlice                :", gsub("\\n", "", object$rows))
  }
  cat("\nResponse variable    :", object$rvar)
  if (object$type == "classification") {
    cat("\nLevel                :", object$lev, "in", object$rvar)
  }
  cat("\nExplanatory variables:", paste0(object$evar, collapse = ", "), "\n")
  if (length(object$wtsname) > 0) {
    cat("Weights used         :", object$wtsname, "\n")
  }
  cat("Mtry                 :", object$mtry, "\n")
  cat("Number of trees      :", object$num.trees, "\n")
  cat("Min node size        :", object$min.node.size, "\n")
  cat("Sample fraction      :", object$sample.fraction, "\n")
  cat("Number of threads    :", object$num.threads, "\n")
  if (length(object$extra_args)) {
    extra_args <- deparse(object$extra_args) %>%
      sub("list\\(", "", .) %>%
      sub("\\)$", "", .)
    cat("Additional arguments :", extra_args, "\n")
  }
  if (!is.empty(object$wts, "None") && (length(unique(object$wts)) > 2 || min(object$wts) >= 1)) {
    cat("Nr obs               :", format_nr(sum(object$wts), dec = 0), "\n")
  } else {
    cat("Nr obs               :", format_nr(length(object$rv), dec = 0), "\n")
  }
  if (object$type != "classification") {
    cat("R-squared            :", format_nr(object$model$r.square, dec = 3), "\n")
  }
  OOB <- ifelse(object$type == "classification", object$model$prediction.error, sqrt(object$model$prediction.error))
  cat("OOB prediction error :", format_nr(OOB, dec = 3), "\n")
}

#' Plot method for the rforest function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/rforest.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{rforest}}
#' @param plots Plots to produce for the specified Random Forest model. Use "" to avoid showing any plots (default). Options are ...
#' @param nrobs Number of data points to show in dashboard scatter plots (-1 for all)
#' @param incl Which variables to include in PDP or Prediction plots
#' @param incl_int Which interactions to investigate in PDP or Prediction plots
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned.
#'   This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples
#'   and \url{https://ggplot2.tidyverse.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- rforest(titanic, "survived", c("pclass", "sex"), lev = "Yes")
#'
#' @seealso \code{\link{rforest}} to generate results
#' @seealso \code{\link{summary.rforest}} to summarize results
#' @seealso \code{\link{predict.rforest}} for prediction
#'
#' @importFrom pdp partial
#'
#' @export
plot.rforest <- function(x, plots = "", nrobs = Inf,
                         incl = NULL, incl_int = NULL,
                         shiny = FALSE, custom = FALSE, ...) {
  if (is.character(x) || !inherits(x$model, "ranger")) {
    return(x)
  }
  plot_list <- list()
  nrCol <- 1

  if (x$type == "regression" && "dashboard" %in% plots) {
    plot_list <- plot.regress(x, plots = "dashboard", lines = "line", nrobs = nrobs, custom = TRUE)
    nrCol <- 2
  }

  if ("pred_plot" %in% plots) {
    nrCol <- 2
    if (length(incl) > 0 | length(incl_int) > 0) {
      plot_list <- pred_plot(x, plot_list, incl, incl_int, ...)
    } else {
      return("Select one or more variables to generate Prediction plots")
    }
  }

  if ("pdp" %in% plots) {
    nrCol <- 2
    if (length(incl) > 0 || length(incl_int) > 0) {
      plot_list <- pdp_plot(x, plot_list, incl, incl_int, ...)
    } else {
      return("Select one or more variables to generate Partial Dependence Plots")
    }
  }

  if ("vimp" %in% plots) {
    nrCol <- 1
    vip <- x$model$variable.importance
    if (x$type == "regression") vip <- vip / max(vip)
    vimp <- data.frame(
      vip = vip,
      vars = names(vip),
      stringsAsFactors = FALSE
    ) %>%
      arrange(vip) %>%
      mutate(vars = factor(vars, levels = vars))
    plot_list[["vimp"]] <- visualize(vimp, yvar = "vip", xvar = "vars", type = "bar", custom = TRUE) +
      guides(fill = guide_legend(title = "")) +
      labs(x = "", y = "Variable Importance (permutation)") +
      coord_flip() +
      theme(axis.text.y = element_text(hjust = 0))
  }

  if ("vip" %in% plots) {
    nrCol <- 1
    if (length(x$evar) < 2) {
      message("Model must contain at least 2 explanatory variables (features). Permutation Importance plot cannot be generated")
    } else {
      vi_scores <- varimp(x)
      plot_list[["vip"]] <-
        visualize(vi_scores, yvar = "Importance", xvar = "Variable", type = "bar", custom = TRUE) +
        labs(
          title = "Permutation Importance",
          x = NULL,
          y = ifelse(x$type == "regression", "Importance (R-square decrease)", "Importance (AUC decrease)")
        ) +
        coord_flip() +
        theme(axis.text.y = element_text(hjust = 0))
    }
  }

  if (length(plot_list) > 0) {
    if (custom) {
      if (length(plot_list) == 1) plot_list[[1]] else plot_list
    } else {
      patchwork::wrap_plots(plot_list, ncol = nrCol) %>%
        (function(x) if (isTRUE(shiny)) x else print(x))
    }
  }
}

#' Predict method for the rforest function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/rforest.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{rforest}}
#' @param pred_data Provide the dataframe to generate predictions (e.g., diamonds). The dataset must contain all columns used in the estimation
#' @param pred_cmd Generate predictions using a command. For example, `pclass = levels(pclass)` would produce predictions for the different
#'   levels of factor `pclass`. To add another variable, create a vector of prediction strings, (e.g., c('pclass = levels(pclass)', 'age = seq(0,100,20)')
#' @param pred_names Names for the predictions to be stored. If one name is provided, only the first column of predictions is stored. If empty, the levels
#'   in the response variable of the rforest model will be used
#' @param OOB Use Out-Of-Bag predictions (TRUE or FALSE). Relevant when evaluating predictions for the training sample. If missing, datasets will be compared
#'   to determine of OOB predictions should be used
#' @param dec Number of decimals to show
#' @param envir Environment to extract data from
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- rforest(titanic, "survived", c("pclass", "sex"), lev = "Yes")
#' predict(result, pred_cmd = "pclass = levels(pclass)")
#' result <- rforest(diamonds, "price", "carat:color", type = "regression")
#' predict(result, pred_cmd = "carat = 1:3")
#' predict(result, pred_data = diamonds) %>% head()
#'
#' @seealso \code{\link{rforest}} to generate the result
#' @seealso \code{\link{summary.rforest}} to summarize results
#'
#' @export
predict.rforest <- function(object, pred_data = NULL, pred_cmd = "",
                            pred_names = "", OOB = NULL, dec = 3,
                            envir = parent.frame(), ...) {
  if (is.character(object)) {
    return(object)
  }

  ## ensure you have a name for the prediction dataset
  if (is.data.frame(pred_data)) {
    df_name <- deparse(substitute(pred_data))
  } else {
    df_name <- pred_data
  }

  pfun <- function(model, pred, se, conf_lev, OOB = OOB) {
    pred <- mutate_if(pred, is.Date, as.numeric)
    if (is.empty(OOB)) {
      if (isTRUE(dplyr::all_equal(select(model$model, -1), pred))) {
        message("Using OOB predictions after comparing the training and prediction data")
        OOB <- TRUE
      }
    }

    if (isTRUE(OOB)) {
      pred_val <- list(predictions = model$predictions)
      message("Using OOB predictions")
    } else {
      pred_val <- try(sshhr(predict(model, pred)), silent = TRUE)
    }

    if (!inherits(pred_val, "try-error")) {
      pred_val <- as.data.frame(pred_val$predictions, stringsAsFactors = FALSE)
      if (nrow(pred_val) != nrow(pred)) {
        pred_val <- list() %>% add_class("try-error")
        attr(pred_val, "condition") <- list(message = "Attempt to use OOB predictions failed. This could be because\na filter was set but the random forest model has not yet been\nre-estimated.")
      } else {
        if (ncol(pred_val) == 1) {
          pred_names <- "Prediction"
        } else if (is.empty(pred_names)) {
          pred_names <- colnames(pred_val)
        }
        pred_val <- select(pred_val, 1:min(ncol(pred_val), length(pred_names))) %>%
          set_colnames(pred_names)
      }
    }

    pred_val
  }

  predict_model(object, pfun, "rforest.predict", pred_data, pred_cmd, conf_lev = 0.95, se = FALSE, dec, envir = envir, OOB = OOB) %>%
    set_attr("radiant_pred_data", df_name)
}

#' Print method for predict.rforest
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#'
#' @export
print.rforest.predict <- function(x, ..., n = 10) {
  print_predict_model(x, ..., n = n, header = "Random Forest")
}

#' Plot method for rforest.predict function
#'
#' @param x Return value from predict function predict.rforest
#' @param xvar Variable to display along the X-axis of the plot
#' @param facet_row Create vertically arranged subplots for each level of the selected factor variable
#' @param facet_col Create horizontally arranged subplots for each level of the selected factor variable
#' @param color Adds color to a scatter plot to generate a heat map. For a line plot one line is created for each group and each is assigned a different color
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- mnl(
#'   ketchup,
#'   rvar = "choice",
#'   evar = c("price.heinz28", "price.heinz32", "price.heinz41", "price.hunts32"),
#'   lev = "heinz28"
#' )
#' pred <- predict(result, pred_cmd = "price.heinz28 = seq(3, 5, 0.1)")
#' plot(pred, xvar = "price.heinz28")
#'
#' @seealso \code{\link{predict.mnl}} to generate predictions
#' @importFrom rlang .data
#'
#' @export
plot.rforest.predict <- function(x, xvar = "", facet_row = ".", facet_col = ".",
                                 color = "none", ...) {
  if (color != ".class") {
    return(plot.model.predict(
      x,
      xvar = xvar, facet_row = facet_row, facet_col = facet_col,
      color = color, ...
    ))
  }

  ## should work with req in regress_ui but doesn't
  if (is.empty(xvar)) {
    return(invisible())
  }
  if (is.character(x)) {
    return(x)
  }
  if (facet_col != "." && facet_row == facet_col) {
    return("The same variable cannot be used for both Facet row and Facet column")
  }

  pvars <- base::setdiff(attr(x, "radiant_vars"), attr(x, "radiant_evar"))
  rvar <- attr(x, "radiant_rvar")
  x %<>% gather(".class", "Prediction", !!pvars)

  byvar <- c(xvar, color)
  if (facet_row != ".") byvar <- unique(c(byvar, facet_row))
  if (facet_col != ".") byvar <- unique(c(byvar, facet_col))

  tmp <- group_by_at(x, .vars = byvar) %>%
    select_at(.vars = c(byvar, "Prediction")) %>%
    summarise_all(mean)
  p <- ggplot(tmp, aes(x = .data[[xvar]], y = .data$Prediction, color = .data[[color]], group = .data[[color]])) +
    geom_line()

  if (facet_row != "." || facet_col != ".") {
    facets <- ifelse(facet_row == ".", paste("~", facet_col), paste(facet_row, "~", facet_col))
    facet_fun <- ifelse(facet_row == ".", facet_wrap, facet_grid)
    p <- p + facet_fun(as.formula(facets))
  }

  p <- p + guides(color = guide_legend(title = rvar))

  sshhr(p)
}

#' Store predicted values generated in the rforest function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/rforest.html} for an example in Radiant
#'
#' @param dataset Dataset to add predictions to
#' @param object Return value from model function
#' @param name Variable name(s) assigned to predicted values. If empty, the levels of the response variable will be used
#' @param ... Additional arguments
#'
#' @examples
#' result <- rforest(
#'   ketchup,
#'   rvar = "choice",
#'   evar = c("price.heinz28", "price.heinz32", "price.heinz41", "price.hunts32"),
#'   lev = "heinz28"
#' )
#' pred <- predict(result, pred_data = ketchup)
#' ketchup <- store(ketchup, pred, name = c("heinz28", "heinz32", "heinz41", "hunts32"))
#'
#' @export
store.rforest.predict <- function(dataset, object, name = NULL, ...) {

  ## extract the names of the variables predicted
  pvars <- base::setdiff(attr(object, "radiant_vars"), attr(object, "radiant_evar"))

  ## as.vector removes all attributes from df
  # df <- as.vector(object[, pvars, drop = FALSE])
  df <- object[, pvars, drop = FALSE] %>% mutate(across(everything(), as.vector))

  if (is.empty(name)) {
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

#' Cross-validation for a Random Forest
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/rforest.html} for an example in Radiant
#'
#' @param object Object of type "rforest" or "ranger"
#' @param K Number of cross validation passes to use
#' @param repeats Repeated cross validation
#' @param mtry Number of variables to possibly split at in each node. Default is the (rounded down) square root of the number variables
#' @param num.trees Number of trees to create
#' @param min.node.size Minimal node size
#' @param sample.fraction Fraction of observations to sample. Default is 1 for sampling with replacement and 0.632 for sampling without replacement
#' @param seed Random seed to use as the starting point
#' @param trace Print progress
#' @param fun Function to use for model evaluation (i.e., auc for classification and RMSE for regression)
#' @param ... Additional arguments to be passed to 'fun'
#'
#' @return A data.frame sorted by the mean of the performance metric
#'
#' @seealso \code{\link{rforest}} to generate an initial model that can be passed to cv.rforest
#' @seealso \code{\link{Rsq}} to calculate an R-squared measure for a regression
#' @seealso \code{\link{RMSE}} to calculate the Root Mean Squared Error for a regression
#' @seealso \code{\link{MAE}} to calculate the Mean Absolute Error for a regression
#' @seealso \code{\link{auc}} to calculate the area under the ROC curve for classification
#' @seealso \code{\link{profit}} to calculate profits for classification at a cost/margin threshold
#'
#' @importFrom shiny getDefaultReactiveDomain withProgress incProgress
#'
#' @examples
#' \dontrun{
#' result <- rforest(dvd, "buy", c("coupon", "purch", "last"))
#' cv.rforest(
#'   result,
#'   mtry = 1:3, min.node.size = seq(1, 10, 5),
#'   num.trees = c(100, 200), sample.fraction = 0.632
#' )
#' result <- rforest(titanic, "survived", c("pclass", "sex"), max.depth = 1)
#' cv.rforest(result, mtry = 1:3, min.node.size = seq(1, 10, 5))
#' cv.rforest(result, mtry = 1:3, num.trees = c(100, 200), fun = profit, cost = 1, margin = 5)
#' result <- rforest(diamonds, "price", c("carat", "color", "clarity"), type = "regression")
#' cv.rforest(result, mtry = 1:3, min.node.size = 1)
#' cv.rforest(result, mtry = 1:3, min.node.size = 1, fun = Rsq)
#' }
#'
#' @export
cv.rforest <- function(object, K = 5, repeats = 1, mtry = 1:5, num.trees = NULL, min.node.size = 1, sample.fraction = NA,
                       trace = TRUE, seed = 1234, fun, ...) {
  if (inherits(object, "rforest")) object <- object$model
  if (inherits(object, "ranger")) {
    dv <- as.character(object$call$formula[[2]])
    m <- eval(object$call[["data"]])
    mtry <- mtry[mtry < ncol(m)]
    weights <- eval(object$call[["case.weights"]])
    if (is.numeric(m[[dv]])) {
      type <- "regression"
    } else {
      type <- "classification"
      if (is.factor(m[[dv]])) {
        lev <- levels(m[[dv]])[1]
      } else if (is.logical(m[[dv]])) {
        lev <- TRUE
      } else {
        stop("The level to use for classification is not clear. Use a factor of logical as the response variable")
      }
    }
  } else {
    stop("The model object does not seems to be a random forest")
  }

  if (is.empty(num.trees)) {
    num.trees <- object$call[["num.trees"]]
  }
  if (is.empty(sample.fraction)) {
    sample.fraction <- object$call[["sample.fraction"]]
    sample.fraction <- ifelse(is.null(sample.fraction), 1, sample.fraction)
  } else {
    object$call[["replace"]] <- FALSE
  }

  set.seed(seed)
  tune_grid <- expand.grid(mtry = mtry, min.node.size = min.node.size, num.trees = num.trees, sample.fraction = sample.fraction)
  out <- data.frame(
    mean = NA, std = NA, min = NA, max = NA,
    mtry = tune_grid[["mtry"]], min.node.size = tune_grid[["min.node.size"]],
    num.trees = tune_grid[["num.trees"]], sample.fraction = tune_grid[["sample.fraction"]]
  )

  if (missing(fun)) {
    if (type == "classification") {
      fun <- radiant.model::auc
      cn <- "AUC (mean)"
    } else {
      fun <- radiant.model::RMSE
      cn <- "RMSE (mean)"
    }
  } else {
    cn <- glue("{deparse(substitute(fun))} (mean)")
  }

  if (length(shiny::getDefaultReactiveDomain()) > 0) {
    trace <- FALSE
    incProgress <- shiny::incProgress
    withProgress <- shiny::withProgress
  } else {
    incProgress <- function(...) {}
    withProgress <- function(...) list(...)[["expr"]]
  }

  nitt <- nrow(tune_grid)
  withProgress(message = "Running cross-validation (rforest)", value = 0, {
    for (i in seq_len(nitt)) {
      perf <- double(K * repeats)
      object$call[["mtry"]] <- tune_grid[i, "mtry"]
      object$call[["min.node.size"]] <- tune_grid[i, "min.node.size"]
      object$call[["num.trees"]] <- tune_grid[i, "num.trees"]
      object$call[["sample.fraction"]] <- tune_grid[i, "sample.fraction"]
      if (trace) {
        cat("Working on mtry", tune_grid[i, "mtry"], "num.trees", tune_grid[i, "num.trees"], "\n")
      }
      for (j in seq_len(repeats)) {
        rand <- sample(K, nrow(m), replace = TRUE)
        for (k in seq_len(K)) {
          object$call[["data"]] <- quote(m[rand != k, , drop = FALSE])
          if (length(weights) > 0) {
            object$call[["case.weights"]] <- weights[rand != k]
          }
          if (type == "classification") {
            pred <- predict(eval(object$call), m[rand == k, , drop = FALSE])$prediction[, 1]
            if (missing(...)) {
              perf[k + (j - 1) * K] <- fun(pred, unlist(m[rand == k, dv]), lev)
            } else {
              perf[k + (j - 1) * K] <- fun(pred, unlist(m[rand == k, dv]), lev, ...)
            }
          } else {
            pred <- predict(eval(object$call), m[rand == k, , drop = FALSE])$prediction
            rvar <- unlist(m[rand == k, dv])
            if (missing(...)) {
              perf[k + (j - 1) * K] <- fun(pred, rvar)
            } else {
              perf[k + (j - 1) * K] <- fun(pred, rvar, ...)
            }
          }
        }
      }
      out[i, 1:4] <- c(mean(perf), sd(perf), min(perf), max(perf))
      incProgress(1 / nitt, detail = paste("\nCompleted run", i, "out of", nitt))
    }
  })

  if (type == "classification") {
    out <- arrange(out, desc(mean))
  } else {
    out <- arrange(out, mean)
  }
  ## show evaluation metric in column name
  colnames(out)[1] <- cn
  out
}
#' Neural Networks
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nn.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param rvar The response variable in the model
#' @param evar Explanatory variables in the model
#' @param type Model type (i.e., "classification" or "regression")
#' @param lev The level in the response variable defined as _success_
#' @param size Number of units (nodes) in the hidden layer
#' @param decay Parameter decay
#' @param wts Weights to use in estimation
#' @param seed Random seed to use as the starting point
#' @param check Optional estimation parameters ("standardize" is the default)
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list with all variables defined in nn as an object of class nn
#'
#' @examples
#' nn(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>% summary()
#' nn(titanic, "survived", c("pclass", "sex")) %>% str()
#' nn(diamonds, "price", c("carat", "clarity"), type = "regression") %>% summary()
#'
#' @seealso \code{\link{summary.nn}} to summarize results
#' @seealso \code{\link{plot.nn}} to plot results
#' @seealso \code{\link{predict.nn}} for prediction
#'
#' @importFrom nnet nnet
#'
#' @export
nn <- function(
  dataset, rvar, evar,
  type = "classification", lev = "",
  size = 1, decay = .5, wts = "None",
  seed = NA, check = "standardize",
  data_filter = ""
) {

  if (rvar %in% evar) {
    return("Response variable contained in the set of explanatory variables.\nPlease update model specification." %>%
      add_class("nn"))
  } else if (is_empty(size) || size < 1) {
    return("Size should be larger than or equal to 1." %>% add_class("nn"))
  } else if (is_empty(decay) || decay < 0) {
    return("Decay should be larger than or equal to 0." %>% add_class("nn"))
  }

  vars <- c(rvar, evar)

  if (is_empty(wts, "None")) {
    wts <- NULL
  } else if (is_string(wts)) {
    wtsname <- wts
    vars <- c(rvar, evar, wtsname)
  }

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, vars, filt = data_filter)

  if (!is_empty(wts)) {
    if (exists("wtsname")) {
      wts <- dataset[[wtsname]]
      dataset <- select_at(dataset, .vars = base::setdiff(colnames(dataset), wtsname))
    }
    if (length(wts) != nrow(dataset)) {
      return(
        paste0("Length of the weights variable is not equal to the number of rows in the dataset (", format_nr(length(wts), dec = 0), " vs ", format_nr(nrow(dataset), dec = 0), ")") %>%
          add_class("nn")
      )
    }
  }

  if (any(summarise_all(dataset, .funs = funs(does_vary)) == FALSE)) {
    return("One or more selected variables show no variation. Please select other variables." %>% add_class("nn"))
  }

  rv <- dataset[[rvar]]

  if (type == "classification") {
    linout <- FALSE; entropy <- TRUE
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

    ## transformation to TRUE/FALSE depending on the selected level (lev)
    dataset[[rvar]] <- dataset[[rvar]] == lev
  } else {
    linout <- TRUE; entropy <- FALSE
  }

  ## standardize data to limit stability issues ...
  # http://stats.stackexchange.com/questions/23235/how-do-i-improve-my-neural-network-stability
  if ("standardize" %in% check) {
    dataset <- scaledf(dataset, wts = wts)
  }

  vars <- evar
  ## in case : is used
  if (length(vars) < (ncol(dataset) - 1)) {
    vars <- evar <- colnames(dataset)[-1]
  }

  ## use decay http://stats.stackexchange.com/a/70146/61693
  nninput <- list(
    formula = as.formula(paste(rvar, "~ . ")),
    rang = .1, size = size, decay = decay, weights = wts,
    maxit = 10000, linout = linout, entropy = entropy,
    skip = FALSE, trace = FALSE, data = dataset
  )

  ## based on http://stackoverflow.com/a/14324316/1974918
  seed <- gsub("[^0-9]", "", seed)
  if (!is_empty(seed)) {
    if (exists(".Random.seed")) {
      gseed <- .Random.seed
      on.exit(.Random.seed <<- gseed)
    }
    set.seed(seed)
  }

  ## need do.call so Garson/Olden plot will work
  model <- do.call(nnet::nnet, nninput)
  coefnames <- model$coefnames
  hasLevs <- sapply(select(dataset, -1), function(x) is.factor(x) || is.logical(x) || is.character(x))
  if (sum(hasLevs) > 0) {
    for (i in names(hasLevs[hasLevs])) {
      coefnames %<>% gsub(paste0("^", i), paste0(i, "|"), .) %>%
        gsub(paste0(":", i), paste0(":", i, "|"), .)
    }
    rm(i, hasLevs)
  }

  ## nn returns residuals as a matrix
  model$residuals <- model$residuals[, 1]

  ## nn model object does not include the data by default
  model$model <- dataset
  rm(dataset) ## dataset not needed elsewhere

  as.list(environment()) %>% add_class(c("nn", "model"))
}

#' Center or standardize variables in a data frame
#'
#' @param dataset Data frame
#' @param center Center data (TRUE or FALSE)
#' @param scale Scale data (TRUE or FALSE)
#' @param sf Scaling factor (default is 2)
#' @param wts Weights to use (default is NULL for no weights)
#' @param calc Calculate mean and sd or use attributes attached to dat
#'
#' @return Scaled data frame
#'
#' @seealso \code{\link{copy_attr}} to copy attributes from a training to a validation dataset
#'
#' @export
scaledf <- function(
  dataset, center = TRUE, scale = TRUE,
  sf = 2, wts = NULL, calc = TRUE
) {

  isNum <- sapply(dataset, function(x) is.numeric(x))
  if (sum(isNum) == 0) return(dataset)
  cn <- names(isNum)[isNum]

  ## remove set_attr calls when dplyr removes and keep attributes appropriately
  descr <- attr(dataset, "description")
  if (calc) {
    if (length(wts) == 0) {
      ms <- summarise_at(dataset, .vars = cn, .funs = funs(mean(., na.rm = TRUE))) %>%
        set_attr("description", NULL)
      if (scale) {
        sds <- summarise_at(dataset, .vars = cn, .funs = funs(sd(., na.rm = TRUE))) %>%
          set_attr("description", NULL)
      }
    } else {
      ms <- summarise_at(dataset, .vars = cn, .funs = funs(weighted.mean(., wts, na.rm = TRUE))) %>%
        set_attr("description", NULL)
      if (scale) {
        sds <- summarise_at(dataset, .vars = cn, .funs = funs(weighted.sd(., wts, na.rm = TRUE))) %>%
          set_attr("description", NULL)
      }
    }
  } else {
    ms <- attr(dataset, "ms")
    sds <- attr(dataset, "sds")
    if (is.null(ms) && is.null(sds)) return(dataset)
  }
  if (center && scale) {
    mutate_at(dataset, .vars = intersect(names(ms), cn), .funs = funs((. - ms$.) / (sf * sds$.))) %>%
      set_attr("ms", ms) %>%
      set_attr("sds", sds) %>%
      set_attr("description", descr)
  } else if (center) {
    mutate_at(dataset, .vars = intersect(names(ms), cn), .funs = funs(. - ms$.)) %>%
      set_attr("ms", ms) %>%
      set_attr("description", descr)
  } else if (scale) {
    mutate_at(dataset, .vars = intersect(names(sds), cn), .funs = funs(. / (sf * sds$.))) %>%
      set_attr("sds", sds) %>%
      set_attr("description", descr)
  } else {
    dataset
  }
}

#' Summary method for the nn function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nn.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{nn}}
#' @param prn Print list of weights
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- nn(titanic, "survived", "pclass", lev = "Yes")
#' summary(result)
#'
#' @seealso \code{\link{nn}} to generate results
#' @seealso \code{\link{plot.nn}} to plot results
#' @seealso \code{\link{predict.nn}} for prediction
#'
#' @export
summary.nn <- function(object, prn = TRUE, ...) {

  if (is.character(object)) return(object)
  cat("Neural Network\n")
  if (object$type == "classification") {
    cat("Activation function  : Logistic (classification)")
  } else {
    cat("Activation function  : Linear (regression)")
  }
  cat("\nData                 :", object$df_name)
  if (object$data_filter %>% gsub("\\s", "", .) != "") {
    cat("\nFilter               :", gsub("\\n", "", object$data_filter))
  }
  cat("\nResponse variable    :", object$rvar)
  if (object$type == "classification") {
    cat("\nLevel                :", object$lev, "in", object$rvar)
  }
  cat("\nExplanatory variables:", paste0(object$evar, collapse = ", "), "\n")
  if (length(object$wtsname) > 0) {
    cat("Weights used         :", object$wtsname, "\n")
  }
  cat("Network size         :", object$size, "\n")
  cat("Parameter decay      :", object$decay, "\n")
  if (!is_empty(object$seed)) {
    cat("Seed                 :", object$seed, "\n")
  }

  network <- paste0(object$model$n, collapse = "-")
  nweights <- length(object$model$wts)
  cat("Network              :", network, "with", nweights, "weights\n")

  if (!is_empty(object$wts, "None") && (length(unique(object$wts)) > 2 || min(object$wts) >= 1)) {
    cat("Nr obs               :", format_nr(sum(object$wts), dec = 0), "\n")
  } else {
    cat("Nr obs               :", format_nr(length(object$rv), dec = 0), "\n")
  }

  if (object$model$convergence != 0) {
    cat("\n** The model did not converge **")
  } else {
    if (prn) {
      cat("Weights              :\n")
      out <- capture.output(summary(object$model))[-1:-2]
      if (nchar(out[1]) > 150) {
        oop <- base::options(width = 100)
        on.exit(base::options(oop), add = TRUE)
      }
      gsub("^", "  ", out) %>%
      paste0(collapse = "\n") %>%
      cat("\n")
    }
  }
}

#' Plot method for the nn function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nn.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{nn}}
#' @param shiny Did the function call originate inside a shiny app
#' @param plots Plots to produce for the specified Neural Network model. Use "" to avoid showing any plots (default). Options are "olden" or "garson" for importance plots, or "net" to depict the network structure
#' @param size Font size used
#' @param nrobs Number of data points to show in scatter plots (-1 for all)
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- nn(titanic, "survived", c("pclass", "sex"), lev = "Yes")
#' plot(result, plots = "net")
#' plot(result, plots = "olden")
#'
#' @seealso \code{\link{nn}} to generate results
#' @seealso \code{\link{summary.nn}} to summarize results
#' @seealso \code{\link{predict.nn}} for prediction
#'
#' @importFrom NeuralNetTools plotnet olden garson
#' @importFrom graphics par
#'
#' @export
plot.nn <- function(
  x, plots = "garson", size = 12, nrobs = -1,
  shiny = FALSE, custom = FALSE, ...
) {

  if (is.character(x)) return(x)
  plot_list <- list()
  ncol <- 1

  if ("olden" %in% plots || "olsen" %in% plots) { ## legacy for typo
    plot_list[["olsen"]] <- NeuralNetTools::olden(x$model, x_lab = x$coefnames, cex_val = 4) +
      coord_flip() +
      theme_set(theme_gray(base_size = size)) +
      theme(legend.position = "none") +
      labs(title = paste0("Olden plot of variable importance (size = ", x$size, ", decay = ", x$decay, ")"))
  }

  if ("garson" %in% plots) {
    plot_list[["garson"]] <- NeuralNetTools::garson(x$model, x_lab = x$coefnames) +
      coord_flip() +
      theme_set(theme_gray(base_size = size)) +
      theme(legend.position = "none") +
      labs(title = paste0("Garson plot of variable importance (size = ", x$size, ", decay = ", x$decay, ")"))
  }

  if ("net" %in% plots) {
    ## don't need as much spacing at the top and bottom
    mar <- par(mar = c(0, 4.1, 0, 2.1))
    on.exit(par(mar = mar$mar))
    return(do.call(NeuralNetTools::plotnet, list(mod_in = x$model, x_names = x$coefnames, cex_val = size / 16)))
  }

  if (x$type == "regression" && "dashboard" %in% plots) {
    plot_list <- plot.regress(x, plots = "dashboard", lines = "line", nrobs = nrobs, custom = TRUE)
    ncol <- 2
  }

  if (length(plot_list) > 0) {
    if (custom) {
      if (length(plot_list) == 1) {
        return(plot_list[[1]])
      } else {
        return(plot_list)
      }
    }

    sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = ncol)) %>% {
      if (shiny) . else print(.)
    }
  }
}

#' Predict method for the nn function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nn.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{nn}}
#' @param pred_data Provide the dataframe to generate predictions (e.g., diamonds). The dataset must contain all columns used in the estimation
#' @param pred_cmd Generate predictions using a command. For example, `pclass = levels(pclass)` would produce predictions for the different levels of factor `pclass`. To add another variable, create a vector of prediction strings, (e.g., c('pclass = levels(pclass)', 'age = seq(0,100,20)')
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- nn(titanic, "survived", c("pclass", "sex"), lev = "Yes")
#' predict(result, pred_cmd = "pclass = levels(pclass)")
#' result <- nn(diamonds, "price", "carat:color", type = "regression")
#' predict(result, pred_cmd = "carat = 1:3")
#' predict(result, pred_data = diamonds) %>% head()
#'
#' @seealso \code{\link{nn}} to generate the result
#' @seealso \code{\link{summary.nn}} to summarize results
#'
#' @export
predict.nn <- function(
  object, pred_data = NULL, pred_cmd = "",
  dec = 3, ...
) {

  if (is.character(object)) return(object)

  ## ensure you have a name for the prediction dataset
  if (is.data.frame(pred_data)) {
    df_name <- deparse(substitute(pred_data))
  } else {
    df_name <- pred_data
  }

  pfun <- function(model, pred, se, conf_lev) {
    pred_val <- try(sshhr(predict(model, pred)), silent = TRUE)

    if (!inherits(pred_val, "try-error")) {
      pred_val %<>% as.data.frame(stringsAsFactors = FALSE) %>%
        select(1) %>%
        set_colnames("Prediction")
    }

    pred_val
  }

  predict_model(object, pfun, "nn.predict", pred_data, pred_cmd, conf_lev = 0.95, se = FALSE, dec) %>%
    set_attr("pred_data", df_name)
}

#' Print method for predict.nn
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#'
#' @export
print.nn.predict <- function(x, ..., n = 10)
  print_predict_model(x, ..., n = n, header = "Neural Network")

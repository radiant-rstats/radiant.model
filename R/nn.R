#' Neural Networks
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nn.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param rvar The response variable in the model
#' @param evar Explanatory variables in the model
#' @param type Model type (i.e., "classification" or "regression")
#' @param lev The level in the response variable defined as _success_
#' @param size Number of units (nodes) in the hidden layer
#' @param decay Paramater decay
#' @param wts Weights to use in estimation
#' @param seed Random seed to use as the starting point
#' @param check Optional estimation parameters ("standardize" is the default)
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list with all variables defined in nn as an object of class nn
#'
#' @examples
#' result <- nn("titanic", "survived", c("pclass","sex"), lev = "Yes")
#' result <- nn("titanic", "survived", c("pclass","sex"))
#' result <- nn("diamonds", "price", c("carat","clarity"), type = "regression")
#'
#' @seealso \code{\link{summary.nn}} to summarize results
#' @seealso \code{\link{plot.nn}} to plot results
#' @seealso \code{\link{predict.nn}} for prediction
#'
#' @importFrom nnet nnet
#'
#' @export
nn <- function(dataset, rvar, evar,
               type = "classification",
               lev = "",
               size = 1,
               decay = .5,
               wts = "None",
               seed = NA,
               check = "standardize",
               data_filter = "") {

  if (rvar %in% evar) {
    return("Response variable contained in the set of explanatory variables.\nPlease update model specification." %>%
      add_class("nn"))
  }

  if (is_empty(size) || size < 1) {
    return("Size should be larger than or equal to 1." %>% add_class("nn"))
  }

  if (is_empty(decay) || decay < 0) {
    return("Decay should be larger than or equal to 0." %>% add_class("nn"))
  }

  if (!is.null(wts) && wts == "None") {
    wts <- NULL
    vars <- c(rvar, evar)
  } else {
    wtsname <- wts
    vars <- c(rvar, evar, wtsname)
  }

  dat <- getdata(dataset, vars, filt = data_filter)
  if (!is_string(dataset)) dataset <- deparse(substitute(dataset)) %>% set_attr("df", TRUE)

  if (!is.null(wts)) {
    wts <- dat[[wtsname]]
    dat <- select_at(dat, .vars = setdiff(colnames(dat), wtsname))
  }

  if (any(summarise_all(dat, .funs = funs(does_vary)) == FALSE)) {
    return("One or more selected variables show no variation. Please select other variables." %>% add_class("nn"))
  }

  rv <- dat[[rvar]]

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
    dat[[rvar]] <- dat[[rvar]] == lev
  } else {
    linout <- TRUE; entropy <- FALSE
  }

  ## standardize data to limit stability issues ...
  # http://stats.stackexchange.com/questions/23235/how-do-i-improve-my-neural-network-stability
  if ("standardize" %in% check) dat <- scaledf(dat, wts = wts)

  vars <- evar
  ## in case : is used
  if (length(vars) < (ncol(dat) - 1)) {
    vars <- evar <- colnames(dat)[-1]
  }

  ## use decay http://stats.stackexchange.com/a/70146/61693
  nninput <- list(
    formula = as.formula(paste(rvar, "~ . ")),
    rang = .1, size = size, decay = decay, weights = wts,
    maxit = 10000, linout = linout, entropy = entropy,
    skip = FALSE, trace = FALSE, data = dat
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
  isFct <- sapply(select(dat, -1), function(x) is.factor(x) || is.logical(x))
  if (sum(isFct) > 0) {
    for (i in names(isFct[isFct]))
      coefnames <- gsub(i, paste0(i, "|"), coefnames) %>% gsub("\\|\\|", "\\|", .)
    rm(i, isFct)
  }

  ## nn returns residuals as a matrix
  model$residuals <- model$residuals[, 1]

  ## nn model object does not include the data by default
  model$model <- dat
  rm(dat) ## dat not needed elsewhere

  as.list(environment()) %>% add_class(c("nn", "model"))
}

#' Center or standardize variables in a data frame
#'
#' @param dat Data frame
#' @param center Center data (TRUE or FALSE)
#' @param scale Scale data (TRUE or FALSE)
#' @param sf Scaling factor (default is 2)
#' @param wts Weights to use (default is NULL for no weights)
#' @param calc Calculate mean and sd or use attributes attached to dat
#'
#' @return Scaled data frame
#'
#' @seealso \code{\link{copy_attr}} to copy attributes from a traning to a validation dataset
#'
#' @export
scaledf <- function(dat, center = TRUE, scale = TRUE, sf = 2, wts = NULL, calc = TRUE) {
  # isNum <- sapply(dat, function(x) is.numeric(x) || is.logical(x))
  isNum <- sapply(dat, function(x) is.numeric(x))
  if (sum(isNum) == 0) return(dat)
  cn <- names(isNum)[isNum]

  ## remove set_attr calls when dplyr removes and keep attributes appropriately
  desc <- attr(dat, "description")

  if (calc) {
    if (length(wts) == 0) {
      ms <- summarise_at(dat, .vars = cn, .funs = funs(mean(., na.rm = TRUE))) %>%
        set_attr("description", NULL)
      if (scale) {
        sds <- summarise_at(dat, .vars = cn, .funs = funs(sd(., na.rm = TRUE))) %>%
          set_attr("description", NULL)
      }
    } else {
      ms <- summarise_at(dat, .vars = cn, .funs = funs(weighted.mean(., wts, na.rm = TRUE))) %>%
        set_attr("description", NULL)
      if (scale) {
        sds <- summarise_at(dat, .vars = cn, .funs = funs(weighted.sd(., wts, na.rm = TRUE))) %>%
          set_attr("description", NULL)
      }
    }
  } else {
    ms <- attr(dat, "ms")
    sds <- attr(dat, "sds")
    if (is.null(ms) && is.null(sds)) return(dat)
  }
  if (center && scale) {
    mutate_at(dat, .vars = intersect(names(ms), cn), .funs = funs((. - ms$.) / (sf * sds$.))) %>%
      set_attr("ms", ms) %>%
      set_attr("sds", sds) %>%
      set_attr("description", desc)
  } else if (center) {
    mutate_at(dat, .vars = intersect(names(ms), cn), .funs = funs(. - ms$.)) %>%
      set_attr("ms", ms) %>%
      set_attr("description", desc)
  } else if (scale) {
    mutate_at(dat, .vars = intersect(names(sds), cn), .funs = funs(. / (sf * sds$.))) %>%
      set_attr("sds", sds) %>%
      set_attr("description", desc)
  } else {
    dat
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
#' result <- nn("titanic", "survived", "pclass", lev = "Yes")
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
  cat("\nData                 :", object$dataset)
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

  if (!is_empty(object$wts, "None") && class(object$wts) == "integer") {
    cat("Nr obs               :", formatnr(sum(object$wts), dec = 0), "\n")
  } else {
    cat("Nr obs               :", formatnr(length(object$rv), dec = 0), "\n")
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
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This opion can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org/} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- nn("titanic", "survived", c("pclass","sex"), lev = "Yes")
#' plot(result, plots = c("olden","net"))
#'
#' @seealso \code{\link{nn}} to generate results
#' @seealso \code{\link{summary.nn}} to summarize results
#' @seealso \code{\link{predict.nn}} for prediction
#'
#' @importFrom NeuralNetTools plotnet olden garson
#' @importFrom graphics par
#'
#' @export
plot.nn <- function(x, plots = "garson", size = 12, shiny = FALSE, custom = FALSE, ...) {
  object <- x
  rm(x)
  if (is.character(object)) return(object)
  plot_list <- list()

  if ("olden" %in% plots || "olsen" %in% plots) { ## legacy for typo
    plot_list[["olsen"]] <- NeuralNetTools::olden(object$model, x_lab = object$coefnames, cex_val = 4) +
      coord_flip() +
      theme_set(theme_gray(base_size = size)) +
      theme(legend.position = "none") +
      labs(title = "Olden plot of variable importance")
  }

  if ("garson" %in% plots) {
    plot_list[["garson"]] <- NeuralNetTools::garson(object$model, x_lab = object$coefnames) +
      coord_flip() +
      theme_set(theme_gray(base_size = size)) +
      theme(legend.position = "none") +
      labs(title = "Garson plot of variable importance")
  }

  if ("net" %in% plots) {
    ## don't need as much spacing at the top and bottom
    mar <- par(mar = c(0, 4.1, 0, 2.1))
    on.exit(par(mar = mar$mar))
    return(do.call(NeuralNetTools::plotnet, list(mod_in = object$model, x_names = object$coefnames, cex_val = size / 16)))
  }

  if (length(plot_list) > 0) {
    if (custom) {
      if (length(plot_list) == 1) {
        return(plot_list[[1]])
      } else {
        return(plot_list)
      }
    }

    sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = 1)) %>% {
      if (shiny) . else print(.)
    }
  }
}

#' Predict method for the nn function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nn.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{nn}}
#' @param pred_data Provide the name of a dataframe to generate predictions (e.g., "titanic"). The dataset must contain all columns used in the estimation
#' @param pred_cmd Generate predictions using a command. For example, `pclass = levels(pclass)` would produce predictions for the different levels of factor `pclass`. To add another variable use a `,` (e.g., `pclass = levels(pclass), age = seq(0,100,20)`)
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param se Logical that indicates if prediction standard errors should be calculated (default = FALSE)
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- nn("titanic", "survived", c("pclass","sex"), lev = "Yes")
#' predict(result, pred_cmd = "pclass = levels(pclass)")
#' result <- nn("diamonds", "price", "carat:color", type = "regression")
#' predict(result, pred_cmd = "carat = 1:3")
#' predict(result, pred_data = "diamonds") %>% head
#'
#' @seealso \code{\link{nn}} to generate the result
#' @seealso \code{\link{summary.nn}} to summarize results
#'
#' @export
predict.nn <- function(object,
                        pred_data = "",
                        pred_cmd = "",
                        conf_lev = 0.95,
                        se = FALSE,
                        dec = 3,
                        ...) {
  if (is.character(object)) return(object)

  ## ensure you have a name for the prediction dataset
  if (!is.character(pred_data)) {
    attr(pred_data, "pred_data") <- deparse(substitute(pred_data))
  }

  pfun <- function(model, pred, se, conf_lev) {
    pred_val <- try(sshhr(predict(model, pred)), silent = TRUE)

    if (!is(pred_val, "try-error")) {
      pred_val %<>% as.data.frame(stringsAsFactors = FALSE) %>%
        select(1) %>%
        set_colnames("Prediction")
    }

    pred_val
  }

  predict_model(object, pfun, "nn.predict", pred_data, pred_cmd, conf_lev, se, dec)
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

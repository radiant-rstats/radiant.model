#' Artificial Neural Networks
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/ann.html} for an example in Radiant
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
#' @return A list with all variables defined in ann as an object of class ann
#'
#' @examples
#' result <- ann("titanic", "survived", c("pclass","sex"), lev = "Yes")
#' result <- ann("titanic", "survived", c("pclass","sex"))
#' result <- ann("diamonds", "price", c("carat","clarity"), type = "regression")
#'
#' @seealso \code{\link{summary.ann}} to summarize results
#' @seealso \code{\link{plot.ann}} to plot results
#' @seealso \code{\link{predict.ann}} for prediction
#'
#' @importFrom nnet nnet
#'
#' @export
ann <- function(dataset, rvar, evar,
                type = "classification",
                lev = "",
                size = 1,
                decay = .5,
                wts = "None",
                seed = NA,
                check = "standardize",
                data_filter = "") {

  if (rvar %in% evar)
    return("Response variable contained in the set of explanatory variables.\nPlease update model specification." %>%
           add_class("ann"))

  if (is_empty(size) || size < 1)
    return("Size should be larger than or equal to 1." %>% add_class("ann"))

  if (is_empty(decay) || decay < 0)
    return("Decay should be larger than or equal to 0." %>% add_class("ann"))

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
    dat <- select_(dat, .dots = paste0("-",wtsname))
  }

  if (any(summarise_all(dat, funs(does_vary)) == FALSE))
    return("One or more selected variables show no variation. Please select other variables." %>% add_class("ann"))

  rv <- dat[[rvar]]

  if (type == "classification") {

    linout <- FALSE; entropy = TRUE; skip = FALSE
    if (lev == "") {
      if (is.factor(rv))
        lev <- levels(rv)[1]
      else
        lev <- rv %>% as.character %>% as.factor %>% levels %>% .[1]
    }

    ## transformation to TRUE/FALSE depending on the selected level (lev)
    dat[[rvar]] <- dat[[rvar]] == lev
  } else {
    linout = TRUE; entropy = FALSE; skip = FALSE
  }

  ## standardize data to limit stability issues ...
  # http://stats.stackexchange.com/questions/23235/how-do-i-improve-my-neural-network-stability
  if ("standardize" %in% check) dat <- scaledf(dat, wts = wts)

  vars <- evar
  ## in case : is used
  if (length(vars) < (ncol(dat)-1)) 
    vars <- evar <- colnames(dat)[-1]

  ## use decay
  # http://stats.stackexchange.com/a/70146/61693
  form <- paste(rvar, "~ . ")
  nninput <- list(formula = as.formula(form),
              rang = .1, size = size, decay = decay, weights = wts, maxit = 10000,
              linout = linout, entropy = entropy, skip = skip, trace = FALSE, data = dat)

  ## based on http://stackoverflow.com/a/14324316/1974918
  seed <- seed %>% gsub("[^0-9]","",.) 
  if (!is_empty(seed)) {
    if (exists(".Random.seed")) {
      gseed <- .Random.seed
      on.exit({.Random.seed <<- gseed})
    }
    set.seed(seed) 
  }

  ## need do.call so Garson/Olden plot will work
  model <- do.call(nnet::nnet, nninput)

  coefnames <- model$coefnames
  isFct <- sapply(select(dat,-1), function(x) is.factor(x) || is.logical(x))
  if (sum(isFct) > 0) {
    for (i in names(isFct[isFct])) 
      coefnames <- gsub(i, paste0(i,"|"), coefnames) %>% gsub("\\|\\|","\\|", .)
    rm(i, isFct)
  }

  ## ann returns residuals as a matrix
  model$residuals <- model$residuals[,1]

  ## ann model object does not include the data by default
  model$model <- dat
  rm(dat) ## dat not needed elsewhere

  as.list(environment()) %>% add_class(c("ann","model"))
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
  isNum <- sapply(dat, is.numeric)
  if (sum(isNum) == 0) return(dat)
  cn <- names(isNum)[isNum]

  if (calc) {
    if (length(wts) == 0) {
      ms <- summarise_at(dat, .cols = cn, .funs = funs(mean(., na.rm = TRUE)))
      if (scale)
        sds <- summarise_at(dat, .cols = cn, .funs = funs(sd(., na.rm = TRUE)))
    } else {
      ms <- summarise_at(dat, .cols = cn, .funs = funs(weighted.mean(., wts, na.rm = TRUE)))
      if (scale)
        sds <- summarise_at(dat, .cols = cn, .funs = funs(weighted.sd(., wts, na.rm = TRUE)))
    }
  } else {
    ms <- attr(dat,"ms")
    sds <- attr(dat,"sds")
    if (is.null(ms) && is.null(sds)) return(dat)
  }
  if (center && scale) {
    mutate_at(dat, .cols = intersect(names(ms), cn), .funs = funs((. - ms$.) / (sf * sds$.))) %>%
      set_attr("ms", ms) %>%
      set_attr("sds", sds)
  } else if (center) {
    mutate_at(dat, .cols = intersect(names(ms), cn), .funs = funs(. - ms$.)) %>%
      set_attr("ms", ms)
  } else if (scale) {
    mutate_at(dat, .cols = intersect(names(sds), cn), .funs = funs(. / (sf * sds$.))) %>%
      set_attr("sds", sds)
  } else {
    dat
  }
}

#' Summary method for the ann function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/ann.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{ann}}
#' @param prn Print list of weights
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- ann("titanic", "survived", "pclass", lev = "Yes")
#' summary(result)
#'
#' @seealso \code{\link{ann}} to generate results
#' @seealso \code{\link{plot.ann}} to plot results
#' @seealso \code{\link{predict.ann}} for prediction
#'
#' @export
summary.ann <- function(object, prn = TRUE, ...) {

  if (is.character(object)) return(object)

  cat("Artificial Neural Network (ANN)\n")
  if (object$type == "classification")
    cat("Activation function  : Logistic (classification)")
  else
    cat("Activation function  : Linear (regression)")
  cat("\nData                 :", object$dataset)
  if (object$data_filter %>% gsub("\\s","",.) != "")
    cat("\nFilter               :", gsub("\\n","", object$data_filter))
  cat("\nResponse variable    :", object$rvar)
  if (object$type == "classification")
    cat("\nLevel                :", object$lev, "in", object$rvar)
  cat("\nExplanatory variables:", paste0(object$evar, collapse=", "),"\n")
  if (length(object$wtsname) > 0)
    cat("Weights used         :", object$wtsname, "\n")
  cat("Network size         :", object$size, "\n")
  cat("Parameter decay      :", object$decay, "\n")
  if (!is_empty(object$seed))
    cat("Seed                 :", object$seed, "\n")

  network <- paste0(object$model$n, collapse = "-")
  nweights <- length(object$model$wts)
  cat("Network              :", network, "with", nweights, "weights\n")

  if (!is_empty(object$wts, "None") && class(object$wts) == "integer")
    cat("Nr obs               :", formatnr(sum(object$wts), dec = 0), "\n")
  else
    cat("Nr obs               :", formatnr(length(object$rv), dec = 0), "\n")

  if (object$model$convergence != 0) {
    cat("\n** The model did not converge **")
  } else {
    if (prn) {
      cat("Weights              :\n")
      cat(paste0(capture.output(print(summary(object$model)))[c(-1, -2)], collapse = "\n"))
    }
  }
}

#' Plot method for the ann function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/ann.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{ann}}
#' @param shiny Did the function call originate inside a shiny app
#' @param plots Plots to produce for the specified ANN model. Use "" to avoid showing any plots (default). Options are "olden" or "garson" for importance plots, or "net" to depict the network structure
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This opion can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org/} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- ann("titanic", "survived", c("pclass","sex"), lev = "Yes")
#' plot(result, plots = c("olden","net"))
#'
#' @seealso \code{\link{ann}} to generate results
#' @seealso \code{\link{summary.ann}} to summarize results
#' @seealso \code{\link{predict.ann}} for prediction
#'
#' @importFrom NeuralNetTools plotnet olden garson
#'
#' @export
plot.ann <- function(x, plots = "garson", shiny = FALSE, custom = FALSE, ...) {

  object <- x; rm(x)
  if (is.character(object)) return(object)
  plot_list <- list()

  if ("olsen" %in% plots || "olden" %in% plots) ## legacy for typo
    plot_list[["olsen"]] <- NeuralNetTools::olden(object$model, x_lab = object$coefnames) + 
      coord_flip()

  if ("garson" %in% plots) 
    plot_list[["garson"]] <- NeuralNetTools::garson(object$model, x_lab = object$coefnames) + 
      coord_flip()

  if ("net" %in% plots) 
    return(do.call(NeuralNetTools::plotnet, list(mod_in = object$model, x_names = object$coefnames)))

  if (length(plot_list) > 0) {
    if (custom)
      if (length(plot_list) == 1) return(plot_list[[1]]) else return(plot_list)

    sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = 1)) %>%
      {if (shiny) . else print(.)}
  }
}

#' Predict method for the ann function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/ann.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{ann}}
#' @param pred_data Provide the name of a dataframe to generate predictions (e.g., "titanic"). The dataset must contain all columns used in the estimation
#' @param pred_cmd Generate predictions using a command. For example, `pclass = levels(pclass)` would produce predictions for the different levels of factor `pclass`. To add another variable use a `,` (e.g., `pclass = levels(pclass), age = seq(0,100,20)`)
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param se Logical that indicates if prediction standard errors should be calculated (default = FALSE)
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- ann("titanic", "survived", c("pclass","sex"), lev = "Yes")
#' predict(result, pred_cmd = "pclass = levels(pclass)")
#' result <- ann("diamonds", "price", "carat:color", type = "regression")
#' predict(result, pred_cmd = "carat = 1:3")
#' predict(result, pred_data = "diamonds") %>% head
#'
#' @seealso \code{\link{ann}} to generate the result
#' @seealso \code{\link{summary.ann}} to summarize results
#'
#' @export
predict.ann <- function(object,
                        pred_data = "",
                        pred_cmd = "",
                        conf_lev = 0.95,
                        se = FALSE,
                        dec = 3,
                        ...) {

  if (is.character(object)) return(object)
  pfun <- function(model, pred, se, conf_lev) {
    pred_val <- try(sshhr(predict(model, pred)), silent = TRUE)

    if (!is(pred_val, 'try-error'))
      pred_val %<>% as.data.frame %>% select(1) %>% set_colnames("Prediction")

    pred_val
  }

  predict_model(object, pfun, "ann.predict", pred_data, pred_cmd, conf_lev, se, dec)
}

#' Print method for predict.ann
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#'
#' @export
print.ann.predict <- function(x, ..., n = 10)
  print_predict_model(x, ..., n = n, header = "Artificial Neural Network (ANN)")

#' Deprecated function to store predictions from an ANN
#'
#' @details Use \code{\link{store.model.predict}} or \code{\link{store.model}} instead
#'
#' @param object Return value from \code{\link{predict.ann}}
#' @param data Dataset name
#' @param name Variable name assigned to the residuals or predicted values
#'
#' @export
store_ann <- function(object, data = object$dataset, name = paste0("predict_ann"))
  store.model.predict(object, data = data, name = name)


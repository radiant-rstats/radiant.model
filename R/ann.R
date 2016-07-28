#' Artificial Neural Networks
#'
#' @details See \url{http://radiant-rstats.github.io/docs/model/ann.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param rvar The response variable in the logit (probit) model
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
  if (!is_string(dataset)) dataset <- "-----"

  if (!is.null(wts)) {
    wts <- dat[[wtsname]]
    dat <- select_(dat, .dots = paste0("-",wtsname))
  }

  if (any(summarise_each(dat, funs(does_vary)) == FALSE))
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
  if (length(vars) < (ncol(dat)-1)) vars <- colnames(dat)[-1]

  ## use decay
  # http://stats.stackexchange.com/a/70146/61693
  form <- paste(rvar, "~ . ")
  nninput <- list(formula = as.formula(form),
              rang = .1, size = size, decay = decay, weights = wts, maxit = 10000,
              linout = linout, entropy = entropy, skip = skip, trace = FALSE, data = dat)

  ## need do.call so Garson/Olden plot will work
  if (!is_not(seed)) set.seed(seed)
  model <- do.call(nnet::nnet, nninput)

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
#' @param calc Calculate mean and sd or use available attributes
#'
#' @return Scaled data frame
#'
#' @export
scaledf <- function(dat, center = TRUE, scale = TRUE, sf = 2, wts = NULL, calc = TRUE) {
  isNum <- sapply(dat, is.numeric)
  if (sum(isNum) == 0) return(dat)
  cn <- names(isNum)[isNum]

  if (calc) {
    if (length(wts) == 0) {
      ms <- summarise_each_(dat, funs(mean(., na.rm = TRUE)), vars = cn)
      if (scale)
        sds <- summarise_each_(dat, funs(sd(., na.rm = TRUE)), vars = cn)
    } else {
      ms <- summarise_each_(dat, funs(weighted.mean(., wts, na.rm = TRUE)), vars = cn)
      if (scale)
        sds <- summarise_each_(dat, funs(weighted.sd(., wts, na.rm = TRUE)), vars = cn)
    }
  } else {
    ms <- attr(dat,"ms")
    sds <- attr(dat,"sds")
    if (is.null(ms) && is.null(sds)) return(dat)
  }
  if (center && scale) {
    mutate_each_(dat, funs((. - ms$.) / (sf * sds$.)), vars = cn) %>%
      set_attr("ms", ms) %>%
      set_attr("sds", sds)
  } else if (center) {
    mutate_each_(dat, funs(. - ms$.), vars = cn) %>%
      set_attr("ms", ms)
  } else if (scale) {
    mutate_each_(dat, funs(. / (sf * sds$.)), vars = cn) %>%
      set_attr("sds", sds)
  } else {
    dat
  }
}

#' Summary method for the ann function
#'
#' @details See \url{http://radiant-rstats.github.io/docs/model/ann.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{ann}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- ann("titanic", "survived", "pclass", lev = "Yes")
#' summary(result)
#'
#' @seealso \code{\link{ann}} to generate esults
#' @seealso \code{\link{plot.ann}} to plot results
#' @seealso \code{\link{predict.ann}} for prediction
#'
#' @export
summary.ann <- function(object, ...) {

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

  if (!is_empty(object$wts, "None") && class(object$wts) == "integer")
    cat("Nr obs               :", formatnr(sum(object$wts), dec = 0), "\n\n")
  else
    cat("Nr obs               :", formatnr(length(object$rv), dec = 0), "\n\n")

  print(object$model)

  if (object$model$convergence != 0)
    cat("\n\nThe model did not converge.")
}

#' Plot method for the ann function
#'
#' @details See \url{http://radiant-rstats.github.io/docs/model/ann.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{ann}}
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- ann("titanic", "survived", c("pclass","sex"), lev = "Yes")
#' plot(result, plots = c("imp","net"))
#'
#' @seealso \code{\link{ann}} to generate results
#' @seealso \code{\link{summary.ann}} to summarize results
#' @seealso \code{\link{predict.ann}} for prediction
#'
#' @importFrom NeuralNetTools plotnet olden
#'
#' @export
plot.ann <- function(x, shiny = FALSE, ...) {

  object <- x; rm(x)
  if (is.character(object)) return(object)
  plot_list <- list()
  plot_list[[1]] <- NeuralNetTools::olden(object$model) + coord_flip()
  # plot_list[[2]] <- NeuralNetTools::garson(object$model) + coord_flip()
  nrCol <- 1

  if (length(plot_list) > 0) {
    sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = nrCol))) ) %>%
      { if (shiny) . else print(.) }
  }
}

#' Predict method for the ann function
#'
#' @details See \url{http://radiant-rstats.github.io/docs/model/ann.html} for an example in Radiant
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
#' result <- logistic("titanic", "survived", c("pclass","sex"), lev = "Yes")
#'  predict(result, pred_cmd = "pclass = levels(pclass)")
#' logistic("titanic", "survived", c("pclass","sex"), lev = "Yes") %>%
#'   predict(pred_cmd = "sex = c('male','female')")
#' logistic("titanic", "survived", c("pclass","sex"), lev = "Yes") %>%
#'  predict(pred_data = "titanic")
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

  pfun <- function(model, pred, se, conf_lev) {
    pred_val <- try(sshhr(predict(object$model, pred)), silent = TRUE)

    if (!is(pred_val, 'try-error'))
      pred_val %<>% as.data.frame %>% select(1) %>% set_colnames("Prediction")

    pred_val
  }

  predict.model(object, pfun, "ann.predict", pred_data, pred_cmd, conf_lev, se, dec)
}

#' Print method for predict.ann
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#'
#' @export
print.ann.predict <- function(x, ..., n = 10)
  print.model.predict(x, ..., n = n, header = "Artificial Neural Network (ANN)")

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


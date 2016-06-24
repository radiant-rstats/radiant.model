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
#' @param check Optional output or estimation parameters. "vif" to show the multicollinearity diagnostics. "confint" to show coefficient confidence interval estimates. "odds" to show odds ratios and confidence interval estimates. "standardize" to output standardized coefficient estimates. "stepwise" to apply step-wise selection of variables
#' @param dec Number of decimals to show
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
                check = "",
                dec = 3,
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

  # dat <- getdata(dataset, c(rvar, evar), filt = data_filter)
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

    linout <- FALSE; entropy = TRUE
    if (lev == "") {
      if (is.factor(rv))
        lev <- levels(rv)[1]
      else
        lev <- rv %>% as.character %>% as.factor %>% levels %>% .[1]
    }

    ## transformation to TRUE/FALSE depending on the selected level (lev)
    dat[[rvar]] <- dat[[rvar]] == lev
  } else {
    linout = TRUE; entropy = FALSE
  }

  ## stability issues ...
  # http://stats.stackexchange.com/questions/23235/how-do-i-improve-my-neural-network-stability
  # isNum <- sapply(dat, is.numeric)
  # if (type == "regression") isNum <- 0 ## not standardizing data for (linear) regression
  # if (sum(isNum) > 0)
    # dat[,isNum] <- select(dat, which(isNum)) %>% dfscale(., wts = wts)

  vars <- evar
  if (length(vars) < (ncol(dat)-1)) vars <- colnames(dat)[-1]

  ## use decay
  # http://stats.stackexchange.com/a/70146/61693

  form <- paste(rvar, "~ . ")
  nninput <- list(formula = as.formula(form),
              rang = .1, size = size, decay = decay, weights = wts, maxit = 10000,
              linout = linout, entropy = entropy, trace = FALSE, data = dat)

  ## need do.call so Garson plot will work
  if (!is.null(seed) && !is.na(seed)) set.seed(seed)
  model <- do.call(nnet::nnet, nninput)

  ## ann return residuals as a matrix
  model$residuals <- model$residuals[,1]

  ## ann model object does not include the data by default
  model$model <- dat

  rm(dat) ## dat not needed elsewhere

  environment() %>% as.list %>% add_class(c("ann","model"))
}

dfscale <- function(dat, center = TRUE, scale = TRUE, wts = NULL, calc = TRUE) {
  if (calc) {
    if (length(wts) > 0) {
      ms <- summarise_each(dat, funs(weighted.mean(., wts, na.rm = TRUE)))
      sds <- summarise_each(dat, funs(weighted.sd(., wts, na.rm = TRUE)))
    } else {
      ms <- summarise_each(dat, funs(mean(., na.rm = TRUE)))
      sds <- summarise_each(dat, funs(sd(., na.rm = TRUE)))
    }
  } else {
    ms <- attr(dat,"ms")
    sds <- attr(dat,"sds")
  }
  if (center) {
    dat <- mutate_each(dat, funs(. - ms$.))
    attr(dat,"ms") <- ms
  }
  if (scale) {
    dat <- mutate_each(dat, funs(2*sds$.))
    attr(dat,"sds") <- sds
  }
  dat
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
  # print(caret::varImp(object$model))

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
                        ...) {

  pfun <- function(model, pred, se, conf_lev) {
    pred_val <- try(sshhr(predict(object$model, pred)), silent = TRUE)

    if (!is(pred_val, 'try-error'))
      pred_val %<>% as.data.frame %>% select(1) %>% set_colnames("Prediction")

    pred_val
  }

  predict.model(object, pfun, "ann.predict", pred_data, pred_cmd, conf_lev, se)
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


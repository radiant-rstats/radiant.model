#' Naive Bayes using e1071::naiveBayes
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nb.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param rvar The response variable in the logit (probit) model
#' @param evar Explanatory variables in the model
#' @param laplace	Positive double controlling Laplace smoothing. The default (0) disables Laplace smoothing.
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list with all variables defined in nb as an object of class nb
#'
#' @examples
#' result <- nb("titanic", "survived", c("pclass","sex","age"))
#'
#' @seealso \code{\link{summary.nb}} to summarize results
#' @seealso \code{\link{plot.nb}} to plot results
#' @seealso \code{\link{predict.nb}} for prediction
#'
#' @importFrom e1071 naiveBayes
#'
#' @export
nb <- function(dataset, rvar, evar, laplace = 0, data_filter = "") {

  if (rvar %in% evar)
    return("Response variable contained in the set of explanatory variables.\nPlease update model specification." %>%
           add_class("nb"))

  dat <- getdata(dataset, c(rvar, evar), filt = data_filter)
  if (!is_string(dataset)) dataset <- deparse(substitute(dataset)) %>% set_attr("df", TRUE)

  if (any(summarise_all(dat, funs(does_vary)) == FALSE))
    return("One or more selected variables show no variation. Please select other variables." %>% add_class("nb"))

  vars <- evar
  ## in case : is used
  if (length(vars) < (ncol(dat)-1)) 
    vars <- evar <- colnames(dat)[-1]

  ## make sure the dv is a factor
  if (!is.factor(dat[[1]])) dat <- as_factor(dat[[1]])
  lev <- levels(dat[[1]])

  ## estimate using e1071
  form <- paste0(rvar, " ~ ", paste0(evar, collapse = "+")) %>% as.formula
  model <- e1071::naiveBayes(dat[ ,-1, drop = FALSE], dat[[1]] , laplace = laplace)

  ## nb does not return residuals
  model$residuals <- NA

  ## nb doesn't indlude model terms, needed for predict_model
  # model$terms <- colnames(dat)
  # attr(model$term, "dataClasses") <- getclass(dat)

  ## nb model object does not include the data by default
  model$model <- dat
  rm(dat) ## dat not needed elsewhere

  as.list(environment()) %>% add_class(c("nb","model"))
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
#' result <- nb("titanic", "survived", c("pclass","sex","age"))
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
  cat("\nData                 :", object$dataset)
  if (object$data_filter %>% gsub("\\s","",.) != "")
    cat("\nFilter               :", gsub("\\n","", object$data_filter))
  cat("\nResponse variable    :", object$rvar)
  cat("\nLevels               :", paste0(object$lev, collapse = ", "), "in", object$rvar)
  cat("\nExplanatory variables:", paste0(object$evar, collapse = ", "))
  cat("\nLaplace              :", object$laplace)
  cat("\nNr obs               :", formatnr(nrow(object$model$model), dec = 0), "\n")

  cat("\nA-priori probabilities:\n")
  apriori <- object$model$apriori %>% {. / sum(.)}
  names(dimnames(apriori))[1] <- object$rvar
  print(round(apriori, 3))

  cat("\nConditional probabilities (categorical) or means & st.dev (numeric):\n")
  for (i in object$model$tables) {
    names(dimnames(i))[1] <- object$rvar
    if (is.null(dimnames(i)[2][[1]])) dimnames(i)[2][[1]] <- c("mean","st.dev")
    print(round(i, dec)); cat("\n")
  }
}

#' Plot method for the nb function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nb.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{nb}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- nb("titanic", "survived", c("pclass","sex"))
#' plot(result)
#' result <- nb("titanic", "pclass", c("sex","age"))
#' plot(result)
#'
#' @seealso \code{\link{nb}} to generate results
#' @seealso \code{\link{summary.nb}} to summarize results
#' @seealso \code{\link{predict.nb}} for prediction
#'
#' @export
plot.nb <- function(x, ...) {

  object <- x; rm(x)
  if (is.character(object)) return(object)

  # x <- mutate_all(object$model$model[,-1, drop = FALSE], funs(as_numeric))
  x <- mutate_all(select(object$model$model, -1), funs(as_numeric))
  y <- object$model$model[[1]]
  k <- length(object$lev)

  if (k == 2) {
    ## with two variables one of them would be set to 0 by caret::varImp
    # apply(x, 2, auc, y) %>% {. - min(.)} %>% {. / max(.)} %>% cbind(.,.) %>% set_colnames(levs)
    ## reporting auc for each variable
    # vimp <- apply(x, 2, auc, y) %>% round(., 3) %>% cbind(.,.) %>% set_colnames(levs)
    vimp <- data.frame(auc = apply(x, 2, auc, y), vars = names(x)) %>% arrange_(.dots = "auc")
    vimp$vars <- factor(vimp$vars, levels = vimp$vars)
    p <- visualize(vimp, yvar = "auc", xvar = "vars", type = "bar", custom = TRUE) +
      xlab("") +
      ylab("Variable Importance (AUC)") +
      coord_flip(ylim = c(0.5, max(vimp$auc))) +
      theme(axis.text.y = element_text(hjust = 0))
  } else {

    cmb <- combn(object$lev, 2)
    vimp <- matrix(NA, ncol(cmb), ncol(x))

    for (i in 1:ncol(cmb)) {
      ind <- y %in% cmb[,i]
      vimp[i,] <- apply(x[ind,,drop = FALSE], 2, auc, droplevels(y[ind]))
    }
    vimp <- as.data.frame(vimp)
    colnames(vimp) <- names(x)
    vimp$Predict <- apply(cmb, 2, paste0, collapse = " vs ")
    vimp$Predict <- factor(vimp$Predict, levels = unique(rev(vimp$Predict)))
    vimp <- gather_(vimp, "vars", "auc", names(x), factor_key = TRUE)

    p <- visualize(vimp, yvar = "auc", xvar = "Predict", type = "bar", fill = "vars", custom = TRUE) +
      guides(fill = guide_legend(title = "X-vars")) +
      xlab("") +
      ylab("Variable Importance (AUC)") + 
      # scale_x_discrete(limits = rev(vimp$Predict)) +
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
#' @param pred_data Provide the name of a dataframe to generate predictions (e.g., "titanic"). The dataset must contain all columns used in the estimation
#' @param pred_cmd Generate predictions using a command. For example, `pclass = levels(pclass)` would produce predictions for the different levels of factor `pclass`. To add another variable use a `,` (e.g., `pclass = levels(pclass), age = seq(0,100,20)`)
#' @param pred_names Names for the predictions to be stored. If one name is provided, only the first column of predictions is stored. If empty, the level in the response variable of the nb model will be used
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- nb("titanic", "survived", c("pclass","sex","age"))
#' predict(result, pred_data = "titanic")
#' predict(result, pred_data = "titanic", pred_names = c("Yes","No"))
#' predict(result, pred_cmd = "pclass = levels(pclass)")
#' result <- nb("titanic", "pclass", c("survived","sex","age"))
#' predict(result, pred_data = "titanic")
#' predict(result, pred_data = "titanic", pred_names = c("1st","2nd","3rd"))
#' predict(result, pred_data = "titanic", pred_names = "")
#' predict(result, pred_data = "titanic", pred_names = NA)
#'
#' @seealso \code{\link{nb}} to generate the result
#' @seealso \code{\link{summary.nb}} to summarize results
#'
#' @export
predict.nb <- function(object,
                       pred_data = "",
                       pred_cmd = "",
                       pred_names = "",
                       dec = 3,
                       ...) {

  if (is.character(object)) return(object)
  pfun <- function(model, pred, se, conf_lev) {

    ## need to make sure levels in original data and pred are the same
    ## as predict.naiveBayes relies on this ordering
    set_levels <- function(name) {
      if (!is.null(model$model[[name]]) && is.factor(model$model[[name]])) {
        levs <- levels(model$model[[name]])
        levs_pred <- levels(pred[[name]])
        if (is.null(levs_pred) || !all(levs == levs_pred))
          pred[[name]] <<- factor(pred[[name]], levels = levs)
      }
    }

    fix <- sapply(colnames(pred), set_levels)
    pred_val <- try(sshhr(predict(model, pred, type = "raw")), silent = TRUE)

    if (!is(pred_val, 'try-error')) {
      pred_val %<>% as.data.frame
      if (all(is_empty(pred_names))) pred_names <- colnames(pred_val)
      pred_val %<>% select(1:min(ncol(pred_val),length(pred_names))) %>%
        set_colnames(pred_names)
    }

    pred_val
  }

  # radiant.model:::predict_model(object, pfun, "nb.predict", pred_data, pred_cmd, conf_lev, se, dec)
  predict_model(object, pfun, "nb.predict", pred_data, pred_cmd, dec = dec)
}

#' Print method for predict.nb
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#'
#' @export
print.nb.predict <- function(x, ..., n = 10) {
  print_predict_model(x, ..., n = n, header = "Naive Bayes Classifier", lev = attr(x, "lev"))
}

#' Plot method for nb.predict function
#'
#' @param x Return value from predict function predict.nb
#' @param xvar Variable to display along the X-axis of the plot
#' @param facet_row Create vertically arranged subplots for each level of the selected factor variable
#' @param facet_col Create horizontally arranged subplots for each level of the selected factor variable
#' @param color Adds color to a scatter plot to generate a heat map. For a line plot one line is created for each group and each is assigned a different colour
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- nb("titanic", "survived", c("pclass","sex","age"))
#' pred <- predict(result, pred_cmd="pclass=levels(pclass), sex=levels(sex), age=seq(0,100,20)")
#' plot(pred, xvar = "age", facet_col = "sex", facet_row = "pclass")
#' pred <- predict(result, pred_data="titanic")
#' plot(pred, xvar = "age", facet_col = "sex")
#'
#' @seealso \code{\link{predict.nb}} to generate predictions
#'
#' @export
plot.nb.predict <- function(x, xvar = "",
                            facet_row = ".",
                            facet_col = ".",
                            color = ".class",
                            ...) {

  ## should work with req in regress_ui but doesn't
  if (is_empty(xvar)) return(invisible())

  if (facet_col != "." && facet_row == facet_col)
    return("The same variable cannot be used for both Facet row and Facet column")

  object <- x; rm(x)
  if (is.character(object)) return(object)

  pvars <- setdiff(attr(object, "vars"), attr(object, "evar"))
  rvar <- attr(object, "rvar")
  object %<>% gather_(".class", "Prediction", gather_cols = pvars)

  byvar <- c(xvar, color)
  if (facet_row != ".") byvar <- unique(c(byvar, facet_row))
  if (facet_col != ".") byvar <- unique(c(byvar, facet_col))

  tmp <- object %>% group_by_(.dots = byvar) %>% select_(.dots = c(byvar, "Prediction")) %>% summarise_all(funs(mean))
  p <- ggplot(tmp, aes_string(x=xvar, y="Prediction", color = color, group = color)) + geom_line()

  if (facet_row != "." || facet_col != ".") {
    facets <- ifelse (facet_row == ".", paste("~", facet_col), paste(facet_row, '~', facet_col))
    facet_fun <- ifelse (facet_row == ".", facet_wrap, facet_grid)
    p <- p + facet_fun(as.formula(facets))
  }

  p <- p + guides(color = guide_legend(title = rvar)) 

  sshhr(p)
}

#' Store predicted values generated in the nb function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nb.html} for an example in Radiant
#'
#' @param object Return value from model function
#' @param ... Additional arguments
#' @param data Data or dataset name (e.g., data = mtcars or data = "mtcars")
#' @param name Variable name(s) assigned to predicted values. If empty, the levels of the response variable will be used
#'
#' @examples
#' result <- nb("titanic", "survived", c("pclass","sex","age"))
#' pred <- predict(result, pred_data = "titanic")
#' store(pred, data = titanic, name = "pred") %>% head
#' store(pred, data = titanic) %>% head
#'
#' @export
store.nb.predict <- function(object, ..., data = attr(object,"pred_data"), name = "") {

  ## extract the names of the variables predicted
  pvars <- setdiff(attr(object, "vars"), attr(object, "evar"))

  ## as.vector removes all attributes from df
  df <- as.vector(object[,pvars])

  if (is_empty(name)) {
    name <- pvars
  } else {
    ## gsub needed because trailing/leading spaces may be added to the variable name
    name <- unlist(strsplit(name, ",")) %>% gsub("\\s","",.)
    if (length(name) < length(pvars))
      df <- df[,1:length(name), drop = FALSE] %>% set_colnames(name)
  }

  indr <- indexr(data, attr(object, "evar"), "", cmd = attr(object, "pred_cmd"))
  # indr <- indexr(data, vars, "", cmd = attr(object, "pred_cmd"))

  pred <- as_data_frame(matrix(NA, nrow = indr$nr, ncol = ncol(df)))
  pred[indr$ind, ] <- df

  changedata(data, vars = pred, var_names = name)
}

#' Gradient Boosted Trees using XGBoost
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/gbt.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param rvar The response variable in the model
#' @param evar Explanatory variables in the model
#' @param type Model type (i.e., "classification" or "regression")
#' @param lev Level to use as the first column in prediction output
#' @param max_depth Maximum 'depth' of tree
#' @param learning_rate Learning rate (eta)
#' @param min_split_loss Minimal improvement (gamma)
#' @param nrounds Number of trees to create
#' @param min_child_weight Minimum number of instances allowed in each node
#' @param subsample Subsample ratio of the training instances (0-1)
#' @param early_stopping_rounds Early stopping rule
#' @param nthread Number of parallel threads to use. Defaults to 12 if available
#' @param wts Weights to use in estimation
#' @param seed Random seed to use as the starting point
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param envir Environment to extract data from
#' @param ... Further arguments to pass to xgboost
#'
#' @return A list with all variables defined in gbt as an object of class gbt
#'
#' @examples
#' gbt(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>% summary()
#' gbt(titanic, "survived", c("pclass", "sex")) %>% str()
#' gbt(titanic, "survived", c("pclass", "sex"), eval_metric = paste0("error@", 0.5/6)) %>% str()
#' gbt(diamonds, "price", c("carat", "clarity"), type = "regression") %>% summary()
#' rig_wrap <- function(preds, dtrain) {
#'   labels <- xgboost::getinfo(dtrain, "label")
#'   value <- rig(preds, labels, lev = 1)
#'   list(metric = "rig", value = value)
#' }
#' gbt(titanic, "survived", c("pclass", "sex"), eval_metric = rig_wrap, maximize = TRUE) %>% str()
#'
#' @seealso \code{\link{summary.gbt}} to summarize results
#' @seealso \code{\link{plot.gbt}} to plot results
#' @seealso \code{\link{predict.gbt}} for prediction
#'
#' @importFrom xgboost xgboost xgb.importance
#' @importFrom lubridate is.Date
#'
#' @export
gbt <- function(
  dataset, rvar, evar, type = "classification", lev = "",
  max_depth = 6, learning_rate = 0.3, min_split_loss = 0,
  min_child_weight = 1, subsample = 1,
  nrounds = 100, early_stopping_rounds = 10,
  nthread = 12, wts = "None", seed = NA,
  data_filter = "", envir = parent.frame(), ...
) {

  if (rvar %in% evar) {
    return("Response variable contained in the set of explanatory variables.\nPlease update model specification." %>%
      add_class("gbt"))
  }

  vars <- c(rvar, evar)

  if (is_empty(wts, "None")) {
    wts <- NULL
  } else if (is_string(wts)) {
    wtsname <- wts
    vars <- c(rvar, evar, wtsname)
  }

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, vars, filt = data_filter, envir = envir) %>%
    mutate_if(is.Date, as.numeric)
  nr_obs <- nrow(dataset)

  if (!is_empty(wts, "None")) {
    if (exists("wtsname")) {
      wts <- dataset[[wtsname]]
      dataset <- select_at(dataset, .vars = base::setdiff(colnames(dataset), wtsname))
    }
    if (length(wts) != nrow(dataset)) {
      return(
        paste0("Length of the weights variable is not equal to the number of rows in the dataset (", format_nr(length(wts), dec = 0), " vs ", format_nr(nrow(dataset), dec = 0), ")") %>%
          add_class("gbt")
      )
    }
  }

  not_vary <- colnames(dataset)[summarise_all(dataset, does_vary) == FALSE]
  if (length(not_vary) > 0) {
    return(paste0("The following variable(s) show no variation. Please select other variables.\n\n** ", paste0(not_vary, collapse = ", "), " **") %>%
      add_class("gbt"))
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
  }

  vars <- evar
  ## in case : is used
  if (length(vars) < (ncol(dataset) - 1)) {
    vars <- evar <- colnames(dataset)[-1]
  }

  gbt_input <- list(
    max_depth = max_depth,
    learning_rate = learning_rate,
    min_split_loss = min_split_loss,
    nrounds = nrounds,
    min_child_weight = min_child_weight,
    subsample = subsample,
    early_stopping_rounds = early_stopping_rounds,
    nthread = nthread
  )

  ## checking for extra args
  extra_args <- list(...)
  extra_args_names <- names(extra_args)
  check_args <- function(arg, default, inp = gbt_input) {
    if (!arg %in% extra_args_names) inp[[arg]] <- default
    inp
  }

  if (type == "classification") {
    gbt_input <- check_args("objective", "binary:logistic")
    gbt_input <- check_args("eval_metric", "auc")
    dty <- as.integer(dataset[[rvar]] == lev)
  } else {
    gbt_input <- check_args("objective", "reg:linear")
    gbt_input <- check_args("eval_metric", "rmse")
    dty <- dataset[[rvar]]
  }

  ## adding data
  dtx <- onehot(dataset[, -1, drop = FALSE])[, -1, drop = FALSE]
  gbt_input <- c(gbt_input, list(data = dtx, label = dty), ...)

  ## based on http://stackoverflow.com/a/14324316/1974918
  seed <- gsub("[^0-9]", "", seed)
  if (!is_empty(seed)) {
    if (exists(".Random.seed")) {
      gseed <- .Random.seed
      on.exit(.Random.seed <<- gseed)
    }
    set.seed(seed)
  }

  ## capturing the iteration history
  output <- capture.output(model <<- do.call(xgboost::xgboost, gbt_input))

  ## adding residuals for regression models
  if (type == "regression") {
    model$residuals <- dataset[[rvar]] - predict(model, dtx)
  } else {
    model$residuals <- NULL
  }

  ## adding feature importance information
  model$importance <- xgboost::xgb.importance(model = model)

  ## gbt model object does not include the data by default
  model$model <- dataset

  rm(dataset, dty, dtx, rv, envir) ## dataset not needed elsewhere
  gbt_input$data <- gbt_input$label <- NULL

  ## needed to work with prediction functions
  check <- ""    

  as.list(environment()) %>% add_class(c("gbt", "model"))
}

#' Summary method for the gbt function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/gbt.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{gbt}}
#' @param prn Print iteration history
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- gbt(titanic, "survived", "pclass", lev = "Yes")
#' summary(result)
#'
#' @seealso \code{\link{gbt}} to generate results
#' @seealso \code{\link{plot.gbt}} to plot results
#' @seealso \code{\link{predict.gbt}} for prediction
#'
#' @export
summary.gbt <- function(object, prn = TRUE, ...) {

  if (is.character(object)) return(object)
  cat("Gradient Boosted Trees (XGBoost)\n")
  if (object$type == "classification") {
    cat("Type                 : Classification")
  } else {
    cat("Type                 : Regression")
  }
  cat("\nData                 :", object$df_name)
  if (!is_empty(object$data_filter)) {
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
  cat("Max depth            :", object$max_depth, "\n")
  cat("Learning rate (eta)  :", object$learning_rate, "\n")
  cat("Min split loss       :", object$min_split_loss, "\n")
  cat("Min child weight     :", object$min_child_weight, "\n")
  cat("Sub-sample           :", object$subsample, "\n")
  cat("Nr of rounds (trees) :", object$nrounds, "\n")
  cat("Early stopping rounds:", object$early_stopping_rounds, "\n")
  if (length(object$extra_args)) {
    extra_args <- deparse(object$extra_args) %>%
      sub("list\\(", "", .) %>%
      sub("\\)$", "", .) %>%
      sub(" {2,}", " ", .)
    cat("Additional arguments :", extra_args, "\n")
  }
  if (!is_empty(object$seed)) {
    cat("Seed                 :", object$seed, "\n")
  }

  if (!is_empty(object$wts, "None") && (length(unique(object$wts)) > 2 || min(object$wts) >= 1)) {
    cat("Nr obs               :", format_nr(sum(object$wts), dec = 0), "\n")
  } else {
    cat("Nr obs               :", format_nr(object$nr_obs, dec = 0), "\n")
  }

  if (prn == TRUE) {
    cat("\nIteration history:\n\n")
    ih <- object$output[c(-2, -3)]
    if (length(ih) > 20) ih <- c(head(ih, 10), "...", tail(ih, 10))
    cat(paste0(ih, collapse = "\n"))
  }
}

#' Plot method for the gbt function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/gbt.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{gbt}}
#' @param shiny Did the function call originate inside a shiny app
#' @param plots Plots to produce for the specified Gradient Boosted Tree model. Use "" to avoid showing any plots (default). Options are ...
#' @param nrobs Number of data points to show in scatter plots (-1 for all)
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- gbt(titanic, "survived", c("pclass", "sex"), lev = "Yes")
#'
#' @seealso \code{\link{gbt}} to generate results
#' @seealso \code{\link{summary.gbt}} to summarize results
#' @seealso \code{\link{predict.gbt}} for prediction
#'
#' @importFrom pdp partial
#'
#' @export
plot.gbt <- function(
  x, plots = "", nrobs = Inf,
  shiny = FALSE, custom = FALSE, ...
) {

  if (is.character(x) || !inherits(x$model, "xgb.Booster")) return(x)
  plot_list <- list()
  ncol <- 1

  if (x$type == "regression" && "dashboard" %in% plots) {
    plot_list <- plot.regress(x, plots = "dashboard", lines = "line", nrobs = nrobs, custom = TRUE)
    ncol <- 2
  } else if ("pdp" %in% plots) {
    ncol <- 2
    mod_dat <- x$model$model[, -1, drop = FALSE]
    dtx <- onehot(mod_dat)[, -1, drop = FALSE]
    for (pn in colnames(mod_dat)) {
      if (is.factor(mod_dat[[pn]])) {
        fn <- paste0(pn, levels(mod_dat[[pn]]))[-1]
        effects <- rep(NA, length(fn))
        nr <- length(fn)
        for (i in seq_len(nr)) {
          seed <- x$seed
          pdi <-  pdp::partial(
            x$model, pred.var = fn[i], plot = FALSE,
            prob = x$type == "classification", train = dtx
          )
          effects[i] <- pdi[pdi[[1]] == 1, 2]
        }
        pgrid <- as.data.frame(matrix(0, ncol = nr))
        colnames(pgrid) <- fn
        base <-  pdp::partial(
          x$model, pred.var = fn,
          pred.grid = pgrid, plot = FALSE,
          prob = x$type == "classification", train = dtx
        )[1, "yhat"]
        pd <- data.frame(label = levels(mod_dat[[pn]]), yhat = c(base, effects)) %>%
          mutate(label = factor(label, levels = label))
        colnames(pd)[1] <- pn
        plot_list[[pn]] <- ggplot(pd, aes_string(x = pn, y = "yhat")) +
          geom_point() +
          labs(y = "")
      } else {
        plot_list[[pn]] <- pdp::partial(
          x$model, pred.var = pn, plot = TRUE, rug = TRUE,
          prob = x$type == "classification", plot.engine = "ggplot2",
          smooth = TRUE, train = dtx
        ) + labs(y = "")
      }
    }
  } else if ("vimp" %in% plots) {
    imp <- x$model$importance
    mod_dat <- x$model$model
    vimp <- data.frame(
      Feature = colnames(mod_dat)[-1],
      Gain = 0
    )
    for (i in colnames(mod_dat)[-1]) {
      if (is.factor(mod_dat[[i]])) {
        fn <- paste0(i, levels(mod_dat[[i]]))
        if (any(fn %in% imp$Feature)) {
          vimp[vimp$Feature == i, "Gain"] <- sum(imp[imp$Feature %in% fn, "Gain"])
        }
      } else {
        if (i %in% imp$Feature) {
          vimp[vimp$Feature == i, "Gain"] <- imp[imp$Feature == i, "Gain"]
        }
      }
    }

    ncol <- 1
    vimp <- vimp %>%
      arrange_at("Gain") %>%
      mutate(Feature = factor(Feature, levels = Feature))
    plot_list[["vimp"]] <- visualize(vimp, yvar = "Gain", xvar = "Feature", type = "bar", custom = TRUE) +
      guides(fill = guide_legend(title = "")) +
      labs(x = "", y = "Variable Importance (Gain)") +
      coord_flip() +
      theme(axis.text.y = element_text(hjust = 0))
  }

  if (length(plot_list) > 0) {
    if (custom) {
      if (length(plot_list) == 1) plot_list[[1]] else plot_list
    } else {
      patchwork::wrap_plots(plot_list, ncol = ncol) %>%
        {if (shiny) . else print(.)}
    }
  }
}

#' Predict method for the gbt function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/gbt.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{gbt}}
#' @param pred_data Provide the dataframe to generate predictions (e.g., diamonds). The dataset must contain all columns used in the estimation
#' @param pred_cmd Generate predictions using a command. For example, `pclass = levels(pclass)` would produce predictions for the different levels of factor `pclass`. To add another variable, create a vector of prediction strings, (e.g., c('pclass = levels(pclass)', 'age = seq(0,100,20)')
#' @param dec Number of decimals to show
#' @param envir Environment to extract data from
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- gbt(titanic, "survived", c("pclass", "sex"), lev = "Yes")
#' predict(result, pred_cmd = "pclass = levels(pclass)")
#' result <- gbt(diamonds, "price", "carat:color", type = "regression")
#' predict(result, pred_cmd = "carat = 1:3")
#' predict(result, pred_data = diamonds) %>% head()
#'
#' @seealso \code{\link{gbt}} to generate the result
#' @seealso \code{\link{summary.gbt}} to summarize results
#'
#' @export
predict.gbt <- function(
  object, pred_data = NULL, pred_cmd = "",
  dec = 3, envir = parent.frame(), ...
) {

  if (is.character(object)) return(object)

  ## ensure you have a name for the prediction dataset
  if (is.data.frame(pred_data)) {
    df_name <- deparse(substitute(pred_data))
  } else {
    df_name <- pred_data
  }

  pfun <- function(model, pred, se, conf_lev) {
    ## ensure the factor levels in the prediction data are the
    ## same as in the data used for estimation
    est_data <- model$model[, -1, drop = FALSE]
    for (i in colnames(pred)) {
      if (is.factor(est_data[[i]])) {
        pred[[i]] <- factor(pred[[i]], levels = levels(est_data[[i]]))
      }
    }
    pred <- onehot(pred[, colnames(est_data), drop = FALSE])[, -1, drop = FALSE]
    ## for testing purposes
    # pred <- model$model[, -1, drop = FALSE]
    pred_val <- try(sshhr(predict(model, pred)), silent = TRUE)
    if (!inherits(pred_val, "try-error")) {
      pred_val %<>% as.data.frame(stringsAsFactors = FALSE) %>%
        select(1) %>%
        set_colnames("Prediction")
    }

    pred_val
  }

  predict_model(object, pfun, "gbt.predict", pred_data, pred_cmd, conf_lev = 0.95, se = FALSE, dec, envir = envir) %>%
    set_attr("radiant_pred_data", df_name)
}

#' Print method for predict.gbt
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#'
#' @export
print.gbt.predict <- function(x, ..., n = 10)
  print_predict_model(x, ..., n = n, header = "Gradiant Boosted Trees")

#' Cross-validation for Gradient Boosted Trees
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/gbt.html} for an example in Radiant
#'
#' @param object Object of type "gbt" or "ranger"
#' @param K Number of cross validation passes to use (aka nfold)
#' @param repeats Repeated cross validation
#' @param params List of parameters (see XGBoost documentation)
#' @param nrounds Number of trees to create
#' @param early_stopping_rounds Early stopping rule
#' @param nthread Number of parallel threads to use. Defaults to 12 if available
#' @param train An optional xgb.DMatrix object containing the original training data. Not needed when using Radiant's gbt function
#' @param type Model type ("classification" or "regression")
#' @param trace Print progress
#' @param seed Random seed to use as the starting point
#' @param maximize When a custom function is used, xgb.cv requires the user indicate if the function output should be maximized (TRUE) or minimized (FALSE)
#' @param fun Function to use for model evaluation (i.e., auc for classification and RMSE for regression)
#' @param ... Additional arguments to be passed to 'fun'
#'
#' @return A data.frame sorted by the mean of the performance metric
#'
#' @seealso \code{\link{gbt}} to generate an initial model that can be passed to cv.gbt
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
#' result <- gbt(dvd, "buy", c("coupon", "purch", "last"))
#' cv.gbt(result, params = list(max_depth = 1:6))
#' cv.gbt(result, params = list(max_depth = 1:6), fun = "logloss")
#' cv.gbt(
#'   result,
#'   params = list(learning_rate = seq(0.1, 1.0, 0.1)),
#'   maximize = TRUE, fun = profit, cost = 1, margin = 5
#' )
#' result <- gbt(diamonds, "price", c("carat", "color", "clarity"), type = "regression")
#' cv.gbt(result, params = list(max_depth = 1:2, min_child_weight = 1:2))
#' cv.gbt(result, params = list(learning_rate = seq(0.1, 0.5, 0.1)), fun = Rsq, maximize = TRUE)
#' cv.gbt(result, params = list(learning_rate = seq(0.1, 0.5, 0.1)), fun = MAE, maximize = FALSE)
#' rig_wrap <- function(preds, dtrain) {
#'   labels <- xgboost::getinfo(dtrain, "label")
#'   value <- rig(preds, labels, lev = 1)
#'   list(metric = "rig", value = value)
#' }
#' result <- gbt(titanic, "survived", c("pclass", "sex"), eval_metric = rig_wrap, maximize = TRUE)
#' cv.gbt(result, params = list(learning_rate = seq(0.1, 0.5, 0.1)))
#' }
#'
#' @export
cv.gbt <- function(
  object, K = 5, repeats = 1, params = list(),
  nrounds = 500, early_stopping_rounds = 10, nthread = 12,
  train = NULL, type = "classification",
  trace = TRUE, seed = 1234, maximize = NULL, fun, ...
) {

  if (inherits(object, "gbt")) {
    dv <- object$rvar
    dataset <- object$model$model
    dtx <- onehot(dataset[, -1, drop = FALSE])[, -1, drop = FALSE]
    type <- object$type
    if (type == "classification") {
      objective <- "binary:logistic"
      dty <- as.integer(dataset[[dv]] == object$lev)
    } else {
      objective <- "reg:linear"
      dty <- dataset[[dv]]
    }
    train <- xgboost::xgb.DMatrix(data = dtx, label = dty)
    params_base <- object$model$params
    if (is_empty(params_base[["eval_metric"]])) {
      params_base[["eval_metric"]] <- object$extra_args[["eval_metric"]]
    }
    if (is_empty(params_base[["maximize"]])) {
      params_base[["maximize"]] <- object$extra_args[["maximize"]]
    }
  } else  if (!inherits(object, "xgb.Booster")) {
    stop("The model object does not seems to be a Gradient Boosted Tree")
  } else {
    if (!inherits(train, "xgb.DMatrix")) {
      train <- eval(object$call[["data"]])
    }
    params_base <- object$params
  }
  if (!inherits(train, "xgb.DMatrix")) {
    stop("Could not access data. Please use the 'train' argument to pass along a matrix created using xgboost::xgb.DMatrix")
  }

  params_base[c("nrounds", "nthread", "silent")] <- NULL
  for (n in names(params)) {
    params_base[[n]] <- params[[n]]
  }
  params <- params_base
  if (is_empty(maximize)) {
    maximize <- params$maximize
  }

  if (missing(fun)) {
    if (type == "classification") {
      if (length(params$eval_metric) == 0) {
        fun <- params$eval_metric <- "auc"
      } else if (is.character(params$eval_metric)) {
        fun <- params$eval_metric
      } else {
        fun <- list("custom" = params$eval_metric)
      }
    } else {
      if (length(params$eval_metric) == 0) {
        fun <- params$eval_metric <- "rmse"
      } else if (is.character(params$eval_metric)) {
        fun <- params$eval_metric
      } else {
        fun <- list("custom" = params$eval_metric)
      } 
    }
  } 
  
  if (length(shiny::getDefaultReactiveDomain()) > 0) {
    trace <- FALSE
    incProgress <- shiny::incProgress
    withProgress <- shiny::withProgress
  } else {
    incProgress <- function(...) {}
    withProgress <- function(...) list(...)[["expr"]]
  }

  ## setting up a customer evaluation function
  if (is.function(fun)) {
    if (missing(...)) {
      if (type == "classification") {
        fun_wrapper <- function(preds, dtrain) {
          labels <- xgboost::getinfo(dtrain, "label")
          value <- fun(preds, labels, 1)
          list(metric = cn, value = value)
        }
      } else {
        fun_wrapper <- function(preds, dtrain) {
          labels <- xgboost::getinfo(dtrain, "label")
          value <- fun(preds, labels)
          list(metric = cn, value = value)
        }
      }
    } else {
      if (type == "classification") {
        fun_wrapper <- function(preds, dtrain) {
          labels <- xgboost::getinfo(dtrain, "label")
          value <- fun(preds, labels, 1, ...)
          list(metric = cn, value = value)
        }
      } else {
        fun_wrapper <- function(preds, dtrain) {
          labels <- xgboost::getinfo(dtrain, "label")
          value <- fun(preds, labels, ...)
          list(metric = cn, value = value)
        }
      }
    }
    cn <- deparse(substitute(fun))
    if (grepl(":{2,3}", cn)) cn <- sub("^.+:{2,3}", "", cn)
    params$eval_metric <- cn
  } else if (is.list(fun)) {
    fun_wrapper <- fun[["custom"]]
    params$eval_metric <- "custom"
  } else {
    fun_wrapper <- params$eval_metric <- fun
  }

  tf <- tempfile()
  tune_grid <- expand.grid(params)
  nitt <- nrow(tune_grid)
  withProgress(message = "Running cross-validation (gbt)", value = 0, {
    out <- list()
    for (i in seq_len(nitt)) {
      cv_params <- tune_grid[i, ]
      if (!is_empty(cv_params$nrounds)) {
        nrounds <- cv_params$nrounds
        cv_params$nrounds <- NULL
      }
      if (trace) {
        cat("Working on", paste0(paste(colnames(cv_params), "=", cv_params), collapse = ", "), "\n")
      }
      for (j in seq_len(repeats)) {
        set.seed(seed)
        sink(tf) ## avoiding messages from xgboost::xgb.cv
        cv_params_tmp <- cv_params
        for (nm in c("eval_metric", "maximize", "early_stopping_rounds", "nthread")) {
          cv_params_tmp[[nm]] <- NULL
        }
        model <- try(xgboost::xgb.cv(
          params = cv_params_tmp,
          data = train,
          nfold = K,
          print_every_n = 500,
          eval_metric = fun_wrapper,
          maximize = maximize,
          early_stopping_rounds = early_stopping_rounds,
          nrounds = nrounds,
          nthread = nthread
        ))
        sink()
        if (inherits(model, "try-error")) {
          stop(model)
        }
        out[[paste0(i, "-", j)]] <- as.data.frame(c(
          nrounds = nrounds, best_iteration = model$best_iteration,
          model$evaluation_log[model$best_iteration, -1], cv_params
        ))
      }
      incProgress(1 / nitt, detail = paste("\nCompleted run", i, "out of", nitt))
    }
  })

  out <- bind_rows(out)
  if (type == "classification") {
    out[order(out[[5]], decreasing = TRUE), ]
  } else {
    out[order(out[[5]], decreasing = FALSE), ]
  }
}

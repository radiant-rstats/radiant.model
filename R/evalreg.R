#' Evaluate the performance of different regression models
#'
#' @details Evaluate different regression models based on predictions. See \url{https://radiant-rstats.github.io/docs/model/evalreg.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param pred Predictions or predictors
#' @param rvar Response variable
#' @param train Use data from training ("Training"), test ("Test"), both ("Both"), or all data ("All") to evaluate model evalreg
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "training == 1")
#'
#' @return A list of results
#'
#' @seealso \code{\link{summary.evalreg}} to summarize results
#' @seealso \code{\link{plot.evalreg}} to plot results
#'
#' @examples
#' data.frame(price = diamonds$price, pred1 = rnorm(3000), pred2 = diamonds$price) %>%
#'   evalreg(pred = c("pred1", "pred2"), "price") %>%
#'   str()
#'
#' @export
evalreg <- function(
  dataset, pred, rvar,
  train = "All", data_filter = ""
) {

  if (!train %in% c("", "All") && is_empty(data_filter)) {
    return("** Filter required. To set a filter go to Data > View and click\n   the filter checkbox **" %>% add_class("evalreg"))
  }

  # Add an option to exponentiate predictions in case of log regression

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dat_list <- list()
  vars <- c(pred, rvar)
  if (train == "Both") {
    dat_list[["Training"]] <- get_data(dataset, vars, filt = data_filter)
    dat_list[["Test"]] <- get_data(dataset, vars, filt = paste0("!(", data_filter, ")"))
  } else if (train == "Training") {
    dat_list[["Training"]] <- get_data(dataset, vars, filt = data_filter)
  } else if (train == "Test" | train == "Validation") {
    dat_list[["Test"]] <- get_data(dataset, vars, filt = paste0("!(", data_filter, ")"))
  } else {
    dat_list[["All"]] <- get_data(dataset, vars, filt = "")
  }

  pdat <- list()
  for (i in names(dat_list)) {
    dat <- dat_list[[i]]
    rv <- dat[[rvar]]

    ## see http://stackoverflow.com/a/35617817/1974918 about extracting a row
    ## from a tbl_df
    pdat[[i]] <- data.frame(
      Type = rep(i, length(pred)),
      Predictor = pred,
      n = nrow(dat[pred]),
      Rsq = cor(rv, select_at(dat, pred))^2 %>% .[1, ],
      RMSE = summarise_at(dat, .vars = pred, .funs = ~ sqrt(mean((rv - .) ^ 2, na.rm = TRUE))) %>% unlist(),
      MAE = summarise_at(dat, .vars = pred, .funs = ~ mean(abs(rv - .), na.rm = TRUE)) %>% unlist(),
      stringsAsFactors = FALSE
    )
  }

  dat <- bind_rows(pdat) %>% as.data.frame(stringsAsFactors = FALSE)
  rm(pdat, dat_list, i)

  as.list(environment()) %>% add_class("evalreg")
}

#' Summary method for the evalreg function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/evalreg.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{evalreg}}
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{evalreg}} to summarize results
#' @seealso \code{\link{plot.evalreg}} to plot results
#'
#' @examples
#' data.frame(price = diamonds$price, pred1 = rnorm(3000), pred2 = diamonds$price) %>%
#'   evalreg(pred = c("pred1", "pred2"), "price") %>%
#'   summary()
#'
#' @export
summary.evalreg <- function(object, dec = 3, ...) {
  if (is.character(object)) return(object)
  cat("Evaluate predictions for regression models\n")
  cat("Data        :", object$df_name, "\n")
  if (!is_empty(object$data_filter)) {
    cat("Filter      :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Results for :", object$train, "\n")
  cat("Predictors  :", paste0(object$pred, collapse = ", "), "\n")
  cat("Response    :", object$rvar, "\n\n")
  format_df(object$dat, dec = dec, mark = ",") %>%
    print(row.names = FALSE)
}

#' Plot method for the evalreg function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/evalreg.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{evalreg}}
#' @param vars Measures to plot, i.e., one or more of "Rsq", "RMSE", "MAE"
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{evalreg}} to generate results
#' @seealso \code{\link{summary.evalreg}} to summarize results
#'
#' @examples
#' data.frame(price = diamonds$price, pred1 = rnorm(3000), pred2 = diamonds$price) %>%
#'   evalreg(pred = c("pred1", "pred2"), "price") %>%
#'   plot()
#'
#' @export
plot.evalreg <- function(x, vars = c("Rsq", "RMSE", "MAE"), ...) {

  if (is.character(x) || is.null(x)) return(invisible())

  dat <- gather(x$dat, "Metric", "Value", !! vars, factor_key = TRUE) %>%
    mutate(Predictor = factor(Predictor, levels = unique(Predictor)))

  ## what data was used in evaluation? All, Training, Test, or Both
  type <- unique(dat$Type)

  p <- visualize(
    dat,
    xvar = "Predictor",
    yvar = "Value",
    type = "bar",
    facet_row = "Metric",
    fill = "Type",
    axes = "scale_y",
    custom = TRUE
  ) +
    labs(
      title = glue('Regression performance plots ({glue_collapse(type, ", ")})'),
      y = "",
      x = "Predictor",
      fill = ""
    )

  if (length(type) < 2) {
    p + theme(legend.position = "none")
  } else {
    p
  }
}

#' R-squared
#'
#' @param pred Prediction (vector)
#' @param rvar Response (vector)
#'
#' @return R-squared
#'
#' @export
Rsq <- function(pred, rvar) cor(pred, rvar)^2

#' Root Mean Squared Error
#'
#' @param pred Prediction (vector)
#' @param rvar Response (vector)
#'
#' @return Root Mean Squared Error
#'
#' @export
RMSE <- function(pred, rvar) sqrt(mean(unlist((pred - rvar)^2)))

#' Mean Absolute Error
#'
#' @param pred Prediction (vector)
#' @param rvar Response (vector)
#'
#' @return Mean Absolute Error
#'
#' @export
MAE <- function(pred, rvar) mean(unlist(abs(pred - rvar)))

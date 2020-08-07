#' Multinomial logistic regression
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/mnl.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param rvar The response variable in the model
#' @param evar Explanatory variables in the model
#' @param lev The level in the response variable to use as the baseline
#' @param int Interaction term to include in the model
#' @param wts Weights to use in estimation
#' @param check Use "standardize" to see standardized coefficient estimates. Use "stepwise-backward" (or "stepwise-forward", or "stepwise-both") to apply step-wise selection of variables in estimation.
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param envir Environment to extract data from
#'
#' @return A list with all variables defined in mnl as an object of class mnl
#'
#' @examples
#' result <- mnl(
#'   ketchup,
#'   rvar = "choice",
#'   evar = c("price.heinz28", "price.heinz32", "price.heinz41", "price.hunts32"),
#'   lev = "heinz28"
#' )
#' str(result)
#'
#' @seealso \code{\link{summary.mnl}} to summarize the results
#' @seealso \code{\link{plot.mnl}} to plot the results
#' @seealso \code{\link{predict.mnl}} to generate predictions
#' @seealso \code{\link{plot.model.predict}} to plot prediction output
#'
#' @export
mnl <- function(
  dataset, rvar, evar, lev = "", int = "",
  wts = "None", check = "",
  data_filter = "", envir = parent.frame()
) {

  if (rvar %in% evar) {
    return("Response variable contained in the set of explanatory variables.\nPlease update model specification." %>%
      add_class("mnl"))
  }

  vars <- c(rvar, evar)

  if (is_empty(wts, "None")) {
    wts <- NULL
  } else if (is_string(wts)) {
    wtsname <- wts
    vars <- c(rvar, evar, wtsname)
  }

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, vars, filt = data_filter, envir = envir)

  if (!is_empty(wts)) {
    if (exists("wtsname")) {
      wts <- dataset[[wtsname]]
      dataset <- select_at(dataset, .vars = base::setdiff(colnames(dataset), wtsname))
    }
    if (length(wts) != nrow(dataset)) {
      return(
        paste0("Length of the weights variable is not equal to the number of rows in the dataset (", format_nr(length(wts), dec = 0), " vs ", format_nr(nrow(dataset), dec = 0), ")") %>%
          add_class("mnl")
      )
    }
  }

  not_vary <- colnames(dataset)[summarise_all(dataset, does_vary) == FALSE]
  if (length(not_vary) > 0) {
    return(paste0("The following variable(s) show no variation. Please select other variables.\n\n** ", paste0(not_vary, collapse = ", "), " **") %>%
      add_class("mnl"))
  }

  rv <- dataset[[rvar]]
  if (lev == "") {
    if (is.factor(rv)) {
      lev <- levels(rv)[1]
    } else {
      lev <- as.character(rv) %>% as.factor() %>% levels() %>% .[1]
    }
  }

  ## re-leveling the
  dataset[[rvar]] <- dataset[[rvar]] %>% as.factor() %>% relevel(ref = lev)
  lev <- levels(dataset[[1]])

  vars <- ""
  var_check(evar, colnames(dataset)[-1], int) %>% {
    vars <<- .$vars
    evar <<- .$ev
    int <<- .$intv
  }

  ## add minmax attributes to data
  mmx <- minmax(dataset)

  ## scale data
  if ("standardize" %in% check) {
    dataset <- scale_df(dataset, wts = wts)
  } else if ("center" %in% check) {
    dataset <- scale_df(dataset, scale = FALSE, wts = wts)
  }

  if ("no_int" %in% check) {
    form_upper <- paste(rvar, "~ 0 +", paste(vars, collapse = " + ")) %>% as.formula()
    form_lower <- paste(rvar, "~ 0") %>% as.formula()
  } else {
    form_upper <- paste(rvar, "~ ", paste(vars, collapse = " + ")) %>% as.formula()
    form_lower <- paste(rvar, "~ 1") %>% as.formula()
  }

  if ("stepwise" %in% check) check <- sub("stepwise", "stepwise-backward", check)
  if ("stepwise-backward" %in% check) {
    ## use k = 2 for AIC, use k = log(nrow(dataset)) for BIC
    mnl_input <- list(formula = form_upper, weights = wts, data = dataset, model = TRUE, trace = FALSE)
    model <- do.call(nnet::multinom, mnl_input) %>%
      step(k = 2, scope = list(lower = form_lower), direction = "backward")

  } else if ("stepwise-forward" %in% check) {
    mnl_input <- list(formula = form_lower, weights = wts, data = dataset, model = TRUE, trace = FALSE)
    model <- do.call(nnet::multinom, mnl_input) %>%
      step(k = 2, scope = list(upper = form_upper), direction = "forward")

  } else if ("stepwise-both" %in% check) {
    mnl_input <- list(formula = form_lower, weights = wts, data = dataset, model = TRUE, trace = FALSE)
    model <- do.call(nnet::multinom, mnl_input) %>%
      step(k = 2, scope = list(lower = form_lower, upper = form_upper), direction = "both")

    ## adding full data even if all variables are not significant
  } else {
    mnl_input <- list(formula = form_upper, weights = wts, data = dataset, model = TRUE, trace = FALSE)
    model <- do.call(nnet::multinom, mnl_input)
  }

  coeff <- tidy(model) %>% as.data.frame()
  coeff$estimate <- log(coeff$estimate)

  ## needed for prediction if standardization or centering is used
  if ("standardize" %in% check || "center" %in% check) {
    attr(model$model, "radiant_ms") <- attr(dataset, "radiant_ms")
    attr(model$model, "radiant_sds") <- attr(dataset, "radiant_sds")
    attr(model$model, "radiant_sf") <- attr(dataset, "radiant_sf")
  }

  colnames(coeff) <- c("level", "label", "coefficient", "std.error", "z.value", "p.value")
  hasLevs <- sapply(select(dataset, -1), function(x) is.factor(x) || is.logical(x) || is.character(x))
  if (sum(hasLevs) > 0) {
    for (i in names(hasLevs[hasLevs])) {
      coeff$label %<>% gsub(paste0("^", i), paste0(i, "|"), .) %>%
        gsub(paste0(":", i), paste0(":", i, "|"), .)
    }
    rm(i)
  }

  coeff$sig_star <- sig_stars(coeff$p.value) %>% format(justify = "left")
  coeff$RRR <- exp(coeff$coefficient)
  coeff <- coeff[, c("level", "label", "RRR", "coefficient", "std.error", "z.value", "p.value", "sig_star")]

  ## adding null.deviance
  umod <- update(model, ~1, trace = FALSE)
  model$null.deviance <- -2 * logLik(umod)
  model$logLik <- logLik(model)
  model$nobs <- nrow(model$residuals)

  ## remove elements no longer needed
  rm(dataset, hasLevs, form_lower, form_upper, envir)

  as.list(environment()) %>% add_class(c("mnl", "model"))
}

#' Summary method for the mnl function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/mnl.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{mnl}}
#' @param sum_check Optional output. "confint" to show coefficient confidence interval estimates. "rrr" to show relative risk ratios (RRRs) and confidence interval estimates.
#' @param conf_lev Confidence level to use for coefficient and RRRs confidence intervals (.95 is the default)
#' @param test_var Variables to evaluate in model comparison (i.e., a competing models Chi-squared test)
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- mnl(
#'   ketchup,
#'   rvar = "choice",
#'   evar = c("price.heinz28", "price.heinz32", "price.heinz41", "price.hunts32"),
#'   lev = "heinz28"
#' )
#' summary(result)
#'
#' @seealso \code{\link{mnl}} to generate the results
#' @seealso \code{\link{plot.mnl}} to plot the results
#' @seealso \code{\link{predict.mnl}} to generate predictions
#' @seealso \code{\link{plot.model.predict}} to plot prediction output
#'
#' @importFrom car linearHypothesis
#'
#' @export
summary.mnl <- function(
  object, sum_check = "", conf_lev = .95,
  test_var = "", dec = 3, ...
) {

  if (is.character(object)) return(object)
  if (class(object$model)[1] != "multinom") return(object)

  if (any(grepl("stepwise", object$check))) {
    step_type <- if ("stepwise-backward" %in% object$check) {
      "Backward"
    } else if ("stepwise-forward" %in% object$check) {
      "Forward"
    } else {
      "Forward and Backward"
    }
    cat("----------------------------------------------------\n")
    cat(step_type, "stepwise selection of variables\n")
    cat("----------------------------------------------------\n")
  }

  cat("Multinomial logistic regression (MNL)")
  cat("\nData                 :", object$df_name)
  if (!is_empty(object$data_filter)) {
    cat("\nFilter               :", gsub("\\n", "", object$data_filter))
  }
  cat("\nResponse variable    :", object$rvar)
  cat("\nBase level           :", object$lev[1], "in", object$rvar)
  cat("\nExplanatory variables:", paste0(object$evar, collapse = ", "), "\n")
  if (length(object$wtsname) > 0) {
    cat("Weights used         :", object$wtsname, "\n")
  }
  expl_var <- if (length(object$evar) == 1) object$evar else "x"
  cat(paste0("Null hyp.: there is no effect of ", expl_var, " on ", object$rvar, "\n"))
  cat(paste0("Alt. hyp.: there is an effect of ", expl_var, " on ", object$rvar, "\n"))
  if ("standardize" %in% object$check) {
    cat("**Standardized RRRs and coefficients shown (2 X SD)**\n")
  } else if ("center" %in% object$check) {
    cat("**Centered RRRs and coefficients shown (x - mean(x))**\n")
  }
  if (object$model$convergence != 0) {
    cat("\n**Model did NOT converge. Consider standardizing the\nexplanatory variables and/or simplifying your model**\n")
  }

  cat("\n")

  coeff <- object$coeff
  coeff$label %<>% format(justify = "left")
  p.small <- coeff$p.value < .001
  coeff[, 3:7] %<>% format_df(dec)
  coeff$p.value[p.small] <- "< .001"
  coeff$RRR[grepl("(Intercept)", coeff$label)] <- ""
  dplyr::rename(coeff, `   ` = "level", `  ` = "label", ` ` = "sig_star") %>%
    print(row.names = FALSE)
  cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")

  mnl_fit <- glance(object$model)
  mnl_fit$null.deviance <- object$model$null.deviance
  mnl_fit$logLik <- object$model$logLik
  mnl_fit$BIC <- round(-2 * mnl_fit$logLik + ln(object$model$nobs) * with(mnl_fit, edf), dec)

  ## pseudo R2 (likelihood ratio) - http://en.wikipedia.org/wiki/Logistic_Model
  mnl_fit %<>% mutate(r2 = (null.deviance - deviance) / null.deviance) %>%
    round(dec)
  if (!is_empty(object$wts, "None") && (length(unique(object$wts)) > 2 || min(object$wts) >= 1)) {
    nobs <- sum(object$wts)
    mnl_fit$BIC <- round(-2 * mnl_fit$logLik + ln(nobs) * with(mnl_fit, edf), dec)
  } else {
    nobs <- object$model$nobs
  }

  # ## chi-squared test of overall model fit (p-value) - http://www.ats.ucla.edu/stat/r/dae/logit.htm
  chi_pval <- with(mnl_fit, pchisq(null.deviance - deviance, edf - 1, lower.tail = FALSE))
  chi_pval %<>% {if (. < .001) "< .001" else round(., dec)}

  cat("\nPseudo R-squared:", mnl_fit$r2)
  cat(paste0("\nLog-likelihood: ", mnl_fit$logLik, ", AIC: ", mnl_fit$AIC, ", BIC: ", mnl_fit$BIC))
  cat(paste0(
    "\nChi-squared: ", with(mnl_fit, null.deviance - deviance) %>% round(dec), " df(",
    with(mnl_fit, edf - 1), "), p.value ", chi_pval
  ), "\n")
  cat("Nr obs:", format_nr(nobs, dec = 0), "\n\n")

  if (anyNA(object$model$coeff)) {
    cat("The set of explanatory variables exhibit perfect multicollinearity.\nOne or more variables were dropped from the estimation.\n")
  }

  if (any(c("confint", "rrr") %in% sum_check)) {
    if (any(is.na(object$model$coeff))) {
      cat("There is perfect multicollineary in the set of explanatory variables.\nOne or more variables were dropped from the estimation.\n")
      cat("Confidence intervals were not calculated.\n")
    } else {
      ci_perc <- ci_label(cl = conf_lev)
      ci_tab <- confint(object$model, level = conf_lev)
      if (length(dim(ci_tab)) > 2) {
        ci_tab <- apply(ci_tab, 2, rbind)
      }
      ci_tab <- as.data.frame(ci_tab, stringsAsFactors = FALSE) %>%
        set_colnames(c("Low", "High")) %>%
        cbind(select(object$coeff, c(1, 2, 4)), .)

      if ("confint" %in% sum_check) {
        ci_tab %T>%
          {.$`+/-` <- (.$High - .$coefficient)} %>%
          format_df(dec) %>%
          set_colnames(c("  ", " ", "coefficient", ci_perc[1], ci_perc[2], "+/-")) %>%
          print(row.names = FALSE)
        cat("\n")
      }
    }
  }

  if ("rrr" %in% sum_check) {
    if (any(is.na(object$model$coeff))) {
      cat("RRRs were not calculated\n")
    } else {
      rrrlab <- if ("standardize" %in% object$check) "std RRR" else "RRR"
      ci_tab[,-c(1, 2)] <- exp(ci_tab[,-c(1, 2)])
      ci_tab[!grepl("(Intercept)", ci_tab[[2]]), ] %>%
        format_df(dec) %>%
        set_colnames(c("  ", "", rrrlab, ci_perc[1], ci_perc[2])) %>%
        print(row.names = FALSE)
      cat("\n")
    }
  }

  if (!is_empty(test_var)) {
    if (any(grepl("stepwise", object$check))) {
      cat("Model comparisons are not conducted when Stepwise has been selected.\n")
    } else {
      vars <- object$evar
      if (object$int != "" && length(vars) > 1) {
        ## updating test_var if needed
        test_var <- test_specs(test_var, object$int)
        vars <- c(vars, object$int)
      }

      no_int <- ifelse("no_int" %in% object$check, "~ 0 +", "~")
      not_selected <- base::setdiff(vars, test_var)
      if (length(not_selected) > 0) {
        sub_form <- paste(object$rvar, no_int, paste(not_selected, collapse = " + ")) %>% as.formula()
      } else {
        sub_form <- paste(object$rvar, no_int) %>% as.formula()
      }
      mnl_input <- list(formula = sub_form, weights = object$wts, data = object$model$model, trace = FALSE)
      mnl_sub <- do.call(nnet::multinom, mnl_input)
      mnl_sub_fit <- glance(mnl_sub)
      mnl_sub_fit$null.deviance <- object$model$null.deviance
      mnl_sub_test <- anova(mnl_sub, object$model, test = "Chi")

      pval <- mnl_sub_test[2, "Pr(Chi)"]
      df <- mnl_sub_test[2, 5]
      chi2 <- mnl_sub_test[2, "LR stat."]

      ## pseudo R2 (likelihood ratio) - http://en.wikipedia.org/wiki/Logistic_Model
      mnl_sub_fit %<>% mutate(r2 = (null.deviance - deviance) / null.deviance) %>% round(dec)
      mnl_sub_pval <- if (!is.na(pval) && pval < .001) "< .001" else round(pval, dec)
      cat(paste0(paste0("Model ", 1:2, ": ", object$rvar, " ~ ", mnl_sub_test$Model), collapse = "\n"))
      cat("\nPseudo R-squared, Model 1 vs 2:", c(mnl_sub_fit$r2, mnl_fit$r2))
      cat(paste0("\nChi-squared: ", round(chi2, dec), " df(", df, "), p.value ", mnl_sub_pval))
    }
  }

  if ("confusion" %in% sum_check) {
    cat("Confusion matrix:\n")
    predicted <- predict(object$model, type = "class")
    actual <- object$model$model[[object$rvar]]
    print(table(predicted, actual))
    cat("\nMisclassification error:", format_nr(mean(predicted != actual), perc = TRUE, dec = dec))
  }
}

#' Plot method for the mnl function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/mnl.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{mnl}}
#' @param plots Plots to produce for the specified MNL model. Use "" to avoid showing any plots (default). "dist" shows histograms (or frequency bar plots) of all variables in the model. "scatter" shows scatter plots (or box plots for factors) for the response variable with each explanatory variable. "coef" provides a coefficient plot
#' @param conf_lev Confidence level to use for coefficient and relative risk ratios (RRRs) intervals (.95 is the default)
#' @param intercept Include the intercept in the coefficient plot (TRUE or FALSE). FALSE is the default
#' @param nrobs Number of data points to show in scatter plots (-1 for all)
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{https://ggplot2.tidyverse.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- mnl(
#'   ketchup,
#'   rvar = "choice",
#'   evar = c("price.heinz28", "price.heinz32", "price.heinz41", "price.hunts32"),
#'   lev = "heinz28"
#' )
#' plot(result, plots = "coef")
#'
#' @seealso \code{\link{mnl}} to generate results
#' @seealso \code{\link{predict.mnl}} to generate predictions
#' @seealso \code{\link{plot.model.predict}} to plot prediction output
#'
#' @export
plot.mnl <- function(
  x, plots = "coef", conf_lev = .95,
  intercept = FALSE, nrobs = -1,
  shiny = FALSE, custom = FALSE, ...
) {

  if (is.character(x) || !inherits(x$model, "multinom")) return(x)
  if (is_empty(plots[1])) return("Please select a mnl regression plot from the drop-down menu")

  model <- x$model$model
  rvar <- x$rvar
  evar <- intersect(x$evar, colnames(model))
  vars <- c(rvar, evar)
  nrCol <- 2
  plot_list <- list()

  if ("dist" %in% plots) {
    for (i in vars)
      plot_list[[paste("dist_", i)]] <- select_at(model, .vars = i) %>%
        visualize(xvar = i, bins = 10, custom = TRUE)
  }

  if ("coef" %in% plots) {
    if (length(evar) == 0 && !intercept) return("** Model contains only an intercept **")

    yl <- {
      if (sum(c("standardize", "center") %in% x$check) == 2) {
        "RRR (Standardized & Centered)"
      } else if ("standardize" %in% x$check) {
        "RRR (standardized)"
      } else if ("center" %in% x$check) {
        "RRR (centered)"
      } else {
        "RRR"
      }
    }

    ci_tab <- confint(x$model, level = conf_lev)
    if (length(dim(ci_tab)) > 2) {
      ci_tab <- apply(ci_tab, 2, rbind)
      color <- "level"
    } else {
      color = NULL
    }
    ci_tab <- as.data.frame(ci_tab, stringsAsFactors = FALSE) %>%
      na.omit() %>%
      set_colnames(c("Low", "High")) %>%
      cbind(select(x$coeff, c(1, 2, 4)), .)

    if (!isTRUE(intercept)) {
      ci_tab <- ci_tab[!grepl("(Intercept)", ci_tab[[2]]), ]
    }
    labels <- unique(ci_tab[[2]])
    ci_tab[, -c(1, 2)] <- exp(ci_tab[, -c(1, 2)])

    nrCol <- 1
    plot_list[["coef"]] <- ggplot(ci_tab) +
        geom_pointrange(aes_string(x = "label", y = "coefficient", ymin = "Low", ymax = "High", color = color), position = position_dodge(width = -0.6)) +
        geom_hline(yintercept = 1, linetype = "dotdash", color = "blue") +
        labs(y = yl, x = "") +
        ## can't use coord_trans together with coord_flip
        ## http://stackoverflow.com/a/26185278/1974918
        scale_x_discrete(limits = rev(labels)) +
        scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.5, 1, 2, 5, 10), trans = "log") +
        coord_flip() +
        theme(axis.text.y = element_text(hjust = 0))
  }

  if ("correlations" %in% plots) {
    if (length(evar) == 0) {
      message("Model contains only an intercept. Correlation plot cannot be generated")
    } else {
      return(radiant.basics:::plot.correlation(select_at(model, .vars = vars), nrobs = nrobs))
    }
  }

  if (length(plot_list) > 0) {
    if (custom) {
      if (length(plot_list) == 1) plot_list[[1]] else plot_list
    } else {
      patchwork::wrap_plots(plot_list, ncol = nrCol) %>%
        {if (shiny) . else print(.)}
    }
  }
}

#' Predict method for the mnl function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/mnl.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{mnl}}
#' @param pred_data Provide the dataframe to generate predictions (e.g., ketchup). The dataset must contain all columns used in the estimation
#' @param pred_cmd Generate predictions using a command. For example, `pclass = levels(pclass)` would produce predictions for the different levels of factor `pclass`. To add another variable, create a vector of prediction strings, (e.g., c('pclass = levels(pclass)', 'age = seq(0,100,20)')
#' @param pred_names Names for the predictions to be stored. If one name is provided, only the first column of predictions is stored. If empty, the levels in the response variable of the mnl model will be used
#' @param dec Number of decimals to show
#' @param envir Environment to extract data from
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- mnl(
#'   ketchup,
#'   rvar = "choice",
#'   evar = c("price.heinz28", "price.heinz32", "price.heinz41", "price.hunts32"),
#'   lev = "heinz28"
#' )
#' predict(result, pred_cmd = "price.heinz28 = seq(3, 5, 0.1)")
#' predict(result, pred_data = slice(ketchup, 1:20))
#'
#' @seealso \code{\link{mnl}} to generate the result
#' @seealso \code{\link{summary.mnl}} to summarize results
#'
#' @export
predict.mnl <- function(
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
    pred_val <- try(sshhr(predict(model, pred, type = "probs")), silent = TRUE)

    if (!inherits(pred_val, "try-error")) {
      # if (is.numeric(pred_val)) pred_val <- t(pred_val)
      # if (is.null(dim(pred_val))) pred_val <- t(pred_val)
      if (is.vector(pred_val)) pred_val <- t(pred_val)
      pred_val %<>% as.data.frame(stringsAsFactors = FALSE)
      if (all(is_empty(pred_names))) pred_names <- colnames(pred_val)
      pred_val %<>% select(1:min(ncol(pred_val), length(pred_names))) %>%
        set_colnames(pred_names)
    }

    pred_val
  }

  predict_model(object, pfun, "mnl.predict", pred_data, pred_cmd, dec = dec, envir = envir) %>%
    set_attr("radiant_pred_data", df_name)
}

#' Print method for mnl.predict
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#'
#' @export
print.mnl.predict <- function(x, ..., n = 10)
  print_predict_model(x, ..., n = n, header = "Multinomial logistic regression (MNL)", lev = attr(x, "radiant_lev"))

#' Plot method for mnl.predict function
#'
#' @param x Return value from predict function predict.mnl
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
#'
#' @export
plot.mnl.predict <- function(
  x, xvar = "", facet_row = ".", facet_col = ".",
  color = ".class", ...
) {

  ## should work with req in regress_ui but doesn't
  if (is_empty(xvar)) return(invisible())

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

#' Store predicted values generated in the mnl function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/mnl.html} for an example in Radiant
#'
#' @param dataset Dataset to add predictions to
#' @param object Return value from model function
#' @param name Variable name(s) assigned to predicted values. If empty, the levels of the response variable will be used
#' @param ... Additional arguments
#'
#' @examples
#' result <- mnl(
#'   ketchup,
#'   rvar = "choice",
#'   evar = c("price.heinz28", "price.heinz32", "price.heinz41", "price.hunts32"),
#'   lev = "heinz28"
#' )
#' pred <- predict(result, pred_data = ketchup)
#' ketchup <- store(ketchup, pred, name = c("heinz28", "heinz32", "heinz41", "hunts32"))
#'
#' @export
store.mnl.predict <- function(dataset, object, name = NULL, ...) {

  ## extract the names of the variables predicted
  pvars <- base::setdiff(attr(object, "radiant_vars"), attr(object, "radiant_evar"))

  ## as.vector removes all attributes from df
  df <- as.vector(object[, pvars])

  if (is_empty(name)) {
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

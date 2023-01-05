#' Logistic regression
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/logistic.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param rvar The response variable in the model
#' @param evar Explanatory variables in the model
#' @param lev The level in the response variable defined as _success_
#' @param int Interaction term to include in the model
#' @param wts Weights to use in estimation
#' @param check Use "standardize" to see standardized coefficient estimates. Use "stepwise-backward" (or "stepwise-forward", or "stepwise-both") to apply step-wise selection of variables in estimation. Add "robust" for robust estimation of standard errors (HC1)
#' @param form Optional formula to use instead of rvar, evar, and int
#' @param ci_type To use the profile-likelihood (rather than Wald) for confidence intervals use "profile". For datasets with more than 5,000 rows the Wald method will be used, unless "profile" is explicitly set
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param arr Expression to arrange (sort) the data on (e.g., "color, desc(price)")
#' @param rows Rows to select from the specified dataset
#' @param envir Environment to extract data from
#'
#' @return A list with all variables defined in logistic as an object of class logistic
#'
#' @examples
#' logistic(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>% summary()
#' logistic(titanic, "survived", c("pclass", "sex")) %>% str()
#' @seealso \code{\link{summary.logistic}} to summarize the results
#' @seealso \code{\link{plot.logistic}} to plot the results
#' @seealso \code{\link{predict.logistic}} to generate predictions
#' @seealso \code{\link{plot.model.predict}} to plot prediction output
#'
#' @importFrom sandwich vcovHC
#'
#' @export
logistic <- function(dataset, rvar, evar, lev = "", int = "",
                     wts = "None", check = "", form, ci_type,
                     data_filter = "", arr = "", rows = NULL, envir = parent.frame()) {
  if (!missing(form)) {
    form <- as.formula(format(form))
    paste0(format(form), collapse = "")

    vars <- all.vars(form)
    rvar <- vars[1]
    evar <- vars[-1]
  }

  if (rvar %in% evar) {
    return("Response variable contained in the set of explanatory variables.\nPlease update model specification." %>%
      add_class("logistic"))
  }

  vars <- c(rvar, evar)

  if (is.empty(wts, "None")) {
    wts <- NULL
  } else if (is_string(wts)) {
    wtsname <- wts
    vars <- c(rvar, evar, wtsname)
  }

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  if (any(evar == ".")) {
    dataset <- get_data(dataset, "", filt = data_filter, arr = arr, rows = rows, envir = envir)
    evar <- setdiff(colnames(dataset), rvar)
  } else {
    dataset <- get_data(dataset, vars, filt = data_filter, arr = arr, rows = rows, envir = envir)
  }

  if (missing(ci_type)) {
    ## Use profiling for smaller datasets
    if (nrow(na.omit(dataset)) < 5000) {
      ci_type <- "profile"
    } else {
      ci_type <- "default"
    }
  }

  if (!is.empty(wts)) {
    if (exists("wtsname")) {
      wts <- dataset[[wtsname]]
      dataset <- select_at(dataset, .vars = base::setdiff(colnames(dataset), wtsname))
    }
    if (length(wts) != nrow(dataset)) {
      return(
        paste0("Length of the weights variable is not equal to the number of rows in the dataset (", format_nr(length(wts), dec = 0), " vs ", format_nr(nrow(dataset), dec = 0), ")") %>%
          add_class("logistic")
      )
    }
    if (!is.integer(wts)) {
      if (length(unique(wts)) == 2 && min(wts) < 1) {
        check <- union(check, "robust")
      }
    }
  }

  not_vary <- colnames(dataset)[summarise_all(dataset, does_vary) == FALSE]
  if (length(not_vary) > 0) {
    return(paste0("The following variable(s) show no variation. Please select other variables.\n\n** ", paste0(not_vary, collapse = ", "), " **") %>%
      add_class("logistic"))
  }

  rv <- dataset[[rvar]]
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

  if (!missing(form)) {
    int <- setdiff(attr(terms.formula(form), "term.labels"), evar)
  }

  vars <- ""
  var_check(evar, colnames(dataset)[-1], int) %>%
    {
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

  if (missing(form)) {
    form_upper <- paste(rvar, "~", paste(vars, collapse = " + ")) %>% as.formula()
  } else {
    form_upper <- form
    rm(form)
  }

  form_lower <- paste(rvar, "~ 1") %>% as.formula()
  if ("stepwise" %in% check) check <- sub("stepwise", "stepwise-backward", check)
  if ("stepwise-backward" %in% check) {
    ## use k = 2 for AIC, use k = log(nrow(dataset)) for BIC
    model <- sshhr(glm(form_upper, weights = wts, family = binomial(link = "logit"), data = dataset)) %>%
      step(k = 2, scope = list(lower = form_lower), direction = "backward")
  } else if ("stepwise-forward" %in% check) {
    model <- sshhr(glm(form_lower, weights = wts, family = binomial(link = "logit"), data = dataset)) %>%
      step(k = 2, scope = list(upper = form_upper), direction = "forward")
  } else if ("stepwise-both" %in% check) {
    model <- sshhr(glm(form_lower, weights = wts, family = binomial(link = "logit"), data = dataset)) %>%
      step(k = 2, scope = list(lower = form_lower, upper = form_upper), direction = "both")
  } else {
    model <- sshhr(glm(form_upper, weights = wts, family = binomial(link = "logit"), data = dataset))
  }

  ## needed for prediction if standardization or centering is used
  if ("standardize" %in% check || "center" %in% check) {
    attr(model$model, "radiant_ms") <- attr(dataset, "radiant_ms")
    attr(model$model, "radiant_sds") <- attr(dataset, "radiant_sds")
    attr(model$model, "radiant_sf") <- attr(dataset, "radiant_sf")
  }

  coeff <- tidy(model) %>%
    na.omit() %>%
    as.data.frame()
  colnames(coeff) <- c("label", "coefficient", "std.error", "z.value", "p.value")
  hasLevs <- sapply(select(dataset, -1), function(x) is.factor(x) || is.logical(x) || is.character(x))
  if (sum(hasLevs) > 0) {
    for (i in names(hasLevs[hasLevs])) {
      coeff$label %<>% gsub(paste0("^", i), paste0(i, "|"), .) %>%
        gsub(paste0(":", i), paste0(":", i, "|"), .)
    }
    rm(i)
  }

  if ("robust" %in% check) {
    vcov <- sandwich::vcovHC(model, type = "HC1")
    coeff$std.error <- sqrt(diag(vcov))
    coeff$z.value <- coeff$coefficient / coeff$std.error
    coeff$p.value <- 2 * pnorm(abs(coeff$z.value), lower.tail = FALSE)
  }

  coeff$sig_star <- sig_stars(coeff$p.value) %>% format(justify = "left")
  coeff$OR <- exp(coeff$coefficient)
  coeff$`OR%` <- with(coeff, ifelse(OR < 1, -(1 - OR), OR - 1))
  coeff <- coeff[, c("label", "OR", "OR%", "coefficient", "std.error", "z.value", "p.value", "sig_star")]

  ## remove elements no longer needed
  rm(dataset, hasLevs, form_lower, form_upper, envir)

  # added for consistency with other model types
  type <- "classification"

  as.list(environment()) %>% add_class(c("logistic", "model"))
}

#' Summary method for the logistic function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/logistic.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{logistic}}
#' @param sum_check Optional output. "vif" to show multicollinearity diagnostics. "confint" to show coefficient confidence interval estimates. "odds" to show odds ratios and confidence interval estimates.
#' @param conf_lev Confidence level to use for coefficient and odds confidence intervals (.95 is the default)
#' @param test_var Variables to evaluate in model comparison (i.e., a competing models Chi-squared test)
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#'
#' result <- logistic(titanic, "survived", "pclass", lev = "Yes")
#' result <- logistic(titanic, "survived", "pclass", lev = "Yes")
#' summary(result, test_var = "pclass")
#' res <- logistic(titanic, "survived", c("pclass", "sex"), int = "pclass:sex", lev = "Yes")
#' summary(res, sum_check = c("vif", "confint", "odds"))
#' titanic %>%
#'   logistic("survived", c("pclass", "sex", "age"), lev = "Yes") %>%
#'   summary("vif")
#' @seealso \code{\link{logistic}} to generate the results
#' @seealso \code{\link{plot.logistic}} to plot the results
#' @seealso \code{\link{predict.logistic}} to generate predictions
#' @seealso \code{\link{plot.model.predict}} to plot prediction output
#'
#' @importFrom car vif linearHypothesis
#'
#' @export
summary.logistic <- function(object, sum_check = "", conf_lev = .95,
                             test_var = "", dec = 3, ...) {
  if (is.character(object)) {
    return(object)
  }
  if (class(object$model)[1] != "glm") {
    return(object)
  }

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

  cat("Logistic regression (GLM)")
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
  cat("\nLevel                :", object$lev, "in", object$rvar)
  cat("\nExplanatory variables:", paste0(object$evar, collapse = ", "), "\n")
  if (length(object$wtsname) > 0) {
    cat("Weights used         :", object$wtsname, "\n")
  } else if (length(object$wts) > 0) {
    cat("Weights used         :", deparse(substitute(object$wts)), "\n")
  }
  expl_var <- if (length(object$evar) == 1) object$evar else "x"
  cat(paste0("Null hyp.: there is no effect of ", expl_var, " on ", object$rvar, "\n"))
  cat(paste0("Alt. hyp.: there is an effect of ", expl_var, " on ", object$rvar, "\n"))
  if ("standardize" %in% object$check) {
    cat("**Standardized odds-ratios and coefficients shown (2 X SD)**\n")
  } else if ("center" %in% object$check) {
    cat("**Centered odds-ratios and coefficients shown (x - mean(x))**\n")
  }
  if ("robust" %in% object$check) {
    cat("**Robust standard errors used**\n")
  }
  cat("\n")

  coeff <- object$coeff
  coeff$label %<>% format(justify = "left")
  p.small <- coeff$p.value < .001
  coeff[, c(2, 4:7)] %<>% format_df(dec)
  coeff[["OR%"]] %<>% format_nr(perc = TRUE, dec = dec - 2, na.rm = FALSE)
  coeff$p.value[p.small] <- "< .001"
  dplyr::rename(coeff, `  ` = "label", ` ` = "sig_star") %>%
    (function(x) {
      x$OR[1] <- x$`OR%`[1] <- ""
      x
    }) %>%
    print(row.names = FALSE)
  cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")

  logit_fit <- glance(object$model)

  ## pseudo R2 (likelihood ratio) - http://en.wikipedia.org/wiki/Logistic_Model
  logit_fit$rnk <- object$model$rank
  logit_fit <- logit_fit %>%
    mutate(
      llnull = null.deviance / -2,
      llfull = deviance / -2,
      r2 = 1 - llfull / llnull,
      r2_adj = 1 - (llfull - (rnk - 1)) / llnull,
      auc = auc(object$model$fitted.values, object$model$model[[object$rvar]])
    ) %>%
    round(dec)

  if (!is.empty(object$wts, "None") && (length(unique(object$wts)) > 2 || min(object$wts) >= 1)) {
    nobs <- sum(object$wts)
    logit_fit$BIC <- round(-2 * logit_fit$logLik + ln(nobs) * with(logit_fit, 1 + df.null - df.residual), dec)
  } else {
    nobs <- logit_fit$nobs
  }

  ## chi-squared test of overall model fit (p-value) - http://www.ats.ucla.edu/stat/r/dae/logit.htm
  chi_pval <- with(object$model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) %>%
    (function(x) if (x < .001) "< .001" else round(x, dec))

  cat(paste0("\nPseudo R-squared:", logit_fit$r2, ", Adjusted Pseudo R-squared:", logit_fit$r2_adj))
  cat(paste0("\nAUC: ", logit_fit$auc, ", Log-likelihood: ", logit_fit$logLik, ", AIC: ", logit_fit$AIC, ", BIC: ", logit_fit$BIC))
  cat(paste0(
    "\nChi-squared: ", with(logit_fit, null.deviance - deviance) %>% round(dec), " df(",
    with(logit_fit, df.null - df.residual), "), p.value ", chi_pval
  ), "\n")
  cat("Nr obs:", format_nr(nobs, dec = 0), "\n\n")

  if (anyNA(object$model$coeff)) {
    cat("The set of explanatory variables exhibit perfect multicollinearity.\nOne or more variables were dropped from the estimation.\n")
  }

  if ("vif" %in% sum_check) {
    if (anyNA(object$model$coeff)) {
      cat("Multicollinearity diagnostics were not calculated.")
    } else {
      ## needed to adjust when step-wise regression is used
      if (length(attributes(object$model$terms)$term.labels) > 1) {
        cat("Variance Inflation Factors\n")
        car::vif(object$model) %>%
          {
            if (is.null(dim(.))) . else .[, "GVIF"]
          } %>% ## needed when factors are included
          data.frame(VIF = ., Rsq = 1 - 1 / ., stringsAsFactors = FALSE) %>%
          .[order(.$VIF, decreasing = TRUE), ] %>% ## not using arrange to keep rownames
          round(dec) %>%
          {
            if (nrow(.) < 8) t(.) else .
          } %>%
          print()
      } else {
        cat("Insufficient number of explanatory variables to calculate\nmulticollinearity diagnostics (VIF)\n")
      }
    }
    cat("\n")
  }

  if (any(c("confint", "odds") %in% sum_check)) {
    if (any(is.na(object$model$coeff))) {
      cat("There is perfect multicollineary in the set of explanatory variables.\nOne or more variables were dropped from the estimation.\n")
      cat("Confidence intervals were not calculated.\n")
    } else {
      ci_perc <- ci_label(cl = conf_lev)

      if ("robust" %in% object$check) {
        cnfint <- radiant.model::confint_robust
      } else if (object$ci_type == "profile") {
        cnfint <- confint
      } else {
        cnfint <- confint.default
      }

      ci_tab <- cnfint(object$model, level = conf_lev, vcov = object$vcov) %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        set_colnames(c("Low", "High")) %>%
        cbind(select_at(object$coeff, "coefficient"), .)

      if ("confint" %in% sum_check) {
        ci_tab %T>%
          {
            .$`+/-` <- (.$High - .$coefficient)
          } %>%
          format_df(dec) %>%
          set_colnames(c("coefficient", ci_perc[1], ci_perc[2], "+/-")) %>%
          set_rownames(object$coeff$label) %>%
          print()
        cat("\n")
      }
    }
  }

  if ("odds" %in% sum_check) {
    if (any(is.na(object$model$coeff))) {
      cat("Odds ratios were not calculated\n")
    } else {
      orlab <- if ("standardize" %in% object$check) "std odds ratio" else "odds ratio"
      exp(ci_tab[-1, ]) %>%
        format_df(dec) %>%
        set_colnames(c(orlab, ci_perc[1], ci_perc[2])) %>%
        set_rownames(object$coeff$label[-1]) %>%
        print()
      cat("\n")
    }
  }

  if (!is.empty(test_var)) {
    if (any(grepl("stepwise", object$check))) {
      cat("Model comparisons are not conducted when Stepwise has been selected.\n")
    } else {
      # sub_form <- ". ~ 1"
      sub_form <- paste(object$rvar, "~ 1")

      vars <- object$evar
      if (object$int != "" && length(vars) > 1) {
        ## updating test_var if needed
        test_var <- test_specs(test_var, object$int)
        vars <- c(vars, object$int)
      }

      not_selected <- base::setdiff(vars, test_var)
      if (length(not_selected) > 0) sub_form <- paste(object$rvar, "~", paste(not_selected, collapse = " + "))
      ## update with logit_sub NOT working when called from radiant - strange
      # logit_sub <- update(object$model, sub_form, data = object$model$model)
      logit_sub <- sshhr(glm(as.formula(sub_form), weights = object$wts, family = binomial(link = "logit"), data = object$model$model))
      logit_sub_fit <- glance(logit_sub)
      logit_sub_test <- anova(logit_sub, object$model, test = "Chi")

      matchCf <- function(clist, vlist) {
        matcher <- function(vl, cn) {
          if (grepl(":", vl)) {
            strsplit(vl, ":") %>%
              unlist() %>%
              sapply(function(x) gsub("var", x, "((var.*:)|(:var))")) %>%
              paste0(collapse = "|") %>%
              grepl(cn) %>%
              cn[.]
          } else {
            mf <- grepl(paste0("^", vl, "$"), cn) %>% cn[.]
            if (length(mf) == 0) {
              mf <- grepl(paste0("^", vl), cn) %>% cn[.]
            }
            mf
          }
        }

        cn <- names(clist)
        sapply(vlist, matcher, cn) %>% unname()
      }

      if ("robust" %in% object$check) {
        ## http://stats.stackexchange.com/a/132521/61693
        logit_sub_lh <- car::linearHypothesis(
          object$model,
          matchCf(object$model$coef, test_var),
          vcov = object$vcov
        )
        pval <- logit_sub_lh[2, "Pr(>Chisq)"]
        df <- logit_sub_lh[2, "Df"]
        chi2 <- logit_sub_lh[2, "Chisq"]
      } else {
        pval <- logit_sub_test[2, "Pr(>Chi)"]
        df <- logit_sub_test[2, "Df"]
        chi2 <- logit_sub_test[2, "Deviance"]
      }

      ## pseudo R2 (likelihood ratio) - http://en.wikipedia.org/wiki/Logistic_Model
      logit_sub_fit %<>% mutate(r2 = (null.deviance - deviance) / null.deviance) %>% round(dec)
      logit_sub_pval <- if (!is.na(pval) && pval < .001) "< .001" else round(pval, dec)
      cat(attr(logit_sub_test, "heading")[2])
      cat("\nPseudo R-squared, Model 1 vs 2:", c(logit_sub_fit$r2, logit_fit$r2))
      cat(paste0("\nChi-squared: ", round(chi2, dec), " df(", df, "), p.value ", logit_sub_pval))
    }
  }
}

#' Plot method for the logistic function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/logistic.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{logistic}}
#' @param plots Plots to produce for the specified GLM model. Use "" to avoid showing any plots (default). "dist" shows histograms (or frequency bar plots) of all variables in the model. "scatter" shows scatter plots (or box plots for factors) for the response variable with each explanatory variable. "coef" provides a coefficient plot and "influence" shows (potentially) influential observations
#' @param conf_lev Confidence level to use for coefficient and odds confidence intervals (.95 is the default)
#' @param intercept Include the intercept in the coefficient plot (TRUE or FALSE). FALSE is the default
#' @param incl Which variables to include in a coefficient plot
#' @param excl Which variables to exclude in a coefficient plot
#' @param incl_int Which interactions to investigate in PDP plots
#' @param nrobs Number of data points to show in scatter plots (-1 for all)
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{https://ggplot2.tidyverse.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- logistic(titanic, "survived", c("pclass", "sex"), lev = "Yes")
#' plot(result, plots = "coef")
#' @seealso \code{\link{logistic}} to generate results
#' @seealso \code{\link{plot.logistic}} to plot results
#' @seealso \code{\link{predict.logistic}} to generate predictions
#' @seealso \code{\link{plot.model.predict}} to plot prediction output
#'
#' @importFrom broom augment
#' @importFrom rlang .data
#'
#' @export
plot.logistic <- function(x, plots = "coef", conf_lev = .95,
                          intercept = FALSE, incl = NULL, excl = NULL, incl_int = NULL,
                          nrobs = -1, shiny = FALSE, custom = FALSE, ...) {
  if (is.character(x) || !inherits(x$model, "glm")) {
    return(x)
  }
  if (is.empty(plots[1])) {
    return("Please select a logistic regression plot from the drop-down menu")
  }

  if ("(weights)" %in% colnames(x$model$model) &&
    min(x$model$model[["(weights)"]]) == 0) {
    ## broom::augment chokes when a weight variable has 0s
    model <- x$model$model
    model$.fitted <- predict(x$model, type = "response")
  } else {
    model <- broom::augment(x$model, type.predict = "response")
  }

  ## adjustment in case max > 1 (e.g., values are 1 and 2)
  model$.actual <- as_integer(x$rv) %>%
    (function(x) x - max(x) + 1)

  rvar <- x$rvar
  evar <- intersect(x$evar, colnames(model))
  vars <- c(rvar, evar)
  nrCol <- 2
  plot_list <- list()

  ## use orginal data rather than the logical used for estimation
  model[[rvar]] <- x$rv

  if ("dist" %in% plots) {
    for (i in vars) {
      plot_list[[paste("dist_", i)]] <- select_at(model, .vars = i) %>%
        visualize(xvar = i, bins = 10, custom = TRUE)
    }
  }

  if ("coef" %in% plots) {
    if (nrow(x$coeff) == 1 && !intercept) {
      return("** Model contains only an intercept **")
    }

    yl <- {
      if (sum(c("standardize", "center") %in% x$check) == 2) {
        "Odds-ratio (Standardized & Centered)"
      } else if ("standardize" %in% x$check) {
        "Odds-ratio (standardized)"
      } else if ("center" %in% x$check) {
        "Odds-ratio (centered)"
      } else {
        "Odds-ratio"
      }
    }

    nrCol <- 1
    if ("robust" %in% x$check) {
      cnfint <- radiant.model::confint_robust
    } else if (x$ci_type == "profile") {
      cnfint <- confint
    } else {
      cnfint <- confint.default
    }

    coef_df <- cnfint(x$model, level = conf_lev, vcov = x$vcov) %>%
      exp(.) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      na.omit() %>%
      set_colnames(c("Low", "High")) %>%
      cbind(select(x$coeff, 2), .) %>%
      set_rownames(x$coeff$label) %>%
      {
        if (!intercept) .[-1, ] else .
      } %>%
      mutate(variable = factor(rownames(.), levels = rownames(.)))

    # addressing issues with extremely high upper bounds
    coef_df[coef_df$High > 10000, c("Low", "High")] <- NA
    coef_df <- na.omit(coef_df)

    if (length(incl) > 0) {
      incl <- paste0("^(", paste0(incl, "[|]*", collapse = "|"), ")")
      incl <- grepl(incl, coef_df$variable)
      if (isTRUE(intercept)) incl[1] <- TRUE
      coef_df <- coef_df[incl, ]
    }
    if (length(excl) > 0) {
      excl <- paste0("^(", paste0(excl, "[|]*", collapse = "|"), ")")
      if (isTRUE(intercept)) excl[1] <- TRUE
      coef_df <- coef_df[!excl, ]
    }
    coef_df <- droplevels(coef_df)

    plot_list[["coef"]] <- ggplot(coef_df) +
      geom_pointrange(aes(x = .data$variable, y = .data$OR, ymin = .data$Low, ymax = .data$High)) +
      geom_hline(yintercept = 1, linetype = "dotdash", color = "blue") +
      labs(y = yl, x = "") +
      ## can't use coord_trans together with coord_flip
      ## http://stackoverflow.com/a/26185278/1974918
      scale_x_discrete(limits = rev(coef_df$variable)) +
      scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.5, 1, 2, 5, 10), trans = "log") +
      coord_flip() +
      theme(axis.text.y = element_text(hjust = 0))
  }

  if ("scatter" %in% plots) {
    nrobs <- as.integer(nrobs)
    if (nrobs > 0 && nrobs < nrow(model)) {
      model <- sample_n(model, nrobs, replace = FALSE)
    }
    for (i in evar) {
      if ("factor" %in% class(model[[i]])) {
        plot_list[[paste0("scatter_", i)]] <- ggplot(model, aes(x = .data[[i]], fill = .data[[rvar]])) +
          geom_bar(position = "fill", alpha = 0.5) +
          labs(y = "")
      } else {
        plot_list[[paste0("scatter_", i)]] <- select_at(model, .vars = c(i, rvar)) %>%
          visualize(xvar = rvar, yvar = i, check = "jitter", type = "scatter", custom = TRUE)
      }
    }
    nrCol <- 1
  }

  if ("fit" %in% plots) {
    nrCol <- 1

    if (nrow(model) < 30) {
      return("Insufficient observations to generate Model fit plot")
    }

    model$.fittedbin <- radiant.data::xtile(model$.fitted, 30)

    min_bin <- min(model$.fittedbin)
    max_bin <- max(model$.fittedbin)

    if (prop(model$.actual[model$.fittedbin == min_bin]) < prop(model$.actual[model$.fittedbin == max_bin])) {
      model$.fittedbin <- 1 + max_bin - model$.fittedbin
      df <- group_by_at(model, .vars = ".fittedbin") %>%
        summarise(Probability = mean(.fitted))
    } else {
      df <- group_by_at(model, .vars = ".fittedbin") %>%
        summarise(Probability = mean(1 - .fitted))
    }

    plot_list[["fit"]] <-
      visualize(model, xvar = ".fittedbin", yvar = ".actual", type = "bar", custom = TRUE) +
      geom_line(data = df, aes(y = .data$Probability), color = "blue", linewidth = 1) +
      ylim(0, 1) +
      labs(title = "Actual vs Fitted values (binned)", x = "Predicted probability bins", y = "Probability")
  }

  if ("correlations" %in% plots) {
    if (length(evar) == 0) {
      message("Model contains only an intercept. Correlation plot cannot be generated")
    } else {
      return(radiant.basics:::plot.correlation(select_at(model, .vars = vars), nrobs = nrobs))
    }
  }

  if ("influence" %in% plots) {
    nrCol <- 1

    ## based on http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/
    mod <- model %>%
      select(.std.resid, .cooksd) %>%
      mutate(index = 1:n(), .cooksd.max = .cooksd) %>%
      arrange(desc(.cooksd)) %>%
      mutate(index.max = 1:n(), .cooksd.max = ifelse(index.max < 4, .cooksd, NA)) %>%
      mutate(index.max = ifelse(index.max < 4, index, NA)) %>%
      arrange(index)

    mod <- mutate(mod, .std.resid = ifelse(abs(.std.resid) < 1 & is.na(index.max), NA, .std.resid))
    lim <- max(abs(mod$.std.resid), na.rm = TRUE) %>%
      (function(x) c(min(-4, -x), max(4, x)))
    plot_list[["influence"]] <- ggplot(mod, aes(index, .std.resid)) +
      geom_point(aes(size = .cooksd), alpha = 0.5) +
      ggrepel::geom_text_repel(aes(label = index.max)) +
      geom_hline(yintercept = c(-1, -3, 1, 3), linetype = "longdash", linewidth = 0.25) +
      scale_y_continuous(breaks = -4:4, limits = lim) +
      labs(
        title = "Influential observations",
        x = "Observation index",
        y = "Standardized residuals",
        size = "cooksd"
      )
  }

  if ("pred_plot" %in% plots) {
    ncol <- 2
    if (length(incl) > 0 | length(incl_int) > 0) {
      plot_list <- pred_plot(x, plot_list, incl, incl_int, ...)
    } else {
      return("Select one or more variables to generate Prediction plots")
    }
  }

  if ("pdp" %in% plots) {
    nrCol <- 2
    if (length(incl) > 0 | length(incl_int) > 0) {
      plot_list <- pdp_plot(x, plot_list, incl, incl_int, ...)
    } else {
      return("Select one or more variables to generate Partial Dependence Plots")
    }
  }

  if ("vip" %in% plots) {
    nrCol <- 1
    if (length(evar) < 2) {
      message("Model must contain at least 2 explanatory variables (features). Permutation Importance plot cannot be generated")
    } else {
      vi_scores <- varimp(x)
      plot_list[["vip"]] <-
        visualize(vi_scores, yvar = "Importance", xvar = "Variable", type = "bar", custom = TRUE) +
        labs(
          title = "Permutation Importance",
          x = NULL,
          y = "Importance (AUC decrease)"
        ) +
        coord_flip() +
        theme(axis.text.y = element_text(hjust = 0))
    }
  }

  if ("linearity" %in% plots) {
    ## based on http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/
    mod <- select_at(model, .vars = c(".fitted", evar)) %>% dplyr::select_if(is.numeric)
    predictors <- setdiff(colnames(mod), ".fitted")
    mod <- mutate(mod, logit = log(.fitted / (1 - .fitted))) %>%
      select(-.fitted) %>%
      gather(key = "predictors", value = "predictor.value", -logit)
    plot_list[["linearity"]] <- ggplot(mod, aes(logit, predictor.value)) +
      geom_point(size = 0.5, alpha = 0.5) +
      geom_smooth(method = "loess") +
      facet_wrap(~predictors, scales = "free_y") +
      labs(
        title = "Checking linearity assumption",
        y = NULL,
        x = NULL
      )
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

#' Predict method for the logistic function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/logistic.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{logistic}}
#' @param pred_data Provide the dataframe to generate predictions (e.g., titanic). The dataset must contain all columns used in the estimation
#' @param pred_cmd Generate predictions using a command. For example, `pclass = levels(pclass)` would produce predictions for the different levels of factor `pclass`. To add another variable, create a vector of prediction strings, (e.g., c('pclass = levels(pclass)', 'age = seq(0,100,20)')
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param se Logical that indicates if prediction standard errors should be calculated (default = FALSE)
#' @param interval Type of interval calculation ("confidence" or "none"). Set to "none" if se is FALSE
#' @param dec Number of decimals to show
#' @param envir Environment to extract data from
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- logistic(titanic, "survived", c("pclass", "sex"), lev = "Yes")
#' predict(result, pred_cmd = "pclass = levels(pclass)")
#' logistic(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>%
#'   predict(pred_cmd = "sex = c('male','female')")
#' logistic(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>%
#'   predict(pred_data = titanic)
#' @seealso \code{\link{logistic}} to generate the result
#' @seealso \code{\link{summary.logistic}} to summarize results
#' @seealso \code{\link{plot.logistic}} to plot results
#' @seealso \code{\link{plot.model.predict}} to plot prediction output
#'
#' @export
predict.logistic <- function(object, pred_data = NULL, pred_cmd = "",
                             conf_lev = 0.95, se = TRUE, interval = "confidence",
                             dec = 3, envir = parent.frame(), ...) {
  if (is.character(object)) {
    return(object)
  }
  if (isTRUE(se)) {
    if (isTRUE(interval == "none")) {
      se <- FALSE
    } else if ("center" %in% object$check || "standardize" %in% object$check) {
      message("Standard error calculations not supported when coefficients are centered or standardized")
      se <- FALSE
      interval <- "none"
    }
  } else {
    interval <- "none"
  }

  ## ensure you have a name for the prediction dataset
  if (is.data.frame(pred_data)) {
    df_name <- deparse(substitute(pred_data))
  } else {
    df_name <- pred_data
  }

  pfun <- function(model, pred, se, conf_lev) {
    pred_val <-
      try(
        sshhr(
          if (se) {
            predict(model, pred, type = "link", se.fit = TRUE)
          } else {
            predict(model, pred, type = "response", se.fit = FALSE)
          }
        ),
        silent = TRUE
      )

    if (!inherits(pred_val, "try-error")) {
      if (se) {
        ## based on https://www.fromthebottomoftheheap.net/2017/05/01/glm-prediction-intervals-i/
        ilink <- family(model)$linkinv
        ci_perc <- ci_label(cl = conf_lev)
        pred_val <- data.frame(
          Prediction = ilink(pred_val[["fit"]]),
          ymax = ilink(pred_val[["fit"]] - qnorm(.5 + conf_lev / 2) * pred_val[["se.fit"]]),
          ymin = ilink(pred_val[["fit"]] + qnorm(.5 + conf_lev / 2) * pred_val[["se.fit"]]),
          stringsAsFactors = FALSE
        ) %>%
          set_colnames(c("Prediction", ci_perc[1], ci_perc[2]))
      } else {
        pred_val <- data.frame(pred_val, stringsAsFactors = FALSE) %>%
          select(1) %>%
          set_colnames("Prediction")
      }
    }
    pred_val
  }

  predict_model(object, pfun, "logistic.predict", pred_data, pred_cmd, conf_lev, se, dec, envir = envir) %>%
    set_attr("radiant_interval", interval) %>%
    set_attr("radiant_pred_data", df_name)
}

#' Print method for logistic.predict
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#'
#' @export
print.logistic.predict <- function(x, ..., n = 10) {
  print_predict_model(x, ..., n = n, header = "Logistic regression (GLM)")
}

#' Confidence interval for robust estimators
#'
#' @details Wrapper for confint with robust standard errors. See \url{https://stackoverflow.com/questions/3817182/vcovhc-and-confidence-interval/3820125#3820125}
#'
#' @param object A fitted model object
#' @param level The confidence level required
#' @param dist Distribution to use ("norm" or "t")
#' @param vcov Covariance matrix generated by, e.g., sandwich::vcovHC
#' @param ... Additional argument(s) for methods
#'
#' @importFrom sandwich vcovHC
#'
#' @export
confint_robust <- function(object, level = 0.95, dist = "norm", vcov = NULL, ...) {
  fac <- ((1 - level) / 2) %>%
    c(., 1 - .)

  cf <- coef(object)
  if (dist == "t") {
    fac <- qt(fac, df = nrow(object$model) - length(cf))
  } else {
    fac <- qnorm(fac)
  }
  if (is.null(vcov)) {
    vcov <- sandwich::vcovHC(object, type = "HC1")
  }
  ses <- sqrt(diag(vcov))
  cf + ses %o% fac
}

#' Calculate min and max before standardization
#'
#' @param dataset Data frame
#' @return Data frame min and max attributes
#'
#' @export
minmax <- function(dataset) {
  isNum <- sapply(dataset, is.numeric)
  if (sum(isNum) == 0) {
    return(dataset)
  }
  cn <- names(isNum)[isNum]

  mn <- summarise_at(dataset, .vars = cn, .funs = ~ min(., na.rm = TRUE))
  mx <- summarise_at(dataset, .vars = cn, .funs = ~ max(., na.rm = TRUE))

  list(min = mn, max = mx)
}

#' Write coefficient table for linear and logistic regression
#'
#' @details Write coefficients and importance scores to csv or or return as a data.frame
#'
#' @param object A fitted model object of class regress or logistic
#' @param file A character string naming a file. "" indicates output to the console
#' @param sort Sort table by variable importance
#' @param intercept Include the intercept in the output (TRUE or FALSE). TRUE is the default
#'
#' @examples
#'
#' regress(
#'   diamonds,
#'   rvar = "price", evar = c("carat", "clarity", "color", "x"),
#'   int = c("carat:clarity", "clarity:color", "I(x^2)"), check = "standardize"
#' ) %>%
#'   write.coeff(sort = TRUE) %>%
#'   format_df(dec = 3)
#'
#' logistic(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>%
#'   write.coeff(intercept = FALSE, sort = TRUE) %>%
#'   format_df(dec = 2)
#' @importFrom stats model.frame
#'
#' @export
write.coeff <- function(object, file = "", sort = FALSE, intercept = TRUE) {
  if (inherits(object, "regress")) {
    mod_class <- "regress"
  } else if (inherits(object, "logistic")) {
    mod_class <- "logistic"
  } else if (inherits(object, "mnl")) {
    mod_class <- "mnl"
  } else {
    "Object is not of class logistic, mnl, or regress" %T>%
      message %>%
      cat("\n\n", file = file)
    return(invisible())
  }

  has_int <- sum(nchar(object$int)) > 0
  check <- object$check

  ## calculating the mean and sd for each variable
  ## extract formula from http://stackoverflow.com/a/9694281/1974918
  frm <- formula(object$model$terms)
  coeff <- object$model$coeff
  dataset <- object$model$model
  cn <- colnames(dataset)
  wts <- object$wts

  if ("center" %in% check) {
    ms <- attr(object$model$model, "radiant_ms")
    if (!is.null(ms)) {
      icn <- intersect(cn, names(ms))
      dataset[icn] <- lapply(icn, function(var) dataset[[var]] + ms[[var]])
    }
  } else if ("standardize" %in% check) {
    ms <- attr(object$model$model, "radiant_ms")
    sds <- attr(object$model$model, "radiant_sds")
    if (!is.null(ms) && !is.null(sds)) {
      icn <- intersect(cn, names(ms))
      sf <- attr(object$model$model, "radiant_sf")
      sf <- ifelse(is.null(sf), 2, sf)
      dataset[icn] <- lapply(icn, function(var) dataset[[var]] * sf * sds[[var]] + ms[[var]])
    }
  }

  ## create the model.matrix
  mm <- model.matrix(frm, model.frame(frm, dataset))[, -1]

  ## removing columns where the corresponding coeff is missing
  cn <- intersect(colnames(mm), names(na.omit(coeff)))
  mm <- mm[, cn, drop = FALSE]

  ## generate summary statistics
  if (length(wts) == 0) {
    cms <- colMeans(mm, na.rm = TRUE)
    csds <- apply(mm, 2, sd, na.rm = TRUE)
    wts_mess <- " "
  } else {
    cms <- apply(mm, 2, weighted.mean, wts, na.rm = TRUE)
    csds <- apply(mm, 2, weighted.sd, wts, na.rm = TRUE)
    wts_mess <- " -- estimated with weights -- "
  }

  cmx <- apply(mm, 2, max, na.rm = TRUE)
  cmn <- apply(mm, 2, min, na.rm = TRUE)
  dummy <- apply(mm, 2, function(x) (sum(x == max(x)) + sum(x == min(x))) == length(x))

  if ("standardize" %in% check) {
    mess <- paste0("Standardized coefficients", wts_mess, "shown\n\n")
  } else {
    mess <- paste0("Non-standardized coefficients", wts_mess, "shown\n\n")
  }
  cat(mess, file = file)

  object <- object[["coeff"]]
  object$dummy <- c(0L, dummy)
  object$mean <- c(1L, cms)
  object$sd <- c(0L, csds)
  object$min <- c(1L, cmn)
  object$max <- c(1L, cmx)

  intc <- grepl("(Intercept)", object$label)

  if (mod_class == "logistic") {
    object$importance <- pmax(object$OR, 1 / object$OR)
    object$OR[intc] <- object$`OR%`[intc] <- 0
    if ("standardize" %in% check) {
      if (has_int) {
        object$OR_normal <- object$`OR%_normal` <- "-"
      } else {
        object$OR_normal <- exp(object$coefficient / (sf * object$sd))
        object$OR_normal[object$dummy == 1] <- object$OR[object$dummy == 1]
        object$`OR%_normal` <- with(object, ifelse(OR_normal < 1, -(1 - OR_normal), OR_normal - 1))
        object$OR_normal[intc] <- object$`OR%_normal`[intc] <- 0
      }
    }
  } else if (mod_class == "mnl") {
    object$importance <- pmax(object$RRR, 1 / object$RRR)
    object$RRR[intc] <- 0
  } else {
    object$importance <- abs(object$coefficient)
    # if ("standardize" %in% check) {
    #   if (has_int) {
    #     object$coeff_normal <- "-"
    #   } else {
    #     # need to also adjust for sd(Y)
    #     object$coeff_normal <- object$coefficient / (sf*object$sd)
    #     object$coeff_normal[object$dummy == 1] <- object$coefficient[object$dummy == 1]
    #     object$coeff_normal[intc] <- 0
    #   }
    # }
  }

  object$importance[intc] <- 0

  if (sort) {
    object[-1, ] <- arrange(object[-1, ], desc(.data$importance))
  }

  if (!intercept) {
    object <- slice(object, -1)
  } ## slice will ensure a tibble / data.frame is returned

  if (!is.empty(file)) {
    sshhr(write.table(object, sep = ",", append = TRUE, file = file, row.names = FALSE))
  } else {
    object
  }
}
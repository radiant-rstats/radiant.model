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
#' @param ci_type To use the profile-likelihood (rather than Wald) for confidence intervals use "profile". For datasets with more than 5,000 rows the Wald method will be used, unless "profile" is explicitly set
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list with all variables defined in logistic as an object of class logistic
#'
#' @examples
#' logistic(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>% summary()
#' logistic(titanic, "survived", c("pclass", "sex")) %>% str()
#'
#' @seealso \code{\link{summary.logistic}} to summarize the results
#' @seealso \code{\link{plot.logistic}} to plot the results
#' @seealso \code{\link{predict.logistic}} to generate predictions
#' @seealso \code{\link{plot.model.predict}} to plot prediction output
#'
#' @importFrom sandwich vcovHC
#'
#' @export
logistic <- function(
  dataset, rvar, evar, lev = "",
  int = "", wts = "None", check = "",
  ci_type, data_filter = ""
) {

  if (rvar %in% evar) {
    return("Response variable contained in the set of explanatory variables.\nPlease update model specification." %>%
      add_class("logistic"))
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

  if (missing(ci_type)) {
    ## Use profiling for smaller datasets
    if (nrow(na.omit(dataset)) < 5000) {
      ci_type <- "profile"
    } else {
      ci_type <- "default"
    }
  }

  if (!is_empty(wts)) {
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

  if (any(summarise_all(dataset, funs(does_vary)) == FALSE)) {
    return("One or more selected variables show no variation. Please select other variables." %>%
      add_class("logistic"))
  }

  rv <- dataset[[rvar]]
  if (lev == "") {
    if (is.factor(rv)) {
      lev <- levels(rv)[1]
    } else {
      lev <- as.character(rv) %>% as.factor() %>% levels() %>% .[1]
    }
  }

  ## transformation to TRUE/FALSE depending on the selected level (lev)
  dataset[[rvar]] <- dataset[[rvar]] == lev

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
    dataset <- scaledf(dataset, wts = wts)
  } else if ("center" %in% check) {
    dataset <- scaledf(dataset, scale = FALSE, wts = wts)
  }

  form_upper <- paste(rvar, "~", paste(vars, collapse = " + ")) %>% as.formula()
  form_lower <- paste(rvar, "~ 1") %>% as.formula()
  if ("stepwise" %in% check) check <- sub("stepwise", "stepwise-backward", check)
  if ("stepwise-backward" %in% check) {
    ## use k = 2 for AIC, use k = log(nrow(dataset)) for BIC
    model <- sshhr(glm(form_upper, weights = wts, family = binomial(link = "logit"), data = dataset)) %>%
      step(k = 2, scope = list(lower = form_lower), direction = "backward")

    ## adding full data even if all variables are not significant
    model$model <- dataset
  } else if ("stepwise-forward" %in% check) {
    model <- sshhr(glm(form_lower, weights = wts, family = binomial(link = "logit"), data = dataset)) %>%
      step(k = 2, scope = list(upper = form_upper), direction = "forward")

    ## adding full data even if all variables are not significant
    model$model <- dataset
  } else if ("stepwise-both" %in% check) {
    model <- sshhr(glm(form_lower, weights = wts, family = binomial(link = "logit"), data = dataset)) %>%
      step(k = 2, scope = list(lower = form_lower, upper = form_upper), direction = "both")

    ## adding full data even if all variables are not significant
    model$model <- dataset
  } else {
    model <- sshhr(glm(form_upper, weights = wts, family = binomial(link = "logit"), data = dataset))
  }

  ## needed for prediction if standardization or centering is used
  if ("standardize" %in% check || "center" %in% check) {
    attr(model$model, "ms") <- attr(dataset, "ms")
    attr(model$model, "sds") <- attr(dataset, "sds")
  }

  attr(model$model, "min") <- mmx[["min"]]
  attr(model$model, "max") <- mmx[["max"]]

  coeff <- tidy(model) %>% as.data.frame()
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
    coeff$z.value <- coef(model) / coeff$std.error
    coeff$p.value <- 2 * pnorm(abs(coeff$z.value), lower.tail = FALSE)
  }

  coeff$sig_star <- sig_stars(coeff$p.value) %>% format(justify = "left")
  coeff$OR <- exp(coeff$coefficient)
  coeff <- coeff[, c("label", "OR", "coefficient", "std.error", "z.value", "p.value", "sig_star")]

  ## remove elements no longer needed
  rm(dataset, hasLevs, form_lower, form_upper)

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
#' result <- logistic(titanic, "survived", "pclass", lev = "Yes")
#' summary(result, test_var = "pclass")
#' res <- logistic(titanic, "survived", c("pclass", "sex"), int = "pclass:sex", lev = "Yes")
#' summary(res, sum_check = c("vif", "confint", "odds"))
#' titanic %>% logistic("survived", c("pclass", "sex", "age"), lev = "Yes") %>% summary("vif")
#'
#' @seealso \code{\link{logistic}} to generate the results
#' @seealso \code{\link{plot.logistic}} to plot the results
#' @seealso \code{\link{predict.logistic}} to generate predictions
#' @seealso \code{\link{plot.model.predict}} to plot prediction output
#'
#' @importFrom car vif linearHypothesis
#'
#' @export
summary.logistic <- function(
  object, sum_check = "", conf_lev = .95,
  test_var = "", dec = 3, ...
) {

  if (is.character(object)) return(object)
  if (class(object$model)[1] != "glm") return(object)

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
  if (object$data_filter %>% gsub("\\s", "", .) != "") {
    cat("\nFilter               :", gsub("\\n", "", object$data_filter))
  }
  cat("\nResponse variable    :", object$rvar)
  cat("\nLevel                :", object$lev, "in", object$rvar)
  cat("\nExplanatory variables:", paste0(object$evar, collapse = ", "), "\n")
  if (length(object$wtsname) > 0) {
    cat("Weights used         :", object$wtsname, "\n")
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
  coeff[, 2:6] %<>% format_df(dec)
  coeff$p.value[p.small] <- "< .001"
  rename(coeff, `  ` = "label", ` ` = "sig_star") %>% {.$OR[1] <- ""; .} %>% print(row.names = FALSE)
  cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")

  logit_fit <- glance(object$model)

  ## pseudo R2 (likelihood ratio) - http://en.wikipedia.org/wiki/Logistic_Model
  logit_fit %<>% mutate(r2 = (null.deviance - deviance) / null.deviance) %>%
    round(dec)
  if (!is_empty(object$wts, "None") && (length(unique(object$wts)) > 2 || min(object$wts) >= 1)) {
    nobs <- sum(object$wts)
    logit_fit$BIC <- round(-2 * logit_fit$logLik + ln(nobs) * with(logit_fit, 1 + df.null - df.residual), dec)
  } else {
    nobs <- logit_fit$df.null + 1
  }

  ## chi-squared test of overall model fit (p-value) - http://www.ats.ucla.edu/stat/r/dae/logit.htm
  chi_pval <- with(object$model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
  chi_pval %<>% {if (. < .001) "< .001" else round(., dec)}

  cat("\nPseudo R-squared:", logit_fit$r2)
  cat(paste0("\nLog-likelihood: ", logit_fit$logLik, ", AIC: ", logit_fit$AIC, ", BIC: ", logit_fit$BIC))
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
          {if (is.null(dim(.))) . else .[, "GVIF^(1/(2*Df))"]} %>% ## needed when factors are included
          data.frame(VIF = ., Rsq = 1 - 1 / ., stringsAsFactors = FALSE) %>%
          .[order(.$VIF, decreasing = TRUE), ] %>% ## not using arrange to keep rownames
          round(dec) %>%
          {if (nrow(.) < 8) t(.) else .} %>%
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
        cbind(select(object$coeff, 3), .)

      if ("confint" %in% sum_check) {
        ci_tab %T>%
          {.$`+/-` <- (.$High - .$coefficient)} %>%
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

  if (!is_empty(test_var)) {
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
        matcher <- function(vl, cn)
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
#' @param plots Plots to produce for the specified GLM model. Use "" to avoid showing any plots (default). "dist" shows histograms (or frequency bar plots) of all variables in the model. "scatter" shows scatter plots (or box plots for factors) for the response variable with each explanatory variable. "coef" provides a coefficient plot
#' @param conf_lev Confidence level to use for coefficient and odds confidence intervals (.95 is the default)
#' @param intercept Include the intercept in the coefficient plot (TRUE or FALSE). FALSE is the default
#' @param nrobs Number of data points to show in scatter plots (-1 for all)
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- logistic(titanic, "survived", c("pclass", "sex"), lev = "Yes")
#' plot(result, plots = "coef")
#'
#' @seealso \code{\link{logistic}} to generate results
#' @seealso \code{\link{plot.logistic}} to plot results
#' @seealso \code{\link{predict.logistic}} to generate predictions
#' @seealso \code{\link{plot.model.predict}} to plot prediction output
#'
#' @export
plot.logistic <- function(
  x, plots = "", conf_lev = .95,
  intercept = FALSE, nrobs = -1,
  shiny = FALSE, custom = FALSE, ...
) {

  if (is.character(x)) return(x)
  if (class(x$model)[1] != "glm") return(x)
  if (is_empty(plots[1])) {
    return("Please select a logistic regression plot from the drop-down menu")
  }

  if ("(weights)" %in% colnames(x$model$model) &&
    min(x$model$model[["(weights)"]]) == 0) {
    model <- x$model$model
  } else {
    ## fortify chokes when a weight variable has 0s
    model <- ggplot2::fortify(x$model)
  }

  model$.fitted <- predict(x$model, type = "response")

  ## adjustment in case max > 1 (e.g., values are 1 and 2)
  model$.actual <- as_numeric(x$rv) %>% {. - max(.) + 1}

  rvar <- x$rvar
  evar <- x$evar
  vars <- c(x$rvar, x$evar)
  nrCol <- 2
  plot_list <- list()

  ## use orginal data rather than the logical used for estimation
  model[[rvar]] <- x$rv

  if ("dist" %in% plots) {
    for (i in vars)
      plot_list[[paste("dist_", i)]] <- select_at(model, .vars = i) %>%
        visualize(xvar = i, bins = 10, custom = TRUE)
  }

  if ("coef" %in% plots) {
    if (nrow(x$coeff) == 1 && !intercept) return("** Model contains only an intercept **")

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

    plot_list[["coef"]] <- cnfint(x$model, level = conf_lev, vcov = x$vcov) %>%
      exp(.) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      na.omit() %>%
      set_colnames(c("Low", "High")) %>%
      cbind(select(x$coeff, 2), .) %>%
      set_rownames(x$coeff$label) %>%
      {if (!intercept) .[-1, ] else .} %>%
      mutate(variable = rownames(.)) %>%
      ggplot() +
        geom_pointrange(aes_string(x = "variable", y = "OR", ymin = "Low", ymax = "High")) +
        geom_hline(yintercept = 1, linetype = "dotdash", color = "blue") +
        labs(y = yl, x = "") +
        ## can't use coord_trans together with coord_flip
        ## http://stackoverflow.com/a/26185278/1974918
        # scale_x_discrete(limits = {if (intercept) rev(x$coeff$`  `) else rev(x$coeff$`  `[-1])}) +
        scale_x_discrete(limits = {if (intercept) rev(x$coeff$label) else rev(x$coeff$label[-1])}) +
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
      if ("factor" %in% class(model[, i])) {
        plot_list[[paste0("scatter_", i)]] <- ggplot(model, aes_string(x = i, fill = rvar)) +
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
      geom_line(data = df, aes_string(y = "Probability"), color = "blue", size = 1) + ylim(0, 1) +
      labs(title = "Actual vs Fitted values (binned)", x = "Predicted probability bins", y = "Probability")
  }

  if ("correlations" %in% plots) {
    return(radiant.basics:::plot.correlation(select_at(model, .vars = vars), nrobs = nrobs))
  }

  if (custom) {
    if (length(plot_list) == 1) {
      return(plot_list[[1]])
    } else {
      return(plot_list)
    }
  }

  if (length(plot_list) > 0) {
    sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = nrCol)) %>% {
      if (shiny) . else print(.)
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
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- logistic(titanic, "survived", c("pclass", "sex"), lev = "Yes")
#'   predict(result, pred_cmd = "pclass = levels(pclass)")
#' logistic(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>%
#'   predict(pred_cmd = "sex = c('male','female')")
#' logistic(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>%
#'   predict(pred_data = titanic)
#'
#' @seealso \code{\link{logistic}} to generate the result
#' @seealso \code{\link{summary.logistic}} to summarize results
#' @seealso \code{\link{plot.logistic}} to plot results
#' @seealso \code{\link{plot.model.predict}} to plot prediction output
#'
#' @export
predict.logistic <- function(
  object, pred_data = NULL, pred_cmd = "",
  conf_lev = 0.95, se = TRUE, interval = "confidence",
  dec = 3, ...
) {

  if (is.character(object)) return(object)
  if (isTRUE(se)) {
    if (isTRUE(interval == "none")) {
      se <- FALSE
    } else if ("center" %in% object$check || "standardize" %in% object$check) {
      message("Standard error calculations not supported when coefficients are centered or standardized")
      se <- FALSE; interval <- "none"
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
          ymax = ilink(pred_val[["fit"]] - qnorm(.5 + conf_lev/2) * pred_val[["se.fit"]]),
          ymin = ilink(pred_val[["fit"]] + qnorm(.5 + conf_lev/2) * pred_val[["se.fit"]]),
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

  predict_model(object, pfun, "logistic.predict", pred_data, pred_cmd, conf_lev, se, dec) %>%
    set_attr("interval", interval) %>%
    set_attr("pred_data", df_name)
}

#' Print method for logistic.predict
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#'
#' @export
print.logistic.predict <- function(x, ..., n = 10)
  print_predict_model(x, ..., n = n, header = "Logistic regression (GLM)")

#' Confidence interval for robust estimators
#'
#' @details Wrapper for confint with robust standard errors. See \url{http://stackoverflow.com/a/3820125/1974918}
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
confint_robust <- function(
  object, level = 0.95, dist = "norm", vcov = NULL, ...
) {
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
  if (sum(isNum) == 0) return(dataset)
  cn <- names(isNum)[isNum]

  mn <- summarise_at(dataset, .vars = cn, .funs = funs(min(., na.rm = TRUE)))
  mx <- summarise_at(dataset, .vars = cn, .funs = funs(max(., na.rm = TRUE)))

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
#' regress(diamonds, rvar = "price", evar = c("carat", "clarity", "x"), check = "standardize") %>%
#'   write.coeff(sort = TRUE) %>%
#'   format_df(dec = 3)
#'
#' @export
write.coeff <- function(
  object, file = "", sort = FALSE, intercept = TRUE
) {
  if ("regress" %in% class(object)) {
    mod_class <- "regress"
  } else if ("logistic" %in% class(object)) {
    mod_class <- "logistic"
  } else {
    "Object is not of class logistic or regress" %T>%
      message %>%
      cat("\n\n", file = file)
    return(invisible())
  }

  ## calculating the mean and sd for each variable
  ## extract formula from http://stackoverflow.com/a/9694281/1974918
  frm <- formula(object$model$terms)
  mm <- model.matrix(frm, object$model$model)
  cms <- colMeans(mm, na.rm = TRUE)[-1]
  csds <- apply(mm, 2, sd, na.rm = TRUE)[-1]
  cmn <- cms * 0
  cmx <- cmn + 1
  mn <- attr(object$model$model, "min")
  nms <- intersect(names(cms), names(mn))
  dummy <- cmx
  dummy[nms] <- 0
  cmn[nms] <- mn[nms]
  mx <- attr(object$model$model, "max")
  cmx[nms] <- mx[nms]
  rm(mm)

  if ("standardize" %in% object$check || "center" %in% object$check) {
    ms <- attr(object$model$model, "ms")
    cms[nms] <- ms[nms]
    sds <- attr(object$model$model, "sds")
    if (!is_empty(sds)) csds[nms] <- sds[nms]
    cat("Standardized coefficients selected\n\n", file = file)
  } else {
    cat("Standardized coefficients not selected\n\n", file = file)
  }

  object <- object[["coeff"]]
  object$OR[1] <- 0
  object$dummy <- c(0L, dummy)
  object$mean <- cms %>% unlist() %>% c(1L, .)
  object$sd <- csds %>% unlist() %>% c(0L, .)
  object$min <- cmn %>% unlist() %>% c(1L, .)
  object$max <- cmx %>% unlist() %>% c(1L, .)

  if (mod_class == "logistic") {
    object$importance <- pmax(object$OR, 1 / object$OR)
    object$OR[1] <- 0
  } else {
    object$importance <- abs(object$coeff)
    object$OR <- NULL
  }

  object$importance[1] <- 0

  if (sort)
    object[-1, ] <- arrange(object[-1, ], desc(.data$importance))

  if (!intercept)
    object <- slice(object, -1)   ## slice will ensure a tibble / data.frame is returned

  if (!is_empty(file)) {
    sshhr(write.table(object, sep = ",", append = TRUE, file = file, row.names = FALSE))
  } else {
    object
  }
}

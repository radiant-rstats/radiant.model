#' Linear regression using OLS
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param rvar The response variable in the regression
#' @param evar Explanatory variables in the regression
#' @param int Interaction terms to include in the model
#' @param check Use "standardize" to see standardized coefficient estimates. Use "stepwise-backward" (or "stepwise-forward", or "stepwise-both") to apply step-wise selection of variables in estimation. Add "robust" for robust estimation of standard errors (HC1)
#' @param form Optional formula to use instead of rvar, evar, and int
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param arr Expression to arrange (sort) the data on (e.g., "color, desc(price)")
#' @param rows Rows to select from the specified dataset
#' @param envir Environment to extract data from
#'
#' @return A list of all variables used in the regress function as an object of class regress
#'
#' @examples
#' regress(diamonds, "price", c("carat", "clarity"), check = "standardize") %>% summary()
#' regress(diamonds, "price", c("carat", "clarity")) %>% str()
#'
#' @seealso \code{\link{summary.regress}} to summarize results
#' @seealso \code{\link{plot.regress}} to plot results
#' @seealso \code{\link{predict.regress}} to generate predictions
#'
#' @importFrom sandwich vcovHC
#'
#' @export
regress <- function(dataset, rvar, evar, int = "", check = "",
                    form, data_filter = "", arr = "", rows = NULL, envir = parent.frame()) {
  if (!missing(form)) {
    form <- as.formula(format(form))
    vars <- all.vars(form)
    rvar <- vars[1]
    evar <- vars[-1]
  }

  if (rvar %in% evar) {
    return("Response variable contained in the set of explanatory variables.\nPlease update model specification." %>%
      add_class("regress"))
  }

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  if (any(evar == ".")) {
    dataset <- get_data(dataset, "", filt = data_filter, arr = arr, rows = rows, envir = envir)
    evar <- setdiff(colnames(dataset), rvar)
  } else {
    dataset <- get_data(dataset, c(rvar, evar), filt = data_filter, arr = arr, rows = rows, envir = envir)
  }

  not_vary <- colnames(dataset)[summarise_all(dataset, does_vary) == FALSE]
  if (length(not_vary) > 0) {
    return(paste0("The following variable(s) show no variation. Please select other variables.\n\n** ", paste0(not_vary, collapse = ", "), " **") %>%
      add_class("regress"))
  }

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
  isNum <- sapply(dataset, is.numeric)
  if (sum(isNum) > 0) {
    if ("standardize" %in% check) {
      dataset <- scale_df(dataset)
    } else if ("center" %in% check) {
      dataset <- scale_df(dataset, scale = FALSE)
    }
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
    model <- lm(form_upper, data = dataset) %>%
      step(k = 2, scope = list(lower = form_lower), direction = "backward")
  } else if ("stepwise-forward" %in% check) {
    model <- lm(form_lower, data = dataset) %>%
      step(k = 2, scope = list(upper = form_upper), direction = "forward")
  } else if ("stepwise-both" %in% check) {
    model <- lm(form_lower, data = dataset) %>%
      step(k = 2, scope = list(lower = form_lower, upper = form_upper), direction = "both")
  } else {
    model <- lm(form_upper, data = dataset)
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
  colnames(coeff) <- c("  ", "coefficient", "std.error", "t.value", "p.value")

  if ("robust" %in% check) {
    vcov <- sandwich::vcovHC(model, type = "HC1")
    coeff$std.error <- sqrt(diag(vcov))
    coeff$t.value <- coeff$coefficient / coeff$std.error
    coeff$p.value <- 2 * pt(abs(coeff$t.value), df = nrow(dataset) - nrow(coeff), lower.tail = FALSE)
  }

  coeff$sig_star <- sig_stars(coeff$p.value) %>% format(justify = "left")
  colnames(coeff) <- c("label", "coefficient", "std.error", "t.value", "p.value", "sig_star")
  hasLevs <- sapply(select(dataset, -1), function(x) is.factor(x) || is.logical(x) || is.character(x))
  if (sum(hasLevs) > 0) {
    for (i in names(hasLevs[hasLevs])) {
      coeff$label %<>% gsub(paste0("^", i), paste0(i, "|"), .) %>%
        gsub(paste0(":", i), paste0(":", i, "|"), .)
    }
    rm(i)
  }

  ## remove elements no longer needed
  rm(dataset, hasLevs, form_lower, form_upper, isNum, envir)

  # added for consistency with other model types
  type <- "regression"

  as.list(environment()) %>% add_class(c("regress", "model"))
}

#' Summary method for the regress function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{regress}}
#' @param sum_check Optional output. "rsme" to show the root mean squared error and the standard deviation of the residuals. "sumsquares" to show the sum of squares table. "vif" to show multicollinearity diagnostics. "confint" to show coefficient confidence interval estimates.
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param test_var Variables to evaluate in model comparison (i.e., a competing models F-test)
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- regress(diamonds, "price", c("carat", "clarity"))
#' summary(result, sum_check = c("rmse", "sumsquares", "vif", "confint"), test_var = "clarity")
#' result <- regress(ideal, "y", c("x1", "x2"))
#' summary(result, test_var = "x2")
#' ideal %>%
#'   regress("y", "x1:x3") %>%
#'   summary()
#'
#' @seealso \code{\link{regress}} to generate the results
#' @seealso \code{\link{plot.regress}} to plot results
#' @seealso \code{\link{predict.regress}} to generate predictions
#'
#' @importFrom car vif linearHypothesis
#'
#' @export
summary.regress <- function(object, sum_check = "", conf_lev = .95,
                            test_var = "", dec = 3, ...) {
  if (is.character(object)) {
    return(object)
  }
  if (class(object$model)[1] != "lm") {
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

  cat("Linear regression (OLS)\n")
  cat("Data     :", object$df_name, "\n")
  if (!is.empty(object$data_filter)) {
    cat("Filter   :", gsub("\\n", "", object$data_filter), "\n")
  }
  if (!is.empty(object$arr)) {
    cat("Arrange  :", gsub("\\n", "", object$arr), "\n")
  }
  if (!is.empty(object$rows)) {
    cat("Slice    :", gsub("\\n", "", object$rows), "\n")
  }
  cat("Response variable    :", object$rvar, "\n")
  cat("Explanatory variables:", paste0(object$evar, collapse = ", "), "\n")
  expl_var <- if (length(object$evar) == 1) object$evar else "x"
  cat(paste0("Null hyp.: the effect of ", expl_var, " on ", object$rvar, " is zero\n"))
  cat(paste0("Alt. hyp.: the effect of ", expl_var, " on ", object$rvar, " is not zero\n"))
  if ("standardize" %in% object$check) {
    cat("**Standardized coefficients shown (2 X SD)**\n")
  } else if ("center" %in% object$check) {
    cat("**Centered coefficients shown (x - mean(x))**\n")
  }
  if ("robust" %in% object$check) {
    cat("**Robust standard errors used**\n")
  }

  coeff <- object$coeff
  coeff$label %<>% format(justify = "left")
  cat("\n")
  if (all(object$coeff$p.value == "NaN")) {
    coeff[, 2] %<>% (function(x) sprintf(paste0("%.", dec, "f"), x))
    print(coeff[, 1:2], row.names = FALSE)
    cat("\nInsufficient variation in explanatory variable(s) to report additional statistics")
    return()
  } else {
    p.small <- coeff$p.value < .001
    coeff[, 2:5] %<>% format_df(dec)
    coeff$p.value[p.small] <- "< .001"
    print(dplyr::rename(coeff, `  ` = "label", ` ` = "sig_star"), row.names = FALSE)
  }

  if (nrow(object$model$model) <= (length(object$evar) + 1)) {
    return("\nInsufficient observations to estimate model")
  }

  reg_fit <- glance(object$model) %>% round(dec)
  cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  cat("R-squared:", paste0(reg_fit$r.squared, ", "), "Adjusted R-squared:", reg_fit$adj.r.squared, "\n")

  ## if stepwise returns only an intercept
  if (nrow(coeff) == 1) {
    return("\nModel contains only an intercept. No additional output shown")
  }

  if (reg_fit["p.value"] < .001) reg_fit["p.value"] <- "< .001"
  cat("F-statistic:", reg_fit$statistic, paste0("df(", reg_fit$df, ",", reg_fit$df.residual, "), p.value"), reg_fit$p.value)
  cat("\nNr obs:", format_nr(reg_fit$nobs, dec = 0), "\n\n")

  if (anyNA(object$model$coeff)) {
    cat("The set of explanatory variables exhibit perfect multicollinearity.\nOne or more variables were dropped from the estimation.\n")
  }

  if ("rmse" %in% sum_check) {
    mean(object$model$residuals^2, na.rm = TRUE) %>%
      sqrt(.) %>%
      round(dec) %>%
      cat("Prediction error (RMSE): ", ., "\n")
    cat("Residual st.dev   (RSD): ", reg_fit$sigma, "\n\n")
  }

  if ("sumsquares" %in% sum_check) {
    atab <- anova(object$model)
    nr_rows <- dim(atab)[1]
    df_reg <- sum(atab$Df[-nr_rows])
    df_err <- sum(atab$Df[nr_rows])
    df_tot <- df_reg + df_err
    ss_reg <- sum(atab$`Sum Sq`[-nr_rows])
    ss_err <- sum(atab$`Sum Sq`[nr_rows])
    ss_tot <- ss_reg + ss_err
    ss_tab <- data.frame(matrix(nrow = 3, ncol = 2), stringsAsFactors = FALSE)
    rownames(ss_tab) <- c("Regression", "Error", "Total")
    colnames(ss_tab) <- c("df", "SS")
    ss_tab$df <- c(df_reg, df_err, df_tot) %>% format_nr(dec = 0)
    ss_tab$SS <- c(ss_reg, ss_err, ss_tot) %>% format_nr(dec = dec)
    cat("Sum of squares:\n")
    format(ss_tab, scientific = FALSE) %>% print()
    cat("\n")
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
          data.frame("VIF" = ., "Rsq" = 1 - 1 / ., stringsAsFactors = FALSE) %>%
          round(dec) %>%
          .[order(.$VIF, decreasing = T), ] %>%
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

  if ("confint" %in% sum_check) {
    if (anyNA(object$model$coeff)) {
      cat("Confidence intervals were not calculated.\n")
    } else {
      ci_perc <- ci_label(cl = conf_lev)

      if ("robust" %in% object$check) {
        cnfint <- radiant.model::confint_robust
      } else {
        cnfint <- confint
      }

      cnfint(object$model, level = conf_lev, dist = "t") %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        set_colnames(c("Low", "High")) %>%
        mutate(`+/-` = (High - Low) / 2) %>%
        mutate_all(~ sprintf(paste0("%.", dec, "f"), .)) %>%
        cbind(coeff[[2]], .) %>%
        set_rownames(object$coeff$label) %>%
        set_colnames(c("coefficient", ci_perc[1], ci_perc[2], "+/-")) %T>%
        print()
      cat("\n")
    }
  }

  if (!is.empty(test_var)) {
    if (any(grepl("stepwise", object$check))) {
      cat("Model comparisons are not conducted when Stepwise has been selected.\n")
    } else {
      sub_form <- paste(object$rvar, "~ 1")

      vars <- object$evar
      if (object$int != "" && length(vars) > 1) {
        ## updating test_var if needed
        test_var <- test_specs(test_var, object$int)
        vars <- c(vars, object$int)
      }

      not_selected <- base::setdiff(vars, test_var)
      if (length(not_selected) > 0) sub_form <- paste(". ~", paste(not_selected, collapse = " + "))
      sub_mod <- update(object$model, sub_form, data = object$model$model) %>%
        anova(object$model, test = "F")

      if (sub_mod[, "Pr(>F)"][2] %>% is.na()) {
        return(cat(""))
      }

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

      test_heading <- attr(sub_mod, "heading")[2]

      if ("robust" %in% object$check) {
        ## http://stats.stackexchange.com/a/132521/61693
        sub_mod <- car::linearHypothesis(
          object$model,
          matchCf(object$model$coef, test_var),
          vcov = object$vcov
        )
      }

      p.value <- sub_mod[, "Pr(>F)"][2] %>%
        (function(x) if (x < .001) "< .001" else round(x, dec))

      cat(test_heading)
      object$model$model[, 1] %>%
        (function(x) sum((x - mean(x))^2)) %>%
        (function(x) 1 - (sub_mod$RSS / x)) %>%
        round(dec) %>%
        cat("\nR-squared, Model 1 vs 2:", .)
      cat("\nF-statistic:", sub_mod$F[2] %>% round(dec), paste0("df(", sub_mod$Res.Df[1] - sub_mod$Res.Df[2], ",", sub_mod$Res.Df[2], "), p.value ", p.value))
    }
  }
}

#' Prediction Plots
#'
#' @details Faster, but less robust, alternative for PDP plots. Variable
#' values not included in the prediction are set to either the mean or
#' the most common value (level)
#'
#' @param x Return value from a model
#' @param plot_list List used to store plots
#' @param incl Which variables to include in prediction plots
#' @param incl_int Which interactions to investigate in prediction plots
#' @param fix Set the desired limited on yhat or have it calculated automatically.
#'   Set to FALSE to have y-axis limits set by ggplot2 for each plot
#' @param hline Add a horizontal line at the average of the target variable. When set to FALSE
#'   no line is added. When set to a specific number, the horizontal line will be added at that value
#' @param nr Number of values to use to generate predictions for a numeric explanatory variable
#'
#' @importFrom radiant.data visualize
#' @importFrom rlang .data
#'
#' @export
pred_plot <- function(x, plot_list = list(), incl, incl_int, fix = TRUE, hline = TRUE, nr = 20) {
  min_max <- c(Inf, -Inf)
  calc_ylim <- function(lab, lst, min_max) {
    if (isTRUE(fix)) {
      vals <- lst[[lab]]
      c(min(min_max[1], min(vals)), max(min_max[2], max(vals)))
    } else if (length(fix) == 2) {
      fix
    } else {
      FALSE
    }
  }

  # needed to avoid rescaling during prediction
  x$check <- setdiff(x$check, c("center", "standardize"))

  mod_dat <- x$model$model # [, -1, drop = FALSE]
  pvar <- "Prediction"

  set_pred_name <- function(pred) {
    if (!pvar %in% colnames(pred)) {
      pname <- colnames(pred)[ncol(mod_dat) + 1]
      if (pname != pvar) {
        cn <- colnames(pred)
        cn[cn == pname] <- pvar
        colnames(pred) <- cn
      }
    }
    return(pred)
  }

  for (pn in incl) {
    df <- mod_dat[, pn, drop = FALSE]
    is_num <- sapply(df, is.numeric)
    if (is.numeric(df[[pn]])) {
      num_range <- df[[pn]] %>%
        (function(x) seq(min(x), max(x), length.out = nr)) %>%
        paste0(collapse = ", ")
      pred <- predict(x, pred_cmd = glue("{pn} = c({num_range})")) %>% set_pred_name()
    } else {
      pred <- predict(x, pred_cmd = glue("{pn} = levels({pn})")) %>% set_pred_name()
    }
    plot_list[[pn]] <- visualize(pred, xvar = pn, yvar = pvar, type = "line", custom = TRUE) + labs(y = NULL)
    min_max <- calc_ylim(pvar, pred, min_max)
  }

  for (pn_lab in incl_int) {
    iint <- strsplit(pn_lab, ":")[[1]]
    df <- mod_dat[, iint, drop = FALSE]
    is_num <- sapply(df, is.numeric)
    if (sum(is_num) == 2) {
      # 2 numeric variables
      cn <- colnames(df)
      num_range1 <- df[[cn[1]]] %>%
        (function(x) seq(min(x), max(x), length.out = nr)) %>%
        paste0(collapse = ", ")
      num_range2 <- df[[cn[2]]] %>%
        (function(x) seq(min(x), max(x), length.out = nr)) %>%
        paste0(collapse = ", ")
      pred <- predict(x, pred_cmd = glue("{cn[1]} = c({num_range1}), {cn[2]} = c({num_range2})")) %>% set_pred_name()
      plot_list[[paste0(pn_lab, "_tile")]] <- ggplot(pred, aes(x = .data[[cn[1]]], y = .data[[cn[2]]], fill = .data[[pvar]])) +
        geom_tile()
    } else if (sum(is_num) == 0) {
      # 2 categorical variables
      cn <- colnames(df)
      pred <- predict(x, pred_cmd = glue("{cn[1]} = levels({cn[1]}), {cn[2]} = levels({cn[2]})")) %>% set_pred_name()
      plot_list[[pn_lab]] <- visualize(
        pred,
        xvar = cn[1], yvar = pvar, type = "line", color = cn[2], custom = TRUE
      ) + labs(y = NULL)
      min_max <- calc_ylim(pvar, pred, min_max)
    } else if (sum(is_num) == 1) {
      # 1 categorical and one numeric variable
      cn <- colnames(df)
      cn_fct <- cn[!is_num]
      cn_num <- cn[is_num]
      num_range <- df[[cn_num[1]]] %>%
        (function(x) seq(min(x), max(x), length.out = 20)) %>%
        paste0(collapse = ", ")
      pred <- predict(x, pred_cmd = glue("{cn_num[1]} = c({num_range}), {cn_fct} = levels({cn_fct})")) %>% set_pred_name()
      plot_list[[pn_lab]] <- plot(pred, xvar = cn_num[1], yvar = pvar, color = cn_fct, custom = TRUE) + labs(y = NULL)
      min_max <- calc_ylim(pvar, pred, min_max)
    }
  }

  if (length(min_max) > 1) {
    for (pn_lab in intersect(c(incl, incl_int), names(plot_list))) {
      plot_list[[pn_lab]] <- plot_list[[pn_lab]] + ylim(min_max[1], min_max[2])
    }
  }

  if (isTRUE(hline)) hline <- mean(mod_dat[[1]])
  if (is.numeric(hline)) {
    for (pn_lab in intersect(c(incl, incl_int), names(plot_list))) {
      plot_list[[pn_lab]] <- plot_list[[pn_lab]] +
        geom_hline(yintercept = hline, lty = 2, linewidth = 0.25)
    }
  }

  return(plot_list)
}


#' Create Partial Dependence Plots
#'
#' @param x Return value from a model
#' @param plot_list List used to store plots
#' @param incl Which variables to include in PDP plots
#' @param incl_int Which interactions to investigate in PDP plots
#' @param fix Set the desired limited on yhat or have it calculated automatically.
#'   Set to FALSE to have y-axis limits set by ggplot2 for each plot
#' @param hline Add a horizontal line at the average of the target variable. When set to FALSE
#'   no line is added. When set to a specific number, the horizontal line will be added at that value
#'
#' @importFrom radiant.data visualize
#' @importFrom pdp partial
#' @importFrom ggplot2 autoplot
#'
#' @export
pdp_plot <- function(x, plot_list = list(), incl, incl_int, fix = TRUE, hline = TRUE) {
  pdp_list <- list()
  min_max <- c(Inf, -Inf)
  calc_ylim <- function(lab, lst, min_max) {
    if (isTRUE(fix)) {
      vals <- lst[[lab]]
      c(min(min_max[1], min(vals)), max(min_max[2], max(vals)))
    } else if (length(fix) == 2) {
      fix
    } else {
      FALSE
    }
  }

  if (length(incl_int) > 0) {
    incl_int <- strsplit(incl_int, ":")
  }
  incl <- c(incl, incl_int)
  for (pn in incl) {
    pn_lab <- paste0(pn, collapse = ":")
    pdp_list[[pn_lab]] <- pdp::partial(
      x$model,
      pred.var = pn,
      plot = FALSE,
      prob = x$type == "classification",
      train = x$model$model
    )
    if (length(pn) < 2 || sum(c(is.numeric(x$model$model[[pn[1]]]), is.numeric(x$model$model[[pn[2]]]))) < 2) {
      min_max <- calc_ylim("yhat", pdp_list[[pn_lab]], min_max)
    }
  }

  for (pn_lab in names(pdp_list)) {
    df <- pdp_list[[pn_lab]]
    is_num <- sapply(df, is.numeric)
    if (ncol(df) == 2) {
      plot_list[[pn_lab]] <- autoplot(pdp_list[[pn_lab]]) +
        labs(y = NULL)
    } else if (sum(is_num) == 3) {
      # 2 numeric variables
      plot_list[[paste0(pn_lab, "_tile")]] <- autoplot(pdp_list[[pn_lab]])
    } else if (sum(is_num) == 1) {
      # 2 categorical variables
      cn <- colnames(df)
      plot_list[[pn_lab]] <- visualize(
        df,
        xvar = cn[1], yvar = cn[3], type = "line", color = cn[2], custom = TRUE
      ) + labs(y = NULL)
    } else if (sum(is_num) == 2) {
      # 1 categorical and one numeric variable
      cn <- colnames(df)
      cn_fct <- cn[!is_num]
      cn_num <- cn[is_num]
      plot_list[[pn_lab]] <- visualize(
        df,
        xvar = cn_num[1], yvar = cn[3], type = "line", color = cn_fct, custom = TRUE
      ) + labs(y = NULL)
    }
  }

  to_augment <- names(plot_list) %>% (function(x) x[!grepl("_tile$", x)])

  if (length(min_max) > 1) {
    for (pn_lab in to_augment) {
      plot_list[[pn_lab]] <- plot_list[[pn_lab]] + ylim(min_max[1], min_max[2])
    }
  }

  if (isTRUE(hline)) hline <- mean(x$model$model[[1]])
  if (is.numeric(hline)) {
    for (pn_lab in to_augment) {
      plot_list[[pn_lab]] <- plot_list[[pn_lab]] +
        geom_hline(yintercept = hline, lty = 2, linewidth = 0.25)
    }
  }

  return(plot_list)
}

#' Plot method for the regress function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{regress}}
#' @param plots Regression plots to produce for the specified regression model. Enter "" to avoid showing any plots (default). "dist" to shows histograms (or frequency bar plots) of all variables in the model. "correlations" for a visual representation of the correlation matrix selected variables. "scatter" to show scatter plots (or box plots for factors) for the response variable with each explanatory variable. "dashboard" for a series of six plots that can be used to evaluate model fit visually. "resid_pred" to plot the explanatory variables against the model residuals. "coef" for a coefficient plot with adjustable confidence intervals and "influence" to show (potentially) influential observations
#' @param lines Optional lines to include in the select plot. "line" to include a line through a scatter plot. "loess" to include a polynomial regression fit line. To include both use c("line", "loess")
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param intercept Include the intercept in the coefficient plot (TRUE, FALSE). FALSE is the default
#' @param incl Which variables to include in a coefficient plot or PDP plot
#' @param excl Which variables to exclude in a coefficient plot
#' @param incl_int Which interactions to investigate in PDP plots
#' @param nrobs Number of data points to show in scatter plots (-1 for all)
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{https://ggplot2.tidyverse.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- regress(diamonds, "price", c("carat", "clarity"))
#' plot(result, plots = "coef", conf_lev = .99, intercept = TRUE)
#' \dontrun{
#' plot(result, plots = "dist")
#' plot(result, plots = "scatter", lines = c("line", "loess"))
#' plot(result, plots = "resid_pred", lines = "line")
#' plot(result, plots = "dashboard", lines = c("line", "loess"))
#' }
#' @seealso \code{\link{regress}} to generate the results
#' @seealso \code{\link{summary.regress}} to summarize results
#' @seealso \code{\link{predict.regress}} to generate predictions
#'
#' @importFrom dplyr sample_n
#' @importFrom ggrepel geom_text_repel
#' @importFrom broom augment
#' @importFrom rlang .data
#'
#' @export
plot.regress <- function(x, plots = "", lines = "",
                         conf_lev = .95, intercept = FALSE,
                         incl = NULL, excl = NULL,
                         incl_int = NULL, nrobs = -1,
                         shiny = FALSE, custom = FALSE, ...) {
  if (is.character(x)) {
    return(x)
  }

  ## checking x size
  if (inherits(x$model, "lm")) {
    model <- broom::augment(x$model)
  } else if (inherits(x, "nn") || inherits(x, "rforest") || inherits(x, "gbt") || inherits(x, "crtree")) {
    model <- x$model$model
    model$pred <- predict(x, x$model$model)$Prediction
    model <- lm(formula(paste0(x$rvar, " ~ ", "pred")), data = model) %>%
      broom::augment()
  } else {
    return(x)
  }

  rvar <- x$rvar
  evar <- intersect(x$evar, colnames(model))
  vars <- c(rvar, evar)

  flines <- sub("loess", "", lines) %>% sub("line", "", .)
  # not clear why this was needed in the first place
  # nlines <- sub("jitter", "", lines)

  if (any(plots %in% c("dashboard", "scatter", "resid_pred")) && !is.empty(nrobs)) {
    nrobs <- as.integer(nrobs)
    if (nrobs > 0 && nrobs < nrow(model)) {
      model <- sample_n(model, nrobs, replace = FALSE)
    }
  }

  nrCol <- 2
  plot_list <- list()
  if ("dashboard" %in% plots) {
    plot_list[["dash1"]] <-
      visualize(model, xvar = ".fitted", yvar = rvar, type = "scatter", custom = TRUE) +
      labs(title = "Actual vs Fitted values", x = "Fitted", y = "Actual")

    plot_list[["dash2"]] <-
      visualize(model, xvar = ".fitted", yvar = ".resid", type = "scatter", custom = TRUE) +
      labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals")

    plot_list[["dash3"]] <- ggplot(model, aes(y = .resid, x = seq_along(.resid))) +
      geom_line() +
      labs(title = "Residuals vs Row order", x = "Row order", y = "Residuals")

    plot_list[["dash4"]] <- ggplot(model, aes(sample = .data$.std.resid)) +
      stat_qq(alpha = 0.5) +
      labs(title = "Normal Q-Q", x = "Theoretical quantiles", y = "Standardized residuals")

    plot_list[["dash5"]] <-
      visualize(model, xvar = ".resid", custom = TRUE) +
      labs(title = "Histogram of residuals", x = "Residuals")

    plot_list[["dash6"]] <- ggplot(model, aes(x = .data$.resid)) +
      geom_density(alpha = 0.3, fill = "green") +
      stat_function(fun = dnorm, args = list(mean = mean(model$.resid), sd = sd(model$.resid)), color = "blue") +
      labs(title = "Residuals vs Normal density", x = "Residuals", y = "") +
      theme(axis.text.y = element_blank())

    if ("loess" %in% lines) {
      for (i in paste0("dash", 1:3)) plot_list[[i]] <- plot_list[[i]] + sshhr(geom_smooth(method = "loess", size = .75, linetype = "dotdash"))
    }

    if ("line" %in% lines) {
      for (i in paste0("dash", c(1, 4))) {
        plot_list[[i]] <- plot_list[[i]] + geom_abline(linetype = "dotdash")
      }
      for (i in paste0("dash", 2:3)) {
        plot_list[[i]] <- plot_list[[i]] + sshhr(geom_smooth(method = "lm", se = FALSE, size = .75, linetype = "dotdash", color = "black"))
      }
    }
  }

  if ("dist" %in% plots) {
    for (i in vars) {
      plot_list[[paste0("dist", i)]] <- select_at(model, .vars = i) %>%
        visualize(xvar = i, bins = 10, custom = TRUE)
    }
  }

  if ("scatter" %in% plots) {
    for (i in evar) {
      if ("factor" %in% class(model[, i])) {
        plot_list[[paste0("scatter", i)]] <- select_at(model, .vars = c(i, rvar)) %>%
          visualize(xvar = i, yvar = rvar, type = "scatter", check = flines, alpha = 0.2, custom = TRUE)
      } else {
        plot_list[[paste0("scatter", i)]] <- select_at(model, .vars = c(i, rvar)) %>%
          visualize(xvar = i, yvar = rvar, type = "scatter", check = lines, custom = TRUE)
      }
    }
  }

  if ("resid_pred" %in% plots) {
    for (i in evar) {
      if ("factor" %in% class(model[, i])) {
        plot_list[[paste0("resid_", i)]] <- select_at(model, .vars = c(i, ".resid")) %>%
          visualize(xvar = i, yvar = ".resid", type = "scatter", check = flines, alpha = 0.2, custom = TRUE) +
          labs(y = "residuals")
      } else {
        plot_list[[paste0("resid_", i)]] <- select_at(model, .vars = c(i, ".resid")) %>%
          visualize(xvar = i, yvar = ".resid", type = "scatter", check = lines, custom = TRUE) +
          labs(y = "residuals")
      }
    }
  }

  if ("coef" %in% plots) {
    nrCol <- 1

    if (nrow(x$coeff) == 1 && !intercept) {
      return("** Model contains only an intercept **")
    }

    yl <- if ("standardize" %in% x$check) "Coefficient (standardized)" else "Coefficient"

    if ("robust" %in% x$check) {
      cnfint <- radiant.model::confint_robust
    } else {
      cnfint <- confint
    }

    coef_df <- cnfint(x$model, level = conf_lev, dist = "t") %>%
      data.frame(stringsAsFactors = FALSE) %>%
      na.omit() %>%
      set_colnames(c("Low", "High")) %>%
      cbind(select(x$coeff, 2), .) %>%
      set_rownames(x$coeff$label) %>%
      {
        if (!intercept) .[-1, ] else .
      } %>%
      mutate(variable = factor(rownames(.), levels = rownames(.)))

    if (length(incl) > 0) {
      incl <- paste0("^(", paste0(incl, "[|]*", collapse = "|"), ")")
      incl <- grepl(incl, coef_df$variable)
      if (isTRUE(intercept)) incl[1] <- TRUE
      coef_df <- coef_df[incl, ]
    }
    if (length(excl) > 0) {
      excl <- paste0("^(", paste0(excl, "[|]*", collapse = "|"), ")")
      if (isTRUE(intercept)) excl[1] <- FALSE
      coef_df <- coef_df[!excl, ]
    }
    coef_df <- droplevels(coef_df)

    plot_list[["coef"]] <- ggplot(coef_df) +
      geom_pointrange(aes(
        x = .data$variable, y = .data$coefficient,
        ymin = .data$Low, ymax = .data$High
      )) +
      geom_hline(yintercept = 0, linetype = "dotdash", color = "blue") +
      labs(y = yl, x = "") +
      scale_x_discrete(limits = rev(coef_df$variable)) +
      coord_flip() +
      theme(axis.text.y = element_text(hjust = 0))
  }

  if ("correlations" %in% plots) {
    if (length(evar) == 0) {
      message("Model contains only an intercept. Correlation plot cannot be generated")
    } else {
      return(radiant.basics:::plot.correlation(x$model$model, nrobs = nrobs))
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
    nrCol <- 2
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
          y = "Importance (R-square decrease)"
        ) +
        coord_flip() +
        theme(axis.text.y = element_text(hjust = 0))
    }
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

#' Predict method for the regress function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{regress}}
#' @param pred_data Provide the dataframe to generate predictions (e.g., diamonds). The dataset must contain all columns used in the estimation
#' @param pred_cmd Command used to generate data for prediction
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param se Logical that indicates if prediction standard errors should be calculated (default = FALSE)
#' @param interval Type of interval calculation ("confidence" or "prediction"). Set to "none" if se is FALSE
#' @param dec Number of decimals to show
#' @param envir Environment to extract data from
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- regress(diamonds, "price", c("carat", "clarity"))
#' predict(result, pred_cmd = "carat = 1:10")
#' predict(result, pred_cmd = "clarity = levels(clarity)")
#' result <- regress(diamonds, "price", c("carat", "clarity"), int = "carat:clarity")
#' predict(result, pred_data = diamonds) %>% head()
#'
#' @seealso \code{\link{regress}} to generate the result
#' @seealso \code{\link{summary.regress}} to summarize results
#' @seealso \code{\link{plot.regress}} to plot results
#'
#' @export
predict.regress <- function(object, pred_data = NULL, pred_cmd = "", conf_lev = 0.95,
                            se = TRUE, interval = "confidence", dec = 3,
                            envir = parent.frame(), ...) {
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

  if (is.data.frame(pred_data)) {
    df_name <- deparse(substitute(pred_data))
  } else {
    df_name <- pred_data
  }

  pfun <- function(model, pred, se, conf_lev) {
    pred_val <-
      try(
        sshhr(
          predict(
            model, pred,
            interval = ifelse(se, interval, "none"),
            level = conf_lev
          )
        ),
        silent = TRUE
      )

    if (!inherits(pred_val, "try-error")) {
      if (se) {
        pred_val %<>% data.frame(stringsAsFactors = FALSE) %>% mutate(diff = .[, 3] - .[, 1])
        ci_perc <- ci_label(cl = conf_lev)
        colnames(pred_val) <- c("Prediction", ci_perc[1], ci_perc[2], "+/-")
      } else {
        pred_val %<>% data.frame(stringsAsFactors = FALSE) %>% select(1)
        colnames(pred_val) <- "Prediction"
      }
    }

    pred_val
  }

  predict_model(object, pfun, "regress.predict", pred_data, pred_cmd, conf_lev, se, dec, envir = envir) %>%
    set_attr("radiant_interval", interval) %>%
    set_attr("radiant_pred_data", df_name)
}

#' Predict method for model functions
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{regress}}
#' @param pfun Function to use for prediction
#' @param mclass Model class to attach
#' @param pred_data Dataset to use for prediction
#' @param pred_cmd Command used to generate data for prediction (e.g., 'carat = 1:10')
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param se Logical that indicates if prediction standard errors should be calculated (default = FALSE)
#' @param dec Number of decimals to show
#' @param envir Environment to extract data from
#' @param ... further arguments passed to or from other methods
#'
#' @importFrom radiant.data set_attr
#'
#' @export
predict_model <- function(object, pfun, mclass, pred_data = NULL, pred_cmd = "",
                          conf_lev = 0.95, se = FALSE, dec = 3, envir = parent.frame(),
                          ...) {
  if (is.character(object)) {
    return(object)
  }
  if (is.empty(pred_data) && is.empty(pred_cmd)) {
    return("Please select data and/or specify a command to generate predictions.\nFor example, carat = seq(.5, 1.5, .1) would produce predictions for values\n of carat starting at .5, increasing to 1.5 in increments of .1. Make sure\nto press return after you finish entering the command.\n\nAlternatively, specify a dataset to generate predictions. You could create\nthis in a spread sheet and use the paste feature in Data > Manage to bring\nit into Radiant")
  }

  pred_type <- "cmd"
  vars <- object$evar
  if (!is.empty(pred_cmd) && is.empty(pred_data)) {
    dat <- object$model$model
    if ("center" %in% object$check) {
      ms <- attr(object$model$model, "radiant_ms")
      if (!is.null(ms)) {
        dat[names(ms)] <- lapply(names(ms), function(var) (dat[[var]] + ms[[var]]))
      }
    } else if ("standardize" %in% object$check) {
      ms <- attr(object$model$model, "radiant_ms")
      sds <- attr(object$model$model, "radiant_sds")
      if (!is.null(ms) && !is.null(sds)) {
        sf <- attr(object$model$model, "radiant_sf")
        sf <- ifelse(is.null(sf), 2, sf)
        dat[names(ms)] <- lapply(names(ms), function(var) (dat[[var]] * sf * sds[[var]] + ms[[var]]))
      }
    }

    pred_cmd %<>% paste0(., collapse = ";") %>%
      gsub("\"", "\'", .) %>%
      gsub(";\\s*$", "", .) %>%
      gsub(";", ",", .)

    pred <- try(eval(parse(text = paste0("with(dat, expand.grid(", pred_cmd, "))"))), silent = TRUE)
    if (inherits(pred, "try-error")) {
      return(paste0("The command entered did not generate valid data for prediction. The\nerror message was:\n\n", attr(pred, "condition")$message, "\n\nPlease try again. Examples are shown in the help file."))
    }

    vars <- vars[vars %in% colnames(dat)]
    dat <- select_at(dat, .vars = vars)

    if (!is.null(object$model$term)) {
      dat_classes <- attr(object$model$term, "dataClasses")[-1]
    } else {
      dat_classes <- get_class(dat)
    }

    ## weights and interaction terms mess-up data manipulation below so remove from
    dat_classes <- dat_classes[!grepl("(^\\(weights\\)$)|(^I\\(.+\\^[0-9]+\\)$)", names(dat_classes))]

    isFct <- dat_classes == "factor"
    isOther <- dat_classes %in% c("date", "other")
    isChar <- dat_classes %in% c("character")
    isLog <- dat_classes == "logical"
    isNum <- dat_classes %in% c("numeric", "integer", "ts", "period")

    # based on http://stackoverflow.com/questions/19982938/how-to-find-the-most-frequent-values-across-several-columns-containing-factors
    max_freq <- function(x) names(which.max(table(x)))
    max_ffreq <- function(x) as.factor(max_freq(x))
    max_lfreq <- function(x) ifelse(mean(x) > .5, TRUE, FALSE)

    plug_data <- data.frame(init___ = 1, stringsAsFactors = FALSE)
    if (sum(isNum) > 0) {
      plug_data %<>% bind_cols(., summarise_at(dat, .vars = vars[isNum], .funs = mean, na.rm = TRUE))
    }
    if (sum(isFct) > 0) {
      plug_data %<>% bind_cols(., summarise_at(dat, .vars = vars[isFct], .funs = max_ffreq))
    }
    if (sum(isChar) > 0) {
      plug_data %<>% bind_cols(., summarise_at(dat, .vars = vars[isChar], .funs = max_freq))
    }
    if (sum(isOther) > 0) {
      plug_data %<>% bind_cols(., summarise_at(dat, .vars = vars[isOther], .funs = max_freq) %>% mutate_all(as.Date, origin = "1970-1-1"))
    }
    if (sum(isLog) > 0) {
      plug_data %<>% bind_cols(., summarise_at(dat, .vars = vars[isLog], .funs = max_lfreq))
    }

    isPDO <- colnames(plug_data)[get_class(plug_data) %in% c("date", "other")]
    isPDO <- dplyr::intersect(isPDO, colnames(pred))
    if (length(isPDO) > 0) {
      pred %<>% mutate_at(.vars = isPDO, as.Date, origin = "1970-1-1")
    }

    if ((sum(isNum) + sum(isFct) + sum(isLog) + sum(isChar) + sum(isOther)) < length(vars)) {
      return("The model includes data-types that cannot be used for\nprediction at this point\n")
    } else {
      pred_names <- names(pred)
      if (sum(pred_names %in% names(plug_data)) < length(pred_names)) {
        vars_in <- pred_names %in% names(plug_data)
        return(paste0("The command entered contains variable names that are not in the model\nVariables in the model: ", paste0(vars, collapse = ", "), "\nVariables not available in prediction data: ", paste0(pred_names[!vars_in], collapse = ", ")))
      } else {
        plug_data[names(pred)] <- list(NULL)
        pred <- cbind(select(plug_data, -1), pred)
      }
    }
  } else {
    ## generate predictions for all observations in the dataset
    pred <- get_data(pred_data, filt = "", rows = NULL, na.rm = FALSE, envir = envir)
    pred_names <- colnames(pred)
    vars_in <- vars %in% pred_names
    ## keep all variables in the prediction data for the "customized" prediction
    if (!sum(vars_in) == length(vars)) {
      return(paste0("All variables in the model must also be in the prediction data\nVariables in the model: ", paste0(vars, collapse = ", "), "\nVariables not available in prediction data: ", paste0(vars[!vars_in], collapse = ", ")))
    }

    if (!is.empty(pred_cmd)) {
      pred_cmd %<>% paste0(., collapse = ";") %>%
        gsub("\"", "\'", .) %>%
        gsub("\\s+", " ", .) %>%
        gsub("<-", "=", .)

      cmd_vars <- strsplit(pred_cmd, ";\\s*")[[1]] %>%
        strsplit(., "=") %>%
        sapply("[", 1) %>%
        gsub("(^\\s+|\\s+$)", "", .)

      cmd_vars_in <- cmd_vars %in% vars
      if (sum(cmd_vars_in) < length(cmd_vars)) {
        return(paste0("The command entered contains variable names that are not in the model\nVariables in the model: ", paste0(vars, collapse = ", "), "\nVariables not available in prediction data: ", paste0(cmd_vars[!cmd_vars_in], collapse = ", ")))
      }

      dots <- rlang::parse_exprs(pred_cmd) %>%
        set_names(cmd_vars)

      ## any variables of type date?
      isPDO <- colnames(pred)[get_class(pred) %in% c("date", "other")]

      pred <- try(mutate(pred, !!!dots), silent = TRUE)
      if (inherits(pred, "try-error")) {
        return(paste0("The command entered did not generate valid data for prediction. The\nerror message was:\n\n", attr(pred, "condition")$message, "\n\nPlease try again. Examples are shown in the help file."))
      }

      if (length(isPDO) > 0) {
        pred %<>% mutate_at(.vars = isPDO, as.Date, origin = "1970-1-1")
      }

      pred_type <- "datacmd"
    } else {
      pred_type <- "data"
    }

    ## only keep the variables used in the model
    pred <- select_at(pred, .vars = vars) %>% na.omit()
  }

  if ("crtree" %in% class(object)) {
    ## also need to update data in crtree because
    ## logicals would get < 0.5 and >= 0.5 otherwise
    pred <- mutate_if(pred, is.logical, as.factor)
  }

  ## scale predictors if needed
  if ("center" %in% object$check || "standardize" %in% object$check) {
    attr(pred, "radiant_ms") <- attr(object$model$model, "radiant_ms")
    if ("standardize" %in% object$check) {
      scale <- TRUE
      attr(pred, "radiant_sds") <- attr(object$model$model, "radiant_sds")
      attr(pred, "radiant_sf") <- attr(object$model$model, "radiant_sf")
    } else {
      scale <- FALSE
    }
    pred_val <- scale_df(pred, center = TRUE, scale = scale, calc = FALSE) %>%
      pfun(object$model, ., se = se, conf_lev = conf_lev, ...)
  } else {
    ## generate predictions using the supplied function (pfun)
    pred_val <- pfun(object$model, pred, se = se, conf_lev = conf_lev, ...)
  }

  if (!inherits(pred_val, "try-error")) {
    ## scale rvar for regression models
    if ("center" %in% object$check) {
      ms <- attr(object$model$model, "radiant_ms")[[object$rvar]]
      if (!is.null(ms)) {
        pred_val[["Prediction"]] <- pred_val[["Prediction"]] + ms
      }
    } else if ("standardize" %in% object$check) {
      ms <- attr(object$model$model, "radiant_ms")[[object$rvar]]
      sds <- attr(object$model$model, "radiant_sds")[[object$rvar]]
      if (!is.null(ms) && !is.null(sds)) {
        sf <- attr(object$model$model, "radiant_sf")
        sf <- ifelse(is.null(sf), 2, sf)
        pred_val[["Prediction"]] <- pred_val[["Prediction"]] * sf * sds + ms
      }
    }

    pred <- data.frame(pred, pred_val, check.names = FALSE, stringsAsFactors = FALSE)
    vars <- colnames(pred)

    if (any(grepl("stepwise", object$check))) {
      ## show only the selected variables when printing predictions
      object$evar <- attr(terms(object$model), "variables") %>%
        as.character() %>%
        .[-c(1, 2)]
      vars <- c(object$evar, colnames(pred_val))
    }

    extra_args <- list(...)
    pred <- set_attr(pred, "radiant_df_name", object$df_name) %>%
      set_attr("radiant_data_filter", object$data_filter) %>%
      set_attr("radiant_arr", object$arr) %>%
      set_attr("radiant_rows", object$rows) %>%
      set_attr("radiant_rvar", object$rvar) %>%
      set_attr("radiant_lev", object$lev) %>%
      set_attr("radiant_evar", object$evar) %>%
      set_attr("radiant_wtsname", object$wtsname) %>%
      set_attr("radiant_vars", vars) %>%
      set_attr("radiant_dec", dec) %>%
      set_attr("radiant_pred_type", pred_type) %>%
      set_attr("radiant_pred_cmd", pred_cmd) %>%
      set_attr("radiant_extra_args", extra_args)

    return(add_class(pred, c(mclass, "model.predict")))
  } else {
    return(paste0("There was an error trying to generate predictions. The error\nmessage was:\n\n", attr(pred_val, "condition")$message, "\n\nPlease try again. Examples are shown in the help file."))
  }
}

#' Print method for the model prediction
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#' @param header Header line
#'
#' @export
print_predict_model <- function(x, ..., n = 10, header = "") {
  class(x) <- "data.frame"
  data_filter <- attr(x, "radiant_data_filter")
  arr <- attr(x, "radiant_arr")
  rows <- attr(x, "radiant_rows")
  vars <- attr(x, "radiant_vars")
  pred_type <- attr(x, "radiant_pred_type")
  pred_data <- attr(x, "radiant_pred_data")

  pred_cmd <- gsub("\\s*([\\=\\+\\*-])\\s*", " \\1 ", attr(x, "radiant_pred_cmd")) %>%
    gsub("(\\s*[;,]\\s*)", "\\1 ", .) %>%
    gsub("\\s+=\\s+=\\s+", " == ", .)

  cat(header)
  cat("\nData                 :", attr(x, "radiant_df_name"), "\n")
  if (!is.empty(data_filter)) {
    cat("Filter               :", gsub("\\n", "", data_filter), "\n")
  }
  if (!is.empty(arr)) {
    cat("Arrange              :", gsub("\\n", "", arr), "\n")
  }
  if (!is.empty(rows)) {
    cat("Slice                :", gsub("\\n", "", rows), "\n")
  }
  cat("Response variable    :", attr(x, "radiant_rvar"), "\n")
  if (!is.empty(attr(x, "radiant_lev"))) {
    cat("Level(s)             :", paste0(attr(x, "radiant_lev"), collapse = ", "), "in", attr(x, "radiant_rvar"), "\n")
  }
  cat("Explanatory variables:", paste0(attr(x, "radiant_evar"), collapse = ", "), "\n")
  if (!is.empty(attr(x, "radiant_wtsname"))) {
    cat("Weights used         :", attr(x, "radiant_wtsname"), "\n")
  }

  if (!is.empty(attr(x, "radiant_interval"), "none")) {
    cat("Interval             :", attr(x, "radiant_interval"), "\n")
  }

  if (pred_type == "cmd") {
    cat("Prediction command   :", pred_cmd, "\n")
  } else if (pred_type == "datacmd") {
    cat("Prediction dataset   :", pred_data, "\n")
    cat("Customize command    :", pred_cmd, "\n")
  } else {
    cat("Prediction dataset   :", pred_data, "\n")
  }

  extra_args <- attr(x, "radiant_extra_args")
  if (!is.empty(extra_args)) {
    extra_args <- deparse(extra_args) %>%
      sub("list\\(", "", .) %>%
      sub("\\)$", "", .)
    cat("Additional arguments :", extra_args, "\n")
  }

  if (n == -1) {
    cat("\n")
    format_df(x, attr(x, "radiant_dec")) %>%
      print(row.names = FALSE)
  } else {
    if (nrow(x) > n) {
      cat("Rows shown           :", n, "of", format_nr(nrow(x), dec = 0), "\n")
    }
    cat("\n")
    head(x, n) %>%
      format_df(attr(x, "radiant_dec")) %>%
      print(row.names = FALSE)
  }
}

#' Print method for predict.regress
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#'
#' @export
print.regress.predict <- function(x, ..., n = 10) {
  print_predict_model(x, ..., n = n, header = "Linear regression (OLS)")
}

#' Plot method for model.predict functions
#'
#' @param x Return value from predict functions (e.g., predict.regress)
#' @param xvar Variable to display along the X-axis of the plot
#' @param facet_row Create vertically arranged subplots for each level of the selected factor variable
#' @param facet_col Create horizontally arranged subplots for each level of the selected factor variable
#' @param color Adds color to a scatter plot to generate a heat map. For a line plot one line is created for each group and each is assigned a different color
#' @param conf_lev Confidence level to use for prediction intervals (.95 is the default)
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' regress(diamonds, "price", c("carat", "clarity")) %>%
#'   predict(pred_cmd = "carat = 1:10") %>%
#'   plot(xvar = "carat")
#' logistic(titanic, "survived", c("pclass", "sex", "age"), lev = "Yes") %>%
#'   predict(pred_cmd = c("pclass = levels(pclass)", "sex = levels(sex)", "age = 0:100")) %>%
#'   plot(xvar = "age", color = "sex", facet_col = "pclass")
#'
#' @seealso \code{\link{predict.regress}} to generate predictions
#' @seealso \code{\link{predict.logistic}} to generate predictions
#'
#' @importFrom rlang .data
#'
#' @export
plot.model.predict <- function(x, xvar = "", facet_row = ".",
                               facet_col = ".", color = "none",
                               conf_lev = .95, ...) {
  if (is.character(x)) {
    return(x)
  }
  ## should work with req in regress_ui but doesn't
  if (is.empty(xvar)) {
    return(invisible())
  }
  if (facet_col != "." && facet_row == facet_col) {
    return("The same variable cannot be used for both Facet row and Facet column")
  }

  cn <- colnames(x)
  pvars <- "Prediction"
  cnpred <- which(cn == pvars)
  if (length(cnpred) == 0) {
    return(invisible())
  }
  if (length(cn) > cnpred) {
    pvars <- c(pvars, "ymin", "ymax")
    cn[cnpred + 1] <- pvars[2]
    cn[cnpred + 2] <- pvars[3]
    colnames(x) <- cn
  }

  byvar <- NULL
  if (color != "none") byvar <- color
  if (facet_row != ".") {
    byvar <- if (is.null(byvar)) facet_row else unique(c(byvar, facet_row))
  }

  if (facet_col != ".") {
    byvar <- if (is.null(byvar)) facet_col else unique(c(byvar, facet_col))
  }

  tbv <- if (is.null(byvar)) xvar else c(xvar, byvar)

  if (any(!tbv %in% colnames(x))) {
    return("Some specified plotting variables are not in the model.\nPress the Estimate button to update results.")
  }

  tmp <- x %>%
    select_at(.vars = c(tbv, pvars)) %>%
    group_by_at(.vars = tbv) %>%
    summarise_all(mean)

  if (color == "none") {
    p <- ggplot(tmp, aes(x = .data[[xvar]], y = .data$Prediction))
  } else {
    p <- ggplot(tmp, aes(x = .data[[xvar]], y = .data$Prediction, color = .data[[color]], group = .data[[color]]))
  }

  if (length(pvars) >= 3) {
    if (is.factor(tmp[[xvar]]) || length(unique(tmp[[xvar]])) < 11) {
      p <- p + geom_pointrange(aes(ymin = .data$ymin, ymax = .data$ymax), size = .3)
    } else {
      p <- p + geom_ribbon(aes(ymin = .data$ymin, ymax = .data$ymax), fill = "grey70", color = NA, alpha = 0.5)
    }
  }

  ## needed now that geom_smooth no longer accepts ymin and ymax as arguments
  ## can't see line properly using geom_ribbon
  if (color == "none") {
    p <- p + geom_line(aes(group = 1))
  } else {
    p <- p + geom_line()
  }

  if (facet_row != "." || facet_col != ".") {
    facets <- ifelse(facet_row == ".", paste("~", facet_col), paste(facet_row, "~", facet_col))
    facet_fun <- ifelse(facet_row == ".", facet_wrap, facet_grid)
    p <- p + facet_fun(as.formula(facets))
  }

  sshhr(p)
}

#' Store predicted values generated in model functions
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param dataset Dataset to add predictions to
#' @param object Return value from model function
#' @param name Variable name(s) assigned to predicted values
#' @param ... Additional arguments
#'
#' @examples
#' regress(diamonds, rvar = "price", evar = c("carat", "cut")) %>%
#'   predict(pred_data = diamonds) %>%
#'   store(diamonds, ., name = c("pred", "pred_low", "pred_high")) %>%
#'   head()
#'
#' @export
store.model.predict <- function(dataset, object, name = "prediction", ...) {
  if (is.empty(name)) name <- "prediction"

  ## gsub needed because trailing/leading spaces may be added to the variable name
  ind <- which(colnames(object) == "Prediction")

  ## if se was calculated
  if (length(name) == 1) {
    name <- unlist(strsplit(name, "(\\s*,\\s*|\\s*;\\s*|\\s+)")) %>%
      gsub("\\s", "", .)
  }
  if (length(name) > 1) {
    name <- name[1:min(3, length(name))]
    ind_mult <- ind:(ind + length(name[-1]))
    df <- object[, ind_mult, drop = FALSE]
  } else {
    df <- object[, "Prediction", drop = FALSE]
  }

  vars <- colnames(object)[seq_len(ind - 1)]
  indr <- indexr(dataset, vars = vars, filt = "", rows = NULL, cmd = attr(object, "radiant_pred_cmd"))
  pred <- as.data.frame(matrix(NA, nrow = indr$nr, ncol = ncol(df)), stringsAsFactors = FALSE)
  # pred[indr$ind, ] <- as.vector(df) ## as.vector removes all attributes from df
  pred[indr$ind, ] <- df %>% mutate(across(everything(), as.vector))

  dataset[, name] <- pred
  dataset
}

#' Store residuals from a model
#'
#' @details The store method for objects of class "model". Adds model residuals to the dataset while handling missing values and filters. See \url{https://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param dataset Dataset to append residuals to
#' @param object Return value from a model function
#' @param name Variable name(s) assigned to model residuals
#' @param ... Additional arguments
#'
#' @examples
#' regress(diamonds, rvar = "price", evar = c("carat", "cut"), data_filter = "price > 1000") %>%
#'   store(diamonds, ., name = "resid") %>%
#'   head()
#'
#' @export
store.model <- function(dataset, object, name = "residuals", ...) {
  indr <- indexr(dataset, vars = c(object$rvar, object$evar), filt = object$data_filter, arr = object$arr, rows = object$rows)
  name <- unlist(strsplit(name, "(\\s*,\\s*|\\s*;\\s*|\\s+)")) %>%
    gsub("\\s", "", .)
  nr_res <- length(name)
  res <- matrix(rep(NA, indr$nr * nr_res), ncol = nr_res) %>%
    set_colnames(name) %>%
    as.data.frame(stringsAsFactors = FALSE)
  residuals <- object$model$residuals
  if (is.vector(residuals)) {
    res[indr$ind, name] <- residuals
  } else {
    res[indr$ind, name] <- residuals[, 1:nr_res]
  }
  dataset[, name] <- res
  dataset
}

#' Check if main effects for all interaction effects are included in the model
#'
#' @details If ':' is used to select a range evar is updated. See \url{https://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param ev List of explanatory variables provided to \code{\link{regress}} or \code{\link{logistic}}
#' @param cn Column names for all explanatory variables in the dataset
#' @param intv Interaction terms specified
#'
#' @return \code{vars} is a vector of right-hand side variables, possibly with interactions, \code{iv} is the list of explanatory variables, and \code{intv} are interaction terms
#'
#' @examples
#' var_check("a:d", c("a", "b", "c", "d"))
#' var_check(c("a", "b"), c("a", "b"), "a:c")
#' var_check(c("a", "b"), c("a", "b"), "a:c")
#' var_check(c("a", "b"), c("a", "b"), c("a:c", "I(b^2)"))
#'
#' @export
var_check <- function(ev, cn, intv = c()) {
  ## if : is used to select a range of variables evar is updated
  vars <- ev
  if (length(vars) < length(cn)) vars <- ev <- cn
  if (!is.empty(intv)) {
    if (all(unlist(strsplit(intv[!grepl("\\^", intv)], ":")) %in% vars)) {
      vars <- c(vars, intv)
    } else {
      cat("Interaction terms contain variables not selected as main effects.\nRemoving interactions from the estimation\n")
      intv <- intv[grepl("\\^", intv)]
    }
  }
  list(vars = vars, ev = ev, intv = intv)
}

#' Add interaction terms to list of test variables if needed
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param tv List of variables to use for testing for regress or logistic
#' @param int Interaction terms specified
#'
#' @return A vector of variables names to test
#'
#' @examples
#' test_specs("a", "a:b")
#' test_specs("a", c("a:b", "b:c"))
#' test_specs("a", c("a:b", "b:c", "I(c^2)"))
#' test_specs(c("a", "b", "c"), c("a:b", "b:c", "I(c^2)"))
#'
#' @export
test_specs <- function(tv, int) {
  int <- int[!grepl("\\^", int)]
  if (any(unlist(strsplit(int, ":")) %in% tv)) {
    cat("Interaction terms contain variables specified for testing.\nRelevant interaction terms are included in the requested test.\n\n")
    unique(int[unlist(sapply(tv, grep, int))])
  } else {
    tv
  }
}
#' Linear regression using OLS
#'
#' @details See \url{http://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param rvar The response variable in the regression
#' @param evar Explanatory variables in the regression
#' @param int Interaction terms to include in the model
#' @param check "standardize" to see standardized coefficient estimates. "stepwise" to apply step-wise selection of variables in estimation
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of all variables variables used in the regress function as an object of class regress
#'
#' @examples
#' result <- regress("diamonds", "price", c("carat","clarity"))
#' result <- regress("diamonds", "price", c("carat","clarity"), check = "standardize")
#'
#' @seealso \code{\link{summary.regress}} to summarize results
#' @seealso \code{\link{plot.regress}} to plot results
#' @seealso \code{\link{predict.regress}} to generate predictions
#'
#' @export
regress <- function(dataset, rvar, evar,
                    int = "",
                    check = "",
                    data_filter = "") {

  if (rvar %in% evar)
    return("Response variable contained in the set of explanatory variables.\nPlease update model specification." %>%
           add_class("regress"))

  dat <- getdata(dataset, c(rvar, evar), filt = data_filter)
  if (!is_string(dataset)) dataset <- "-----"

  if (any(summarise_each(dat, funs(does_vary)) == FALSE))
    return("One or more selected variables show no variation. Please select other variables." %>%
           add_class("regress"))

  vars <- ""
  var_check(evar, colnames(dat)[-1], int) %>%
    { vars <<- .$vars; evar <<- .$ev; int <<- .$intv }

  ## scale data
  isNum <- sapply(dat, is.numeric)
  if (sum(isNum) > 0) {
    if ("standardize" %in% check) {
      dat <- scaledf(dat)
    } else if ("center" %in% check) {
      dat <- scaledf(dat, scale = FALSE)
    }
  }

  form <- paste(rvar, "~", paste(vars, collapse = " + ")) %>% as.formula

  if ("stepwise" %in% check) {
    ## use k = 2 for AIC, use k = log(nrow(dat)) for BIC
    # model <- lm(paste(rvar, "~ 1") %>% as.formula, data = dat) %>%
    ## using backward stepwise selection
    model <- lm(form, data = dat) %>%
      step(k = 2, scope = list(upper = form), direction = "backward")
  } else {
    model <- lm(form, data = dat)
  }

  ## needed for prediction if standardization or centering is used
  if ("standardize" %in% check || "center" %in% check) {
    attr(model$model, "ms") <- attr(dat, "ms")
    attr(model$model, "sds") <- attr(dat, "sds")
  }

  coeff <- tidy(model)
  coeff$` ` <- sig_stars(coeff$p.value) %>% format(justify = "left")
  colnames(coeff) <- c("  ","coefficient","std.error","t.value","p.value"," ")
  isFct <- sapply(select(dat,-1), function(x) is.factor(x) || is.logical(x))
  if (sum(isFct) > 0) {
    for (i in names(isFct[isFct]))
      coeff$`  ` %<>% gsub(i, paste0(i,"|"), .) %>% gsub("\\|\\|","\\|",.)

    rm(i, isFct)
  }
  coeff$`  ` %<>% format(justify = "left")

  rm(dat) ## dat is not needed elsewhere and is already in "model" anyway

  as.list(environment()) %>% add_class(c("regress","model"))
}

#' Summary method for the regress function
#'
#' @details See \url{http://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{regress}}
#' @param sum_check Optional output. "rsme" to show the root mean squared error and the standard deviation of the residuals. "sumsquares" to show the sum of squares table. "vif" to show multicollinearity diagnostics. "confint" to show coefficient confidence interval estimates.
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param test_var Variables to evaluate in model comparison (i.e., a competing models F-test)
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- regress("diamonds", "price", c("carat","clarity"))
#' summary(result, sum_check = c("rmse","sumsquares","vif","confint"), test_var = "clarity")
#' result <- regress("ideal", "y", c("x1","x2"))
#' summary(result, test_var = "x2")
#' ideal %>% regress("y", "x1:x3") %>% summary
#'
#' @seealso \code{\link{regress}} to generate the results
#' @seealso \code{\link{plot.regress}} to plot results
#' @seealso \code{\link{predict.regress}} to generate predictions
#'
#' @importFrom car vif
#'
#' @export
summary.regress <- function(object,
                            sum_check = "",
                            conf_lev = .95,
                            test_var = "",
                            dec = 3,
                            ...) {

  if (is.character(object)) return(object)
  if (class(object$model)[1] != 'lm') return(object)

  if ("stepwise" %in% object$check) {
    cat("-----------------------------------------------\n")
    cat("Backward stepwise selection of variables\n")
    cat("-----------------------------------------------\n")
  }

  cat("Linear regression (OLS)\n")
  cat("Data     :", object$dataset, "\n")
  if (object$data_filter %>% gsub("\\s","",.) != "")
    cat("Filter   :", gsub("\\n","", object$data_filter), "\n")
  cat("Response variable    :", object$rvar, "\n")
  cat("Explanatory variables:", paste0(object$evar, collapse=", "), "\n")
  expl_var <- if (length(object$evar) == 1) object$evar else "x"
  cat(paste0("Null hyp.: the effect of ", expl_var, " on ", object$rvar, " is zero\n"))
  cat(paste0("Alt. hyp.: the effect of ", expl_var, " on ", object$rvar, " is not zero\n"))
  if ("standardize" %in% object$check) {
    cat("**Standardized coefficients shown (2 X SD)**\n")
  } else if ("center" %in% object$check) {
    cat("**Centered coefficients shown (x - mean(x))**\n")
  }

  coeff <- object$coeff
  # object$coeff$p.value <- "NaN"
  cat("\n")
  if (all(object$coeff$p.value == "NaN")) {
    coeff[,2] %<>% {sprintf(paste0("%.",dec,"f"),.)}
    print(coeff[,1:2], row.names=FALSE)
    cat("\nInsufficient variation in explanatory variable(s) to report additional statistics")
    return()
  } else {
    p.small <- coeff$p.value < .001
    coeff[,2:5] %<>% formatdf(dec)
    coeff$p.value[p.small] <- "< .001"
    print(coeff, row.names=FALSE)
  }

  if (nrow(object$model$model) <= (length(object$evar) + 1))
    return("\nInsufficient observations to estimate model")

  ## adjusting df for included intercept term
  df_int <- if (attr(object$model$terms, "intercept")) 1L else 0L

  reg_fit <- glance(object$model) %>% round(dec)
  if (reg_fit['p.value'] < .001) reg_fit['p.value'] <- "< .001"
  cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  cat("R-squared:", paste0(reg_fit$r.squared, ", "), "Adjusted R-squared:", reg_fit$adj.r.squared, "\n")
  cat("F-statistic:", reg_fit$statistic, paste0("df(", reg_fit$df - df_int, ",", reg_fit$df.residual, "), p.value"), reg_fit$p.value)
  cat("\nNr obs:", formatnr(reg_fit$df + reg_fit$df.residual, dec = 0), "\n\n")

  if (anyNA(object$model$coeff))
    cat("The set of explanatory variables exhibit perfect multicollinearity.\nOne or more variables were dropped from the estimation.\n")

  if ("rmse" %in% sum_check) {
    mean(object$model$residuals^2, na.rm=TRUE) %>% sqrt %>% round(dec) %>%
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
    ss_tab <- data.frame(matrix(nrow = 3, ncol = 2))
    rownames(ss_tab) <- c("Regression","Error","Total")
    colnames(ss_tab) <- c("df","SS")
    ss_tab$df <- c(df_reg,df_err,df_tot)
    ss_tab$SS <- c(ss_reg,ss_err,ss_tot)
    cat("Sum of squares:\n")
    format(ss_tab, scientific = FALSE) %>% print
    cat("\n")
  }

  if ("vif" %in% sum_check) {
    if (anyNA(object$model$coeff)) {
      cat("Multicollinearity diagnostics were not calculated.")
    } else {
      if (length(object$evar) > 1) {
        cat("Variance Inflation Factors\n")
        car::vif(object$model) %>%
          { if (!dim(.) %>% is.null) .[,"GVIF"] else . } %>% ## needed when factors are included
          data.frame("VIF" = ., "Rsq" = 1 - 1/.) %>%
          round(dec) %>%
          .[order(.$VIF, decreasing=T),] %>%
          { if (nrow(.) < 8) t(.) else . } %>%
          print
      } else {
        cat("Insufficient explanatory variables to calculate\nmulticollinearity diagnostics (VIF)")
      }
    }
    cat("\n")
  }

  if ("confint" %in% sum_check) {
    if (anyNA(object$model$coeff)) {
      cat("Confidence intervals were not calculated.\n")
    } else {

      ci_perc <- ci_label(cl = conf_lev)
      confint(object$model, level = conf_lev) %>%
        as.data.frame %>%
        set_colnames(c("Low","High")) %>%
        { .$`+/-` <- (.$High - .$Low)/2; . } %>%
        mutate_each(funs(sprintf(paste0("%.",dec,"f"),.))) %>%
        cbind(coeff[[2]],.) %>%
        set_rownames(object$coeff$`  `) %>%
        set_colnames(c("coefficient", ci_perc[1], ci_perc[2], "+/-")) %T>%
        print
      cat("\n")
    }
  }

  if (!is.null(test_var) && test_var[1] != "") {
    if ("stepwise" %in% object$check) {
      cat("Model comparisons are not conducted when Stepwise has been selected.\n")
    } else {
      sub_form <- ". ~ 1"

      vars <- object$evar
      if (object$int != "" && length(vars) > 1) {
        ## updating test_var if needed
        test_var <- test_specs(test_var, object$int)
        vars <- c(vars,object$int)
      }

      not_selected <- setdiff(vars,test_var)
      if (length(not_selected) > 0) sub_form <- paste(". ~", paste(not_selected, collapse = " + "))
      sub_mod <- update(object$model, sub_form, data = object$model$model) %>%
                   anova(object$model, test='F')

      if (sub_mod[,"Pr(>F)"][2] %>% is.na) return(cat(""))
      p.value <- sub_mod[,"Pr(>F)"][2] %>% { if (. < .001) "< .001" else round(.,dec) }

      cat(attr(sub_mod,"heading")[2])
        object$model$model[,1] %>%
        { sum((. - mean(.))^2) } %>%
        {1 - (sub_mod$RSS / .)} %>%
        round(dec) %>%
        cat("\nR-squared, Model 1 vs 2:", .)
      cat("\nF-statistic:", sub_mod$F[2] %>% round(dec), paste0("df(", sub_mod$Res.Df[1]-sub_mod$Res.Df[2], ",", sub_mod$Res.Df[2], "), p.value ", p.value))
    }
  }
}

#' Plot method for the regress function
#'
#' @details See \url{http://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{regress}}
#' @param plots Regression plots to produce for the specified regression model. Enter "" to avoid showing any plots (default). "hist" to show histograms of all variables in the model. "correlations" for a visual representation of the correlation matrix selected variables. "scatter" to show scatter plots (or box plots for factors) for the response variable with each explanatory variable. "dashboard" for a series of six plots that can be used to evaluate model fit visually. "resid_pred" to plot the explanatory variables against the model residuals. "coef" for a coefficient plot with adjustable confidence intervals. "leverage" to show leverage plots for each explanatory variable
#' @param lines Optional lines to include in the select plot. "line" to include a line through a scatter plot. "loess" to include a polynomial regression fit line. To include both use c("line","loess")
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param intercept Include the intercept in the coefficient plot (TRUE, FALSE). FALSE is the default
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This opion can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org/} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- regress("diamonds", "price", c("carat","clarity"))
#' plot(result, plots = "dashboard", lines = c("line","loess"))
#' plot(result, plots = "coef", conf_lev = .99, intercept = TRUE)
#' plot(result, plots = "hist")
#' plot(result, plots = "scatter", lines = c("line","loess"))
#' plot(result, plots = "correlations")
#' plot(result, plots = "resid_pred", lines = "line")
#'
#' @seealso \code{\link{regress}} to generate the results
#' @seealso \code{\link{summary.regress}} to summarize results
#' @seealso \code{\link{predict.regress}} to generate predictions
#'
#' @export
plot.regress <- function(x, plots = "",
                         lines = "",
                         conf_lev = .95,
                         intercept = FALSE,
                         shiny = FALSE,
                         custom = FALSE,
                         ...) {

  object <- x; rm(x)
  if (is.character(object)) return(object)
  if (class(object$model)[1] != 'lm') return(object)

  ## checking object size
  # object_size(object$model, model)
  model <- ggplot2::fortify(object$model)

  rvar <- object$rvar
  evar <- object$evar
  vars <- c(rvar, evar)

  flines <- sub("loess","",lines) %>% sub("line","",.)
  nlines <- sub("jitter","",lines)

  plot_list <- list()
  if ("hist" %in% plots)
    for (i in vars) {
      plot_list[[paste0("hist",i)]] <-
        visualize(select_(model, .dots = i), xvar = i, bins = 10, custom = TRUE)
    }

  if ("dashboard" %in% plots) {

    plot_list[[1]] <-
      visualize(model, xvar = ".fitted", yvar = rvar, type = "scatter", custom = TRUE) +
      labs(list(title = "Actual vs Fitted values", x = "Fitted", y = "Actual"))

    plot_list[[2]] <-
      visualize(model, xvar = ".fitted", yvar = ".resid", type = "scatter", custom = TRUE) +
      labs(list(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals"))

    plot_list[[3]] <- ggplot(model, aes(y=.resid, x=seq_along(.resid))) + geom_line() +
      labs(list(title = "Residuals vs Row order", x = "Row order", y = "Residuals"))

    plot_list[[4]] <- ggplot(model, aes_string(sample=".stdresid")) + stat_qq(alpha = .5) +
      labs(list(title = "Normal Q-Q", x = "Theoretical quantiles", y = "Standardized residuals"))

    plot_list[[5]] <-
      visualize(model, xvar = ".resid", custom = TRUE) +
      labs(list(title = "Histogram of residuals", x = "Residuals"))

    plot_list[[6]] <- ggplot(model, aes_string(x=".resid")) + geom_density(alpha=.3, fill = "green") +
      stat_function(fun = dnorm, args = list(mean = mean(model[,'.resid']), sd = sd(model[,'.resid'])), color = "blue") +
      labs(list(title = "Residuals vs Normal density", x = "Residuals", y = "")) + theme(axis.text.y = element_blank())

    if ("loess" %in% lines)
      for (i in 1:3) plot_list[[i]] <- plot_list[[i]] + sshhr( geom_smooth(method = "loess", size = .75, linetype = "dotdash") )

    if ("line" %in% lines) {
      for (i in c(1,4))
        plot_list[[i]] <- plot_list[[i]] + geom_abline(linetype = 'dotdash')
      for (i in 2:3)
        plot_list[[i]] <- plot_list[[i]] + sshhr( geom_smooth(method = "lm", se = FALSE, size = .75, linetype = "dotdash", colour = 'black') )
    }
  }

  if ("scatter" %in% plots) {
    for (i in evar) {
      if ("factor" %in% class(model[,i])) {
        plot_list[[paste0("scatter",i)]] <-
          visualize(select_(model, .dots = c(i,rvar)), xvar = i, yvar = rvar, type = "scatter", check = flines, alpha = .2, custom = TRUE)
      } else {
        plot_list[[paste0("scatter",i)]] <-
          visualize(select_(model, .dots = c(i,rvar)), xvar = i, yvar = rvar, type = "scatter", check = nlines, custom = TRUE)
      }
    }
  }

  if ("resid_pred" %in% plots) {
    for (i in evar) {
      if ("factor" %in% class(model[,i])) {
        plot_list[[i]] <-
          visualize(select_(model, .dots = c(i,".resid")), xvar = i, yvar = ".resid", type = "scatter", check = flines, alpha = .2, custom = TRUE) +
          ylab("residuals")
      } else {
        plot_list[[i]] <-
          visualize(select_(model, .dots = c(i,".resid")), xvar = i, yvar = ".resid", type = "scatter", check = nlines, custom = TRUE) +
          ylab("residuals")
      }
    }
  }

  if ("coef" %in% plots) {

    yl <- if ("standardize" %in% object$check) "Coefficient (standardized)" else "Coefficient"

    p <-
      confint(object$model, level = conf_lev) %>%
      data.frame %>%
      na.omit %>%
      set_colnames(c("Low","High")) %>%
      cbind(select(object$coeff,2),.) %>%
      set_rownames(object$coeff$`  `) %>%
      { if (!intercept) .[-1,] else . } %>%
      mutate(variable = rownames(.)) %>%
        ggplot() +
          geom_pointrange(aes_string(x = "variable", y = "coefficient",
                          ymin = "Low", ymax = "High")) +
          geom_hline(yintercept = 0, linetype = 'dotdash', color = "blue") +
          xlab("") +
          ylab(yl) +
          scale_x_discrete(limits = {if (intercept) rev(object$coeff$`  `) else rev(object$coeff$`  `[-1])}) +
          coord_flip() + theme(axis.text.y = element_text(hjust = 0))
    return(p)
  }

  if ("correlations" %in% plots)
    return(radiant.basics:::plot.correlation_(object$model$model))

  # if ("leverage" %in% plots) {
  #   ## no plots if aliased coefficients present
  #   if (anyNA(object$model$coeff))
  #     return("The set of explanatory variables exhibit perfect multicollinearity.\nOne or more variables were dropped from the estimation.\nLeverage plot will not be shown")
  #   return(car::leveragePlots(object$model, main = "", ask=FALSE, id.n = 1,
  #          layout = c(ceiling(length(evar)/2),2)))
  # }

  if (custom)
    if (length(plot_list) == 1) return(plot_list[[1]]) else return(plot_list)

  if (exists("plot_list")) {
    sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = 2))) ) %>%
        {if (shiny) . else print(.)}
  }
}

#' Predict method for the regress function
#'
#' @details See \url{http://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{regress}}
#' @param pred_data Name of the dataset to use for prediction
#' @param pred_cmd Command used to generate data for prediction
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param se Logical that indicates if prediction standard errors should be calculated (default = FALSE)
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- regress("diamonds", "price", c("carat","clarity"))
#' predict(result, pred_cmd = "carat = 1:10")
#' predict(result, pred_cmd = "clarity = levels(clarity)")
#' result <- regress("diamonds", "price", c("carat","clarity"), int = c("carat:clarity"))
#' dpred <<- getdata("diamonds") %>% slice(1:10)
#' predict(result, pred_data = "dpred")
#' rm(dpred, envir = .GlobalEnv)
#'
#' @seealso \code{\link{regress}} to generate the result
#' @seealso \code{\link{summary.regress}} to summarize results
#' @seealso \code{\link{plot.regress}} to plot results
#'
#' @export
predict.regress <- function(object,
                            pred_data = "",
                            pred_cmd = "",
                            conf_lev = 0.95,
                            se = TRUE,
                            dec = 3,
                            ...) {

 if ("center" %in% object$check || "standardize" %in% object$check) se <- FALSE

 pfun <- function(model, pred, se, conf_lev) {

    pred_val <-
      try(sshhr(
        predict(model, pred, interval = ifelse (se, "prediction", "none"), level = conf_lev)),
        silent = TRUE
      )

    if (!is(pred_val, 'try-error')) {
      if (se) {
        pred_val %<>% data.frame %>% mutate(diff = .[,3] - .[,1])
        ci_perc <- ci_label(cl = conf_lev)
        colnames(pred_val) <- c("Prediction",ci_perc[1],ci_perc[2],"+/-")
      } else {
        pred_val %<>% data.frame %>% select(1)
        colnames(pred_val) <- "Prediction"
      }
    }

    pred_val
  }

  predict.model(object, pfun, "regress.predict", pred_data, pred_cmd, conf_lev, se, dec)
}

#' Predict method for model functions
#'
#' @details See \url{http://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{regress}}
#' @param pfun Function to use for prediction
#' @param mclass Model class to attach
#' @param pred_data Name of the dataset to use for prediction
#' @param pred_cmd Command used to generate data for prediction
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param se Logical that indicates if prediction standard errors should be calculated (default = FALSE)
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @importFrom radiant.data set_attr
#'
#' @export
predict.model <- function(object, pfun, mclass,
                          pred_data = "",
                          pred_cmd = "",
                          conf_lev = 0.95,
                          se = FALSE,
                          dec = 3,
                          ...) {

  if (is.character(object)) return(object)

  if (is_empty(pred_data) && is_empty(pred_cmd))
    return("Please select data and/or specify a command to generate predictions.\nFor example, carat = seq(.5, 1.5, .1) would produce predictions for values\n of carat starting at .5, increasing to 1.5 in increments of .1. Make sure\nto press return after you finish entering the command.\n\nAlternatively, specify a dataset to generate predictions. You could create\nthis in a spread sheet and use the paste feature in Data > Manage to bring\nit into Radiant")

  # if ("standardize" %in% object$check)
  #   return("Standardized coefficients will not be used for prediction.\nPlease uncheck the 'standardize' box and try again")

  # if ("center" %in% object$check)
  #   return("Centered coefficients will not be used for prediction.\nPlease uncheck the 'center' box and try again")

  pred_type <- "cmd"
  vars <- object$evar
  if (is_empty(pred_data) && !is_empty(pred_cmd)) {
    pred_cmd %<>% gsub("\"","\'",.) %>% gsub(";\\s*$","",.) %>% gsub(";",",",.)
    pred <- try(eval(parse(text = paste0("with(object$model$model, expand.grid(", pred_cmd ,"))"))), silent = TRUE)
    if (is(pred, 'try-error'))
      return(paste0("The command entered did not generate valid data for prediction. The\nerror message was:\n\n", attr(pred,"condition")$message, "\n\nPlease try again. Examples are shown in the help file."))

    # adding information to the prediction data.frame
    dat_classes <- attr(object$model$term, "dataClasses")[-1]

    ## weights mess-up data manipulation below so remove from
    wid <- which(names(dat_classes) %in% "(weights)")
    if (length(wid) > 0) dat_classes <- dat_classes[-wid]

    isFct <- dat_classes == "factor"
    isLog <- dat_classes == "logical"
    isNum <- dat_classes == "numeric"
    dat <- select_(object$model$model, .dots = vars)

    # based on http://stackoverflow.com/questions/19982938/how-to-find-the-most-frequent-values-across-several-columns-containing-factors
    max_freq <- function(x) names(which.max(table(x)))
    max_lfreq <- function(x) ifelse(mean(x) > .5, TRUE, FALSE)

    plug_data <- data.frame(init___ = 1)
    if (sum(isNum) > 0)
      plug_data %<>% bind_cols(., summarise_each_(dat, funs(mean), vars[isNum]))
    if (sum(isFct) > 0)
      plug_data %<>% bind_cols(., summarise_each_(dat, funs(max_freq), vars[isFct]))
    if (sum(isLog) > 0)
      plug_data %<>% bind_cols(., summarise_each_(dat, funs(max_lfreq), vars[isLog]))

    rm(dat)

    if ((sum(isNum) + sum(isFct) + sum(isLog)) < length(vars)) {
      return("The model includes data-types that cannot be used for\nprediction at this point\n")
    } else {
      if (sum(names(pred) %in% names(plug_data)) < length(names(pred))) {
        return("The expression entered contains variable names that are not in the model.\nPlease try again.\n\n")
      } else {

        plug_data[names(pred)] <- list(NULL)
        pred <- cbind(select(plug_data,-1), pred)
      }
    }
  } else {
    ## generate predictions for all observations in the dataset
    pred <- getdata(pred_data, filt = "", na.rm = FALSE)
    pred_names <- names(pred)
    pred <- try(select_(pred, .dots = vars), silent = TRUE)

    if (is(pred, 'try-error'))
      return(paste0("Model variables: ", paste0(vars, collapse = ", "), "\nProfile variables to be added: ", paste0(vars[!vars %in% pred_names], collapse = ", ")))

    if (!is_empty(pred_cmd)) {
      pred_cmd <- pred_cmd %>% gsub("\"","\'",.) %>% gsub(" ","",.)
      vars <-
        strsplit(pred_cmd, ";")[[1]] %>% strsplit(., "=") %>%
        sapply("[", 1) %>% gsub(" ","",.)
      dots <- strsplit(pred_cmd, ";")[[1]] %>% gsub(" ","",.)
      for (i in seq_along(dots))
        dots[[i]] <- sub(paste0(vars[[i]],"="),"",dots[[i]])

      pred <- try(mutate_(pred, .dots = setNames(dots, vars)), silent = TRUE)
      if (is(pred, 'try-error')) {
        return(paste0("The command entered did not generate valid data for prediction. The\nerror message was:\n\n", attr(pred,"condition")$message, "\n\nPlease try again. Examples are shown in the help file."))
      }
      pred_type <- "datacmd"
    } else {
      pred_type <- "data"
    }

    pred %<>% na.omit()
  }

  ## scale predictors if needed
  if ("center" %in% object$check || "standardize" %in% object$check) {
    attr(pred, "ms") <- attr(object$model$model, "ms")
    if ("standardize" %in% object$check) {
      scale <- TRUE
      attr(pred, "sds") <- attr(object$model$model, "sds")
    } else {
      scale <- FALSE
    }
    pred_val <- scaledf(pred, center = TRUE, scale = scale, calc = FALSE) %>%
      pfun(object$model, ., se, conf_lev)
  } else {
    ## generate predictions using the supplied function (pfun)
    pred_val <- pfun(object$model, pred, se, conf_lev)
  }

  if (!is(pred_val, 'try-error')) {
    ## scale rvar
    if ("center" %in% object$check) {
      ms <- attr(object$model$model, "ms")[[object$rvar]]
      if (!is.null(ms))
        pred_val[["Prediction"]] <- pred_val[["Prediction"]]  + ms
    } else if ("standardize" %in% object$check) {
      ms <- attr(object$model$model, "ms")[[object$rvar]]
      sds <- attr(object$model$model,"sds")[[object$rvar]]
      if (!is.null(ms) && !is.null(sds))
        pred_val[["Prediction"]] <- pred_val[["Prediction"]] * 2 * sds + ms
    }

    pred <- data.frame(pred, pred_val, check.names = FALSE)
    vars <- colnames(pred)

    if ("stepwise" %in% object$check) {
      ## show only the selected variables when printing predictions
      object$evar <- attr(terms(object$model), "variables") %>% as.character %>% .[-c(1,2)]
      vars <- c(object$evar, colnames(pred_val))
    }

    ## adding attributes used by other methods
    pred <- set_attr(pred, "dataset", object$dataset) %>%
      set_attr("data_filter", object$data_filter) %>%
      set_attr("rvar", object$rvar) %>%
      set_attr("evar", object$evar) %>%
      set_attr("vars", vars) %>%
      set_attr("dec", dec) %>%
      set_attr("pred_type", pred_type) %>%
      set_attr("pred_data", pred_data) %>%
      set_attr("pred_cmd", pred_cmd)

    return(add_class(pred, c(mclass, "model.predict")))
  } else {
    return(paste0("The command entered did not generate valid data for prediction. The\nerror message was:\n\n", attr(pred_val,"condition")$message, "\n\nPlease try again. Examples are shown in the help file."))
  }
}

#' Print method for the model prediction
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#' @param header Header line
#' @param lev The level in the response variable defined as _success_ for classification models
#'
#' @export
print.model.predict <- function(x, ..., n = 10, header = "", lev = "") {

  data_filter <- attr(x, "data_filter")
  vars <- attr(x, "vars")
  pred_type <- attr(x, "pred_type")
  pred_data <- attr(x, "pred_data")
  pred_cmd <- gsub("([=+-/*])", " \\1 ", attr(x, "pred_cmd")) %>% gsub("([;])", "\\1 ", .)

  cat(header)
  cat("\nData                 :", attr(x, "dataset"), "\n")
  if (data_filter %>% gsub("\\s","",.) != "")
    cat("Filter               :", gsub("\\n","", data_filter), "\n")
  cat("Response variable    :", attr(x, "rvar"), "\n")
  if (!is_empty(lev))
    cat("Level                :", lev, "in", attr(x, "rvar"), "\n")
  cat("Explanatory variables:", paste0(attr(x, "evar"), collapse=", "), "\n")

  if (!is.character(pred_data)) pred_data <- "-----"
  if (pred_type == "cmd") {
    cat("Prediction command   :", pred_cmd, "\n")
  } else if (pred_type == "datacmd") {
    cat("Prediction dataset   :", pred_data,"\n")
    cat("Customize command    :", pred_cmd, "\n")
  } else {
    cat("Prediction dataset   :", pred_data,"\n")
  }

  if (n == -1) {
    cat("\n")
    formatdf(x[,vars], attr(x, "dec")) %>% print(row.names = FALSE)
  } else {
    if (nrow(x) > n)
      cat("Rows shown           :", n, "\n")
    cat("\n")
    head(x[,vars], n) %>% formatdf(attr(x, "dec")) %>% print(row.names = FALSE)
  }
}

#' Print method for predict.regress
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#'
#' @export
print.regress.predict <- function(x, ..., n = 10)
  print.model.predict(x, ..., n = n, header = "Linear regression (OLS)")

#' Plot method for model.predict functions
#'
#' @param x Return value from predict functions (e.g., predict.regress)
#' @param xvar Variable to display along the X-axis of the plot
#' @param facet_row Create vertically arranged subplots for each level of the selected factor variable
#' @param facet_col Create horizontally arranged subplots for each level of the selected factor variable
#' @param color Adds color to a scatter plot to generate a heat map. For a line plot one line is created for each group and each is assigned a different colour
#' @param conf_lev Confidence level to use for prediction intervals (.95 is the default)
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' regress("diamonds", "price", c("carat","clarity")) %>%
#'   predict(pred_cmd = "carat = 1:10") %>%
#'   plot(xvar = "carat")
#' logistic("titanic", "survived", c("pclass","sex","age"), lev = "Yes") %>%
#'   predict(pred_cmd="pclass=levels(pclass), sex=levels(sex), age=seq(0,100,20)") %>%
#'   plot(xvar = "age", color = "sex", facet_col = "pclass")
#'
#' @seealso \code{\link{predict.regress}} to generate predictions
#' @seealso \code{\link{predict.logistic}} to generate predictions
#'
#' @export
plot.model.predict <- function(x, xvar = "",
                               facet_row = ".",
                               facet_col = ".",
                               color = "none",
                               conf_lev = .95,
                               ...) {

  ## should work with req in regress_ui but doesn't
  if (is_empty(xvar)) return(invisible())

  object <- x; rm(x)
  if (is.character(object)) return(object)

  cn <- colnames(object)
  pvars <- "Prediction"
  cnpred <- which(cn == pvars)
  if (length(cn) > cnpred) {
    pvars <- c(pvars,"ymin","ymax")
    cn[cnpred + 1] <- pvars[2]
    cn[cnpred + 2] <- pvars[3]
    colnames(object) <- cn
  }

  byvar <- NULL
  if (color != "none") byvar <- color
  if (facet_row != ".")
    byvar <- if (is.null(byvar)) facet_row else unique(c(byvar, facet_row))

  if (facet_col != ".")
    byvar <- if (is.null(byvar)) facet_col else unique(c(byvar, facet_col))

  tbv <- if (is.null(byvar)) xvar else c(xvar, byvar)
  tmp <- object %>% group_by_(.dots = tbv) %>% select_(.dots = c(tbv, pvars)) %>% summarise_each(funs(mean))
  if (color == 'none') {
    p <- ggplot(tmp, aes_string(x=xvar, y="Prediction")) + geom_line(aes(group = 1))
  } else {
    p <- ggplot(tmp, aes_string(x=xvar, y="Prediction", color = color, group = color)) + geom_line()
  }

  if (facet_row != "." || facet_col != ".") {
    facets <- ifelse (facet_row == ".", paste("~", facet_col), paste(facet_row, '~', facet_col))
    facet_fun <- ifelse (facet_row == ".", facet_wrap, facet_grid)
    p <- p + facet_fun(as.formula(facets))
  }

  if (length(pvars) == 3) {
    if (is.factor(tmp[[xvar]]) || length(unique(tmp[[xvar]])) < 11)
      p <- p + geom_pointrange(aes_string(ymin = "ymin", ymax = "ymax"), size=.3)
    else
      p <- p + geom_smooth(aes_string(ymin = "ymin", ymax = "ymax"), stat="identity")
  }

  sshhr(p)
}

#' Deprecated function to store regression residuals and predictions
#'
#' @details Use \code{\link{store.model.predict}} or \code{\link{store.model}} instead
#'
#' @param object Return value from \code{\link{regress}} or \code{\link{predict.regress}}
#' @param data Dataset name
#' @param type Residuals ("residuals") or predictions ("predictions"). For predictions the dataset name must be provided
#' @param name Variable name assigned to the residuals or predicted values
#'
#' @export
store_reg <- function(object, data = object$dataset,
                      type = "residuals", name = paste0(type, "_reg")) {

  if (type == "residuals")
    store.model(object, data = data, name = name)
  else
    store.model.predict(object, data = data, name = name)
}

#' Store predicted values generated in model functions
#'
#' @details See \url{http://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param object Return value from model function
#' @param ... Additional arguments
#' @param data Data or dataset name (e.g., data = mtcars or data = "mtcars")
#' @param name Variable name(s) assigned to predicted values
#'
#' @examples
#' regress(diamonds, rvar = "price", evar = c("carat","cut")) %>%
#'   predict(diamonds) %>%
#'   store(name = "pred, pred_low, pred_high") %>% head
#'
#' @export
store.model.predict <- function(object, ..., data = attr(object,"pred_data"), name = "prediction") {
  if (is_empty(name)) name <- "prediction"

  ## gsub needed because trailing/leading spaces may be added to the variable name
  ind <- which(colnames(object) == "Prediction")

  ## if se was calculated
  name <- unlist(strsplit(name, ",")) %>% gsub("\\s","",.)
  if (length(name) > 1) {
    name <- name[1:min(3, length(name))]
    ind_mult <- ind:(ind + length(name[-1]))
    df <- object[,ind_mult, drop = FALSE]
  } else {
    df <- object[,"Prediction", drop = FALSE]
  }

  vars <- colnames(object)[1:(ind-1)]
  indr <- indexr(data, vars, "")
  pred <- as_data_frame(matrix(NA, nrow = indr$nr, ncol = ncol(df)))
  pred[indr$ind, ] <- as.vector(df) ## as.vector removes all attributes from df

  changedata(data, vars = pred, var_names = name)
}

#' Store residuals from a model
#'
#' @details See \url{http://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param object Return value from a model function
#' @param ... Additional arguments
#' @param name Variable name(s) assigned to predicted values
#'
#' @examples
#' regress(diamonds, rvar = "price", evar = c("carat","cut")) %>%
#'   store %>% head
#'
#' @export
store.model <- function(object, ..., name = "residuals") {
  if (is_empty(name)) name <- "residuals"
  dat <- {if (object$dataset == "-----") object$model$model else object$dataset}
  indr <- indexr(dat, c(object$rvar, object$evar), object$data_filter)
  res <- rep(NA, indr$nr)
  res[indr$ind] <- object$model$residuals
  changedata(dat, vars = res, var_names = name)
}

#' Check if main effects for all interaction effects are included in the model
#' If ':' is used to select a range _evar_ is updated
#' @details See \url{http://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param ev List of explanatory variables provided to _regress_ or _logistic_
#' @param cn Column names for all explanatory variables in _dat_
#' @param intv Interaction terms specified
#'
#' @return 'vars' is a vector of right-hand side variables, possibly with interactions, 'iv' is the list of explanatory variables, and intv are interaction terms
#'
#' @examples
#' var_check("a:d", c("a","b","c","d"))
#' var_check(c("a", "b"), c("a", "b"), "a:c")
#'
#' @export
var_check <- function(ev, cn, intv = "") {

  ## if : is used to select a range of variables evar is updated
  vars <- ev
  if (length(vars) < length(cn)) vars <- ev <- cn

  if (intv != "" && length(vars) > 1) {
    if ({intv %>% strsplit(":") %>% unlist} %in% vars %>% all) {
      vars <- c(vars, intv)
    } else{
      cat("Interaction terms contain variables not selected as main effects.\nRemoving all interactions from the estimation")
      intv <- ""
    }
  }

  list(vars = vars, ev = ev, intv = intv)
}

#' Add interaction terms to list of test variables if needed
#'
#' @details See \url{http://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
#'
#' @param test_var List of variables to use for testing for regress or logistic
#' @param int Interaction terms specified
#'
#' @return A vector of variables names to test
#'
#' @examples
#' test_specs("a", c("a:b", "b:c"))
#'
#' @export
test_specs <- function(test_var, int) {

  if ({int %>% strsplit(":") %>% unlist} %in% test_var %>% any) {
    cat("Interaction terms contain variables specified for testing.\nRelevant interaction terms are included in the requested test.\n\n")
    for (i in test_var) test_var <- c(test_var, int[grep(i, int)])
    test_var <- unique(test_var)
  }
  test_var
}

#' Classification and regression trees
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/crtree.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param rvar The response variable in the model
#' @param evar Explanatory variables in the model
#' @param type Model type (i.e., "classification" or "regression")
#' @param lev The level in the response variable defined as _success_
#' @param wts Weights to use in estimation
#' @param cp Minimum proportion of root node deviance required for split (default = 0.00001)
#' @param nodes Maxiumum size of tree in number of nodes to return. If equal to NA no pruning is done
#' @param K Number of folds use in cross-validation
#' @param seed Random seed used for cross-validation
#' @param split Splitting criterium to use (i.e., "gini" or "information")
#' @param prior Adjust the initial probabily for the selected level (e.g., set to .5 in unbalanced samples)
#' @param cost Cost for each connection (e.g., email or mailing)
#' @param margin Margin on each customer purchase
#' @param check Optional estimation parameters ("standardize" is the default)
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list with all variables defined in crtree as an object of class tree
#'
#' @examples
#' result <- crtree("titanic", "survived", c("pclass","sex"), lev = "Yes")
#' result <- crtree("titanic", "survived", c("pclass","sex"))
#' result <- crtree("diamonds", "price", c("carat","clarity"), type = "regression")
#'
#' @seealso \code{\link{summary.crtree}} to summarize results
#' @seealso \code{\link{plot.crtree}} to plot results
#' @seealso \code{\link{predict.crtree}} for prediction
#'
#' @importFrom rpart rpart rpart.control prune.rpart
#'
#' @export
crtree <- function(dataset, rvar, evar,
                   type = "",
                   lev = "",
                   wts = "None",
                   cp = 0.001,
                   nodes = NA,
                   K = 10,
                   seed = 1234,
                   split = "gini",
                   prior = NA,
                   cost = NA,
                   margin = NA,
                   check = "",
                   data_filter = "") {

  # library(rpart)
  # library(radiant.model)
  # dataset <- "titanic"
  # rvar <- "survived"
  # evar <- c("pclass","sex")
  # type <- "classification"
  # lev <- "Yes"
  # mindev <- 0.0001
  # cp <- mindev
  # nodes <- NA
  # K <- 5
  # split = "gini"
  # wts <- "None"
  # check <- ""
  # data_filter <- ""

  if (rvar %in% evar)
    return("Response variable contained in the set of explanatory variables.\nPlease update model specification." %>%
           add_class("crtree"))

  ## allow cp to be negative so full tree is built http://stackoverflow.com/q/24150058/1974918
  if (is_empty(cp))
    return("Please provide a complexity parameter to split the data." %>% add_class("crtree"))

  if (!is_empty(nodes) && nodes < 2)
    return("The (maximum) number of nodes in the tree should be larger than or equal to 2." %>% add_class("crtree"))


  if (!is.null(wts) && wts == "None") {
    wts <- NULL
    vars <- c(rvar, evar)
  } else {
    wtsname <- wts
    vars <- c(rvar, evar, wtsname)
  }

  dat <- getdata(dataset, vars, filt = data_filter)
  if (!is_string(dataset)) dataset <- deparse(substitute(dataset)) %>% set_attr("df", TRUE)

  if (!is_not(wts)) {
    wts <- dat[[wtsname]]
    dat <- select_(dat, .dots = paste0("-",wtsname))

    if (!is.integer(wts)) {
      ## rounding to avoid machine precision differences
      wts_int <- as.integer(round(wts,.Machine$double.rounding))
      if (all(round(wts,.Machine$double.rounding) == wts_int)) wts <- wts_int
      rm(wts_int)
    }
  }

  if (any(summarise_all(dat, funs(does_vary)) == FALSE))
    return("One or more selected variables show no variation. Please select other variables." %>% add_class("crtree"))

  rv <- dat[[rvar]]

  if (type == "classification" && !is.factor(rv))
    dat[[rvar]] <- as_factor(dat[[rvar]])

  if (is.factor(dat[[rvar]])) {
    if (type == "regression")
      return("Cannot estimate a regression when the response variable is of type factor." %>% add_class("crtree"))

    if (lev == "") {
      lev <- levels(dat[[rvar]])[1]
    } else {

      if (!lev %in% levels(dat[[rvar]]))
        return(paste0("Specified level is not a level in ", rvar) %>% add_class("crtree"))

      dat[[rvar]] <- factor(dat[[rvar]], levels = unique(c(lev, levels(dat[[rvar]]))))
    }

    type <- "classification"
  } else {
    type <- "regression"
  }

  ## standardize data ...
  if ("standardize" %in% check) dat <- scaledf(dat, wts = wts)

  vars <- evar
  ## in case : is used
  if (length(vars) < (ncol(dat)-1)) vars <- evar <- colnames(dat)[-1]

  form <- paste(rvar, "~ . ")

  seed %>% gsub("[^0-9]","",.) %>% { if (!is_empty(.)) set.seed(seed) }

  ## make max tree
  # http://stackoverflow.com/questions/24150058/rpart-doesnt-build-a-full-tree-problems-with-cp
  # if (cp == 0) cp <- -1
  control <- rpart::rpart.control(cp = cp, xval = K, minsplit = 2, minbucket = 1)

  parms <- list(split = split)
  # prior <- NULL
  # prior <- .5
  # loss <- c(6,.5)
  # cost <- .5
  # margin <- 6
  if (type == "classification") {

    ind <- if(which(lev %in% levels(dat[[rvar]])) == 1) c(1,2) else c(2,1)

    if (!is_not(cost) && !is_not(margin)) {

      parms[["loss"]] <- c(as_numeric(margin), as_numeric(cost)) %>% .[ind] %>%
        {matrix(c(0,.[1],.[2],0), byrow = TRUE, nrow = 2)}
      # print(parms)
      # parms[["loss"]] <- NULL
      # if (which(lev %in% levels(dat[[rvar]])) == 1) {
      #   parms[["loss"]] <- matrix(c(0,cost,margin,0), byrow = TRUE, nrow = 2)
      # } else {
      #   parms[["loss"]] <- matrix(c(0,margin,cost,0), byrow = TRUE, nrow = 2)
      # }
      # parms[["loss"]] <- matrix(c(0,loss[1],loss[2],0), byrow = TRUE, nrow = 2)
    } else if (!is_empty(prior)) {
      # prior <- gsub(","," ", prior) %>% strsplit("\\s+") %>% unlist
      # asNum <- function(x) ifelse(length(x) > 1, as.numeric(x[1])/as.numeric(x[2]), as.numeric(x[1]))
      # prior <- sshhr( strsplit(prior, "/") %>% sapply(asNum) )

      if (!is.numeric(prior))
        return("Prior did not resolve to a numeric factor" %>% add_class("crtree"))
      else if (prior > 1 || prior < 0)
        return("Prior is not a valid probability" %>% add_class("crtree"))
      else
        parms[["prior"]] <- c(prior, 1 - prior) %>% .[ind]

        # return("Prior did not resolve to a numeric factor" %>% add_class("crtree"))
      # else if (length(prior) == 2 && sum(prior) == 1)
        # parms[["prior"]] <- prior
      # else if (length(prior) > 2)
        # return("Priors only supported for bivariate classification" %>% add_class("crtree"))
      # else
        # return("Priors must sum to 1. Use fractions if needed" %>% add_class("crtree"))
    }
  }

  model <- rpart::rpart(as.formula(form), data = dat,
                        parms = parms,
                        weights = wts, control = control)

  if (!is_not(nodes)) {
    unpruned <- model
    if (nrow(model$frame) > 1) {

      cptab <- as.data.frame(model$cptable)
      cptab$nodes <- cptab$nsplit + 1
      ind <- max(which(cptab$nodes <= nodes))
      model <- sshhr(rpart::prune.rpart(model, cp = cptab$CP[ind]))
    }
  }

  ## rpart::rpart does not return residuals by default
  model$residuals <- residuals(model, type = "pearson")

  ## adjusting predicted probabilities
  # if (!is_empty(prior) && length(prior) == 2) {
  # if (!is_empty(prior)) {

  if (is_not(cost) && is_not(margin) && !is_empty(prior)) {

    ## note that this adjust will reset the prior in the print out
    ## to the original prior

    ## predicting using rpart
    # pred <- data.frame(rvar = dat[[rvar]] == lev, pred = predict(model, dat)[,lev])

    ## generating scaled predictions using logistic regression
    # pred <- sshhr(glm(rvar ~ pred + I(pred^2) + I(pred^3) + I(pred^4) + I(pred^5), data = pred) %>%
    #   predict(data.frame(pred = model$frame$yval2[,4])))

    ## mean adjustment
    # pred <- model$frame$yval2[,4] %>% {. / mean(.)} * mean(dat[[rvar]] == lev)

    ## storing adjusted prediction in the 'frame'
    # model$frame$yval2[,4] <- pred
    # model$frame$yval2[,5] <- 1 - pred

    ## using the SAS approach
    org_frac <- mean(dat[[rvar]] == lev)
    # over_frac <- prior[which(lev %in% levels(dat[[rvar]]))]
    over_frac <- prior
    model$frame$yval2[,4] <- 1/(1+(1/org_frac-1)/(1/over_frac-1)*(1/model$frame$yval2[,4]-1))
    model$frame$yval2[,5] <- 1 - model$frame$yval2[,4]
  }

  ## rpart::rpart does not return residuals by default
  # model$residuals <- residuals(model, type = "pearson")

  # pred <- predict(model, dat)[,1]
  # #   ## based on https://stat.ethz.ch/R-manual/R-devel/library/rpart/html/residuals.rpart.html
  # if (is.factor(dat[[rvar]])) {
  #   ## based on https://stat.ethz.ch/R-manual/R-devel/library/rpart/html/residuals.rpart.html
  #   model$residuals <- (1 - pred) / sqrt(pred * (1 - pred))
  # } else {
  #   model$residuals <- rv - pred
  # }
  # rm(pred)

  ## tree model object does not include the data by default
  model$model <- dat
  rm(dat) ## dat not needed elsewhere

  as.list(environment()) %>% add_class(c("crtree","model"))
}

#' Summary method for the crtree function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/crtree.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{crtree}}
#' @param prn Print tree in text form
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- crtree("titanic", "survived", c("pclass","sex"), lev = "Yes")
#' summary(result)
#' result <- crtree("diamonds", "price", c("carat","color"), type = "regression")
#' summary(result)
#'
#' @seealso \code{\link{crtree}} to generate results
#' @seealso \code{\link{plot.crtree}} to plot results
#' @seealso \code{\link{predict.crtree}} for prediction
#'
#' @export
summary.crtree <- function(object, prn = TRUE, ...) {

  if (is.character(object)) return(object)

  if (object$type == "classification")
    cat("Classification tree")
  else
    cat("Regression tree")
  cat("\nData                 :", object$dataset)
  if (object$data_filter %>% gsub("\\s","",.) != "")
    cat("\nFilter               :", gsub("\\n","", object$data_filter))
  cat("\nResponse variable    :", object$rvar)
  if (object$type == "classification")
    cat("\nLevel                :", object$lev, "in", object$rvar)
  cat("\nExplanatory variables:", paste0(object$evar, collapse=", "),"\n")
  if (length(object$wtsname) > 0)
    cat("Weights used         :", object$wtsname, "\n")
  cat("Complexity parameter :", object$cp, "\n")
  if (!is_not(object$nodes)) {
    max_nodes <- sum(object$unpruned$frame$var == "<leaf>")
    cat("Maximum nr. nodes    :", object$nodes, "out of", max_nodes, "\n")
  }
  if (!is_empty(object$cost) && !is_empty(object$margin) && object$type == "classification") {
    cat("Cost:Margin          :", object$cost, ":", object$margin, "\n")
    if (!is_empty(object$prior)) object$prior <- "Prior ignored when cost and margin set"
  }
  if (!is_empty(object$prior) && object$type == "classification")
    cat("Priors               :", paste0(object$prior, collapse=", "),"\n")
  if (!is_empty(object$wts, "None") && class(object$wts) == "integer")
    cat("Nr obs               :", formatnr(sum(object$wts), dec = 0), "\n\n")
  else
    cat("Nr obs               :", formatnr(length(object$rv), dec = 0), "\n\n")

  ## extra output
  # print(object$model$cptable)
  # print(summary(object$model))

  if (prn)
    cat(paste0(capture.output(print(object$model, digits = 4))[c(-1, -2)], collapse = "\n"))
}

#' Plot method for the crtree function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/crtree.html} for an example in Radiant. The standard tree plot used by by the rpart package can be generated by \code{plot.rpart(result$model)}. See \code{\link{plot.rpart}} for additional details.
#'
#' @param x Return value from \code{\link{crtree}}
#' @param plots Plots to produce for the specified rpart tree. "tree" shows a tree diagram. "prune" shows a line graph to evaluate appropriate tree pruning. "imp" shows a variable importance plot
#' @param orient Plot orientation: LR for vertical and TD for horizontal
#' @param labs Use factor labels in plot (TRUE) or revert to default letters used by tree (FALSE)
#' @param dec Decimal places to round results to
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This opion can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org/} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- crtree("titanic", "survived", c("pclass","sex"), lev = "Yes")
#' plot(result)
#' result <- crtree("diamonds", "price", c("carat","clarity", "cut"))
#' plot(result, plots = "prune")
#' result <- crtree("dvd", "buy", c("coupon","purch", "last"), cp = .01)
#' plot(result, plots = "imp")
#'
#' @importFrom DiagrammeR DiagrammeR mermaid
#'
#' @seealso \code{\link{crtree}} to generate results
#' @seealso \code{\link{summary.crtree}} to summarize results
#' @seealso \code{\link{predict.crtree}} for prediction
#'
#' @export
plot.crtree <- function(x, plots = "tree", orient = "LR", 
                        labs = TRUE, dec = 2, shiny = FALSE, 
                        custom = FALSE, ...) {

  if (is_empty(plots) || "tree" %in% plots) {

    if ("character" %in% class(x))
      return(paste0("graph LR\n A[\"", x, "\"]") %>% DiagrammeR::DiagrammeR(.))

    ## avoid error when dec is NA or NULL
    if (is_not(dec)) dec <- 2

    df <- x$frame
    if (is.null(df)) {
      df <- x$model$frame ## make it easier to call from code
      type <- x$model$method
      nlabs <- labels(x$model, collapse = FALSE)
    } else {
      nlabs <- labels(x, collapse = FALSE)
      type <- x$method
    }

    if (nrow(df) == 1)
      return(paste0("graph LR\n A[Cannot graph singlenode tree]") %>% DiagrammeR::DiagrammeR(.))

    df$split1 <- nlabs[,1]
    df$split2 <- nlabs[,2]

    xlevs <- attr(x$model, "xlevels")
    if (length(xlevs) > 0 && labs) {
      for (i in names(xlevs)) {
        if (length(xlevs[[i]]) == 0) next

        ind <- which(df$var %in% i)
        splits <- data.frame(
          split1 = df[ind, "split1"],
          split2 = df[ind, "split2"]
        )

        ## in case an explanatory variables was not used
        if (nrow(splits) == 0) next

        lind <- data.frame(
          split1 = match(splits[["split1"]], letters),
          split2 = match(splits[["split2"]], letters)
        )
        for (j in 1:nrow(lind)) {
          if (is.na(lind[j, 1]) && is.na(lind[j, 2])) {
            splits[j,] <- paste0(":", splits[j,])
          } else if (is.na(lind[j, 1])) {
            splits[j,] <- xlevs[[i]][lind[j,2]] %>% c(paste0("!", .), .)
          } else if (is.na(lind[j, 2])) {
            splits[j,] <- xlevs[[i]][lind[j,1]] %>% c(., paste0("!", .))
          } else {
            splits[j,] <- xlevs[[i]][unlist(lind[j,])]
          }
        }
        df[ind, c("split1", "split2")] <- splits
      }
    }

    if (type == "class") {
      df$yval <- formatnr(df$yval2[,4], dec = dec, perc = TRUE)
      pre <- "p: "
    } else {
      df$yval <- round(df$yval, dec)
      pre <- "b: "
    }
    df$yval2 <- NULL

    df$id <- as.integer(rownames(df))
    df$to1 <- NA
    df$to2 <- NA
    non_leafs <- which(df$var != "<leaf>")
    df$to1[non_leafs] <- df$id[non_leafs + 1]
    df$to2[non_leafs] <- df$to1[non_leafs] + 1
    df <- gather_(df, "level", "to", c("to1", "to2"))

    df$to <- as.integer(df$to)
    df$edge <- ifelse (df$level == "to1", df$split1, df$split2) %>% {paste0("--- |", ., "|")}
    ## seems like only unicode letters are supported in mermaid at this time
    # df$edge <- ifelse (df$level == "to1", df$split1, df$split2) %>% {paste0("--- |", sub("^>", "\u2265",.), "|")}
    # df$edge <- iconv(df$edge, "UTF-8", "ASCII",sub="")
    non_leafs <- which(df$var != "<leaf>")
    df$from <- NA
    df$from[non_leafs] <- paste0("id",df$id[non_leafs],"[",df$var[non_leafs],"]")

    df$to_lab <- NA
    to_lab <- sapply(df$to[non_leafs], function(x) which(x == df$id))[1,]
    df$to_lab[non_leafs] <- paste0("id",df$to[non_leafs], "[", ifelse(df$var[to_lab] == "<leaf>","",paste0(df$var[to_lab],"<br>")), "n: ", formatnr(df$n[to_lab], dec = 0), "<br>", pre, df$yval[to_lab],"]")
    df <- na.omit(df)

    leafs <- paste0("id", setdiff(df$to, df$id))

    style <- paste0(
      "classDef default fill:none, bg:none, stroke-width:0px;
      classDef leaf fill:#9ACD32,stroke:#333,stroke-width:1px;
      class ", paste(leafs, collapse = ","), " leaf;")

    ttip <- df[1:(nrow(df)/2),] %>%  {paste0("click id", .$id, " callback \"n: ", formatnr(.$n, dec = 0), "<br>", pre, .$yval, "\"", collapse = "\n")}

    paste(paste0("graph ", orient), paste(paste0(df$from, df$edge, df$to_lab), collapse = "\n"), style, ttip, sep = "\n") %>%
      DiagrammeR::mermaid(., width = "100%", height = "100%")
  } else {
    plot_list <- list()
    if ("prune" %in% plots) {

      if (!is.null(x$unpruned))
        df <- data.frame(x$unpruned$cptable)
      else
        df <- data.frame(x$model$cptable)

      # df$CP <- sqrt(df$CP * c(Inf, head(df$CP, -1))) # %>% round(5)
      df$nsplit <- as.integer(df$nsplit + 1)
      ind1 <- min(which(df$xerror == min(df$xerror)))
      size1 <- df$nsplit[ind1]
      ind2 <- min(which(df$xerror < (df$xerror[ind1] + df$xstd[ind1])))
      size2 <- df$nsplit[ind2]

      p <- ggplot(data = df, aes_string(x = "nsplit", y = "xerror")) +
        geom_line() +
        geom_vline(xintercept = size1, linetype = "dashed") +
        geom_hline(yintercept = min(df$xerror), linetype = "dashed") +
        labs(
          title = "Evaluate tree pruning based on cross-validation",
          x = "Number of nodes", 
          y = "Relative error"
        )

      if (nrow(df) < 10) p <- p + scale_x_continuous(breaks = df$nsplit)

      footnote <- paste0("\nMinimum error achieved with ", size1, " nodes")
      ## http://stats.stackexchange.com/questions/13471/how-to-choose-the-number-of-splits-in-rpart

      ind2 <- min(which(df$xerror < (df$xerror[ind1] + df$xstd[ind1])))
      p <- p +
        geom_vline(xintercept = size2, linetype = "dotdash", color = "blue") +
        geom_hline(yintercept = df$xerror[ind1] + df$xstd[ind1], linetype = "dotdash", color = "blue")

      if (size2 < size1) 
        footnote <- paste0(footnote, ". Error from tree with ", size2, " nodes is within one std. of minimum")

      plot_list[["prune"]] <- p + labs(caption = footnote)
    }
    if ("imp" %in% plots) {

      imp <- x$model$variable.importance
      if (is.null(imp)) return("Variable importance information not available for singlenode tree")

      df <- data.frame(vars = names(imp), imp = imp / sum(imp)) %>%
        arrange_(.dots = "imp")
      df$vars <- factor(df$vars, levels = df$vars)

      plot_list[["imp"]] <-
        visualize(df, yvar = "imp", xvar = "vars", type = "bar", custom = TRUE) +
        labs(
          title = "Variable importance",
          x = "",
          y = "Importance"
        ) +
        coord_flip() +
        theme(axis.text.y = element_text(hjust = 0))
    }

    if (length(plot_list) > 0) {
      if (custom)
        if (length(plot_list) == 1) return(plot_list[[1]]) else return(plot_list)
          
      sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = 1)) %>%
        {if (shiny) . else print(.)}
    }
  }
}

#' Predict method for the crtree function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/crtree.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{crtree}}
#' @param pred_data Provide the name of a dataframe to generate predictions (e.g., "titanic"). The dataset must contain all columns used in the estimation
#' @param pred_cmd Generate predictions using a command. For example, `pclass = levels(pclass)` would produce predictions for the different levels of factor `pclass`. To add another variable use a `,` (e.g., `pclass = levels(pclass), age = seq(0,100,20)`)
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param se Logical that indicates if prediction standard errors should be calculated (default = FALSE)
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- crtree("titanic", "survived", c("pclass","sex"), lev = "Yes")
#' predict(result, pred_cmd = "pclass = levels(pclass)")
#' result <- crtree("titanic", "survived", "pclass", lev = "Yes")
#' predict(result, pred_data = "titanic") %>% head
#'
#' @seealso \code{\link{crtree}} to generate the result
#' @seealso \code{\link{summary.crtree}} to summarize results
#'
#' @export
predict.crtree <- function(object,
                           pred_data = "",
                           pred_cmd = "",
                           conf_lev = 0.95,
                           se = FALSE,
                           dec = 3,
                           ...) {

  if (is.character(object)) return(object)
  pfun <- function(model, pred, se, conf_lev) {
    pred_val <- try(sshhr(predict(model, pred)), silent = TRUE)

    if (!is(pred_val, 'try-error'))
      pred_val %<>% as.data.frame %>% select(1) %>% set_colnames("Prediction")

    pred_val
  }

  predict_model(object, pfun, "crtree.predict", pred_data, pred_cmd, conf_lev, se, dec)
}

#' Print method for predict.crtree
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#'
#' @export
print.crtree.predict <- function(x, ..., n = 10)
  print_predict_model(x, ..., n = n, header = "Classification and regression trees")

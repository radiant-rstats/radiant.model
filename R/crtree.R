#' Classification and regression trees based on the rpart package
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/crtree.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param rvar The response variable in the model
#' @param evar Explanatory variables in the model
#' @param type Model type (i.e., "classification" or "regression")
#' @param lev The level in the response variable defined as _success_
#' @param wts Weights to use in estimation
#' @param minsplit The minimum number of observations that must exist in a node in order for a split to be attempted.
#' @param minbucket the minimum number of observations in any terminal <leaf> node. If only one of minbucket or minsplit is specified, the code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate.
#' @param cp Minimum proportion of root node deviance required for split (default = 0.00001)
#' @param nodes Maximum size of tree in number of nodes to return. If equal to NA no pruning is done
#' @param K Number of folds use in cross-validation
#' @param seed Random seed used for cross-validation
#' @param split Splitting criterion to use (i.e., "gini" or "information")
#' @param prior Adjust the initial probability for the selected level (e.g., set to .5 in unbalanced samples)
#' @param adjprob Setting a prior will rescale the predicted probabilities. Set adjprob to TRUE to adjust the probabilities back to their original scale after estimation
#' @param cost Cost for each connection (e.g., email or mailing)
#' @param margin Margin on each customer purchase
#' @param check Optional estimation parameters (e.g., "standardize")
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list with all variables defined in crtree as an object of class tree
#'
#' @examples
#' crtree(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>% summary()
#' result <- crtree(titanic, "survived", c("pclass", "sex")) %>% summary()
#' result <- crtree(diamonds, "price", c("carat", "clarity"), type = "regression") %>% str()
#'
#' @seealso \code{\link{summary.crtree}} to summarize results
#' @seealso \code{\link{plot.crtree}} to plot results
#' @seealso \code{\link{predict.crtree}} for prediction
#'
#' @importFrom rpart rpart rpart.control prune.rpart
#'
#' @export
crtree <- function(
  dataset, rvar, evar, type = "", lev = "", wts = "None",
  minsplit = 2, minbucket = round(minsplit/3), cp = 0.001,
  nodes = NA, K = 10, seed = 1234, split = "gini", prior = NA,
  adjprob = TRUE, cost = NA, margin = NA, check = "", data_filter = ""
) {

  if (rvar %in% evar) {
    return("Response variable contained in the set of explanatory variables.\nPlease update model specification." %>%
      add_class("crtree"))
  }

  ## allow cp to be negative so full tree is built http://stackoverflow.com/q/24150058/1974918
  if (is_empty(cp)) {
    return("Please provide a complexity parameter to split the data." %>% add_class("crtree"))
  } else if (!is_empty(nodes) && nodes < 2) {
    return("The (maximum) number of nodes in the tree should be larger than or equal to 2." %>% add_class("crtree"))
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

  if (!is_empty(wts)) {
    if (exists("wtsname")) {
      wts <- dataset[[wtsname]]
      dataset <- select_at(dataset, .vars = base::setdiff(colnames(dataset), wtsname))
    }
    if (length(wts) != nrow(dataset)) {
      return(
        paste0("Length of the weights variable is not equal to the number of rows in the dataset (", format_nr(length(wts), dec = 0), " vs ", format_nr(nrow(dataset), dec = 0), ")") %>%
          add_class("crtree")
      )
    }
  }

  if (any(summarise_all(dataset, funs(does_vary)) == FALSE)) {
    return("One or more selected variables show no variation. Please select other variables." %>% add_class("crtree"))
  }

  rv <- dataset[[rvar]]

  if (type == "classification" && !is.factor(rv)) {
    dataset[[rvar]] <- as_factor(dataset[[rvar]])
  }

  if (is.factor(dataset[[rvar]])) {
    if (type == "regression") {
      return("Cannot estimate a regression when the response variable is of type factor." %>% add_class("crtree"))
    }

    if (lev == "") {
      lev <- levels(dataset[[rvar]])[1]
    } else {
      if (!lev %in% levels(dataset[[rvar]])) {
        return(paste0("Specified level is not a level in ", rvar) %>% add_class("crtree"))
      }

      dataset[[rvar]] <- factor(dataset[[rvar]], levels = unique(c(lev, levels(dataset[[rvar]]))))
    }

    type <- "classification"
    method <- "class"
  } else {
    type <- "regression"
    method <- "anova"
  }

  ## logicals would get < 0.5 and >= 0.5 otherwise
  ## also need to update data in predict_model
  ## so the correct type is used in prediction
  dataset <- mutate_if(dataset, is.logical, as.factor)

  ## standardize data ...
  if ("standardize" %in% check) {
    dataset <- scaledf(dataset, wts = wts)
  }

  vars <- evar
  ## in case : is used
  if (length(vars) < (ncol(dataset) - 1)) vars <- evar <- colnames(dataset)[-1]

  form <- paste(rvar, "~ . ")

  seed %>% gsub("[^0-9]", "", .) %>%
    {if (!is_empty(.)) set.seed(seed)}

  minsplit = ifelse(is_empty(minsplit), 2, minsplit)
  minbucket = ifelse(is_empty(minbucket), round(minsplit/3), minbucket)

  ## make max tree
  # http://stackoverflow.com/questions/24150058/rpart-doesnt-build-a-full-tree-problems-with-cp
  control <- rpart::rpart.control(
    cp = cp,
    xval = K,
    minsplit = minsplit,
    minbucket = minbucket,
  )

  parms <- list(split = split)
  # loss <- c(6,.5); cost <- .5, margin <- 6
  if (type == "classification") {
    ind <- if (which(lev %in% levels(dataset[[rvar]])) == 1) c(1, 2) else c(2, 1)

    if (!is_not(cost) && !is_not(margin)) {
      parms[["loss"]] <- c(as_numeric(margin), as_numeric(cost)) %>% .[ind] %>% {
        matrix(c(0, .[1], .[2], 0), byrow = TRUE, nrow = 2)
      }
    } else if (!is_empty(prior)) {
      if (!is.numeric(prior)) {
        return("Prior did not resolve to a numeric factor" %>% add_class("crtree"))
      } else if (prior > 1 || prior < 0) {
        return("Prior is not a valid probability" %>% add_class("crtree"))
      } else {
        ## prior is applied to the selected level
        parms[["prior"]] <- c(prior, 1 - prior) %>%
          .[ind]
      }
    }
  }

  model <- rpart::rpart(
    as.formula(form),
    data = dataset,
    method = method,
    parms = parms,
    weights = wts,
    control = control
  )

  if (!is_not(nodes)) {
    unpruned <- model
    if (nrow(model$frame) > 1) {
      cptab <- as.data.frame(model$cptable, stringsAsFactors = FALSE)
      cptab$nodes <- cptab$nsplit + 1
      ind <- max(which(cptab$nodes <= nodes))
      model <- sshhr(rpart::prune.rpart(model, cp = cptab$CP[ind]))
    }
  }

  ## rpart::rpart does not return residuals by default
  model$residuals <- residuals(model, type = "pearson")

  if (is_not(cost) && is_not(margin) &&
      !is_empty(prior) && !is_empty(adjprob)) {

    # org_frac <- mean(dataset[[rvar]] == lev)
    # over_frac <- prior
    p <- model$frame$yval2[, 4]
    bp <- mean(dataset[[rvar]] == lev)

    ## note that this adjustment will reset the prior in the print out
    ## to the original prior using the 'SAS approach'
    # model$frame$yval2[, 4] <- 1 / (1 + (1 / bp - 1) / (1 / prior - 1) * (1 / p - 1))
    # model$frame$yval2[, 5] <- 1 - model$frame$yval2[, 4]

    ## when prior = 0.5 can use pp <- p / (p + (1 - p) * (1 - bp) / bp)
    ## more generally, use Theorem 2 from "The Foundations of Cost-Sensitive Learning" by Charles Elkan
    ## in the equation below prior equivalent of b
    # model$frame$yval2[, 4] <- bp * (p - p * b) / (b - p * b + bp * p - b * bp)
    model$frame$yval2[, 4] <- bp * (p - p * prior) / (prior - p * prior + bp * p - prior * bp)
    model$frame$yval2[, 5] <- 1 - model$frame$yval2[, 4]
  }

  ## tree model object does not include the data by default
  model$model <- dataset

  ## passing on variable classes for plotting
  model$var_types <- sapply(dataset, class)

  rm(dataset) ## dataset not needed elsewhere

  as.list(environment()) %>% add_class(c("crtree", "model"))
}

#' Summary method for the crtree function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/crtree.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{crtree}}
#' @param prn Print tree in text form
#' @param cptab Print the cp table
#' @param modsum Print the model summary
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- crtree(titanic, "survived", c("pclass", "sex"), lev = "Yes")
#' summary(result)
#' result <- crtree(diamonds, "price", c("carat", "color"), type = "regression")
#' summary(result)
#'
#' @seealso \code{\link{crtree}} to generate results
#' @seealso \code{\link{plot.crtree}} to plot results
#' @seealso \code{\link{predict.crtree}} for prediction
#'
#' @export
summary.crtree <- function(
  object, prn = TRUE, cptab = FALSE,
  modsum = FALSE, ...
) {

  if (is.character(object)) return(object)

  if (object$type == "classification") {
    cat("Classification tree")
  } else {
    cat("Regression tree")
  }
  cat("\nData                 :", object$df_name)
  if (object$data_filter %>% gsub("\\s", "", .) != "") {
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
  cat("Complexity parameter :", object$cp, "\n")
  cat("Minimum observations :", object$minsplit, "\n")
  if (!is_not(object$nodes)) {
    max_nodes <- sum(object$unpruned$frame$var == "<leaf>")
    cat("Maximum nr. nodes    :", object$nodes, "out of", max_nodes, "\n")
  }
  if (!is_empty(object$cost) && !is_empty(object$margin) && object$type == "classification") {
    cat("Cost:Margin          :", object$cost, ":", object$margin, "\n")
    if (!is_empty(object$prior)) object$prior <- "Prior ignored when cost and margin set"
  }
  if (!is_empty(object$prior) && object$type == "classification") {
    cat("Priors               :", object$prior, "\n")
    cat("Adjusted prob.       :", object$adjprob, "\n")
  }
  if (!is_empty(object$wts, "None") && class(object$wts) == "integer") {
    cat("Nr obs               :", format_nr(sum(object$wts), dec = 0), "\n\n")
  } else {
    cat("Nr obs               :", format_nr(length(object$rv), dec = 0), "\n\n")
  }

  ## extra output
  if (cptab)
    print(object$model$cptable)

  if (modsum) {
    print(summary(object$model))
  } else if (prn) {
    cat(paste0(capture.output(print(object$model))[c(-1, -2)], collapse = "\n"))
  }
}

#' Plot method for the crtree function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/crtree.html} for an example in Radiant. The standard tree plot used by by the rpart package can be generated by \code{plot.rpart(result$model)}. See \code{\link{plot.rpart}} for additional details.
#'
#' @param x Return value from \code{\link{crtree}}
#' @param plots Plots to produce for the specified rpart tree. "tree" shows a tree diagram. "prune" shows a line graph to evaluate appropriate tree pruning. "imp" shows a variable importance plot
#' @param orient Plot orientation for tree: LR for vertical and TD for horizontal
#' @param width Plot width in pixels for tree (default is "900px")
#' @param labs Use factor labels in plot (TRUE) or revert to default letters used by tree (FALSE)
#' @param dec Decimal places to round results to
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- crtree(titanic, "survived", c("pclass", "sex"), lev = "Yes")
#' plot(result)
#' result <- crtree(diamonds, "price", c("carat", "clarity", "cut"))
#' plot(result, plots = "prune")
#' result <- crtree(dvd, "buy", c("coupon", "purch", "last"), cp = .01)
#' plot(result, plots = "imp")
#'
#' @importFrom DiagrammeR DiagrammeR mermaid
#'
#' @seealso \code{\link{crtree}} to generate results
#' @seealso \code{\link{summary.crtree}} to summarize results
#' @seealso \code{\link{predict.crtree}} for prediction
#'
#' @export
plot.crtree <- function(
  x, plots = "tree", orient = "LR",
  width = "900px", labs = TRUE, dec = 2,
  shiny = FALSE, custom = FALSE, ...
) {

  if (is_empty(plots) || "tree" %in% plots) {
    if ("character" %in% class(x)) {
      return(paste0("graph LR\n A[\"", x, "\"]") %>% DiagrammeR::DiagrammeR(.))
    }

    ## avoid error when dec is NA or NULL
    if (is_not(dec)) dec <- 2

    df <- x$frame
    if (is.null(df)) {
      df <- x$model$frame ## make it easier to call from code
      type <- x$model$method
      nlabs <- labels(x$model, minlength = 0, collapse = FALSE)
    } else {
      nlabs <- labels(x, minlength = 0, collapse = FALSE)
      type <- x$method
    }

    if (nrow(df) == 1) {
      return(paste0("graph LR\n A[Graph unavailable for single node tree]") %>% DiagrammeR::DiagrammeR(.))
    }

    if (type == "class") {
      df$yval <- format_nr(df$yval2[, 4], dec = dec, perc = TRUE)
      pre <- "<b>p:</b> "
    } else {
      df$yval <- round(df$yval, dec)
      pre <- "<b>b:</b> "
    }
    df$yval2 <- NULL

    df$id <- as.integer(rownames(df))
    df$to1 <- NA
    df$to2 <- NA
    non_leafs <- which(df$var != "<leaf>")
    df$to1[non_leafs] <- df$id[non_leafs + 1]
    df$to2[non_leafs] <- df$to1[non_leafs] + 1
    df <- gather(df, "level", "to", !! c("to1", "to2"))

    df$split1 <- nlabs[, 1]
    df$split2 <- nlabs[, 2]

    isInt <- x$model$var_types %>% {names(.)[. == "integer"]}
    if (length(isInt) > 0) {
      # inspired by https://stackoverflow.com/a/35556288/1974918
      int_labs <- function(x) {
        paste(
          gsub("(>=|<)\\s*(-{0,1}[0-9]+.*)", "\\1", x),
          gsub("(>=|<)\\s*(-{0,1}[0-9]+.*)", "\\2", x) %>%
            as.numeric() %>%
            ceiling(.)
        )
      }
      int_ind <- df$var %in% isInt
      df[int_ind, "split1"] %<>% int_labs()
      df[int_ind, "split2"] %<>% int_labs()
    }

    df$split1_full <- df$split1
    df$split2_full <- df$split2

    bnr <- 20
    df$split1 <- df$split1 %>% ifelse(nchar(.) > bnr, paste0(strtrim(., bnr), " ..."), .)
    df$split2 <- df$split2 %>% ifelse(nchar(.) > bnr, paste0(strtrim(., bnr), " ..."), .)

    df$to <- as.integer(df$to)
    df$edge <- ifelse(df$level == "to1", df$split1, df$split2) %>% {
      paste0(" --- |", ., "|")
    }
    ## seems like only unicode letters are supported in mermaid at this time
    # df$edge <- ifelse (df$level == "to1", df$split1, df$split2) %>% {paste0("--- |", sub("^>", "\u2265",.), "|")}
    # df$edge <- iconv(df$edge, "UTF-8", "ASCII",sub="")
    non_leafs <- which(df$var != "<leaf>")
    df$from <- NA
    df$from[non_leafs] <- paste0("id", df$id[non_leafs], "[", df$var[non_leafs], "]")

    df$to_lab <- NA
    to_lab <- sapply(df$to[non_leafs], function(x) which(x == df$id))[1, ]
    df$to_lab[non_leafs] <- paste0("id", df$to[non_leafs], "[", ifelse(df$var[to_lab] == "<leaf>", "", paste0(df$var[to_lab], "<br>")), "<b>n:</b> ", format_nr(df$n[to_lab], dec = 0), "<br>", pre, df$yval[to_lab], "]")
    df <- na.omit(df)

    leafs <- paste0("id", base::setdiff(df$to, df$id))

    ## still need the below setup to keep the "chance" class
    ## when a decision analysis plot is in the report
    # style <- paste0(
    #   "classDef default fill:none, bg:none, stroke-width:0px;
    #   classDef leaf fill:#9ACD32,stroke:#333,stroke-width:1px;
    #   class ", paste(leafs, collapse = ","), " leaf;"
    # )

    ## still need this to keep the "chance" class
    ## when a decision analysis plot is in the report
    style <- paste0(
      "classDef default fill:none, bg:none, stroke-width:0px;
      classDef leaf fill:#9ACD32,stroke:#333,stroke-width:1px;
      classDef chance fill:#FF8C00,stroke:#333,stroke-width:1px;
      classDef chance_with_cost fill:#FF8C00,stroke:#333,stroke-width:3px,stroke-dasharray:4,5;
      classDef decision fill:#9ACD32,stroke:#333,stroke-width:1px;
      classDef decision_with_cost fill:#9ACD32,stroke:#333,stroke-width:3px,stroke-dasharray:4,5;
      class ", paste(leafs, collapse = ","), " leaf;"
    )

    ## check orientation for branch labels
    brn <- if (orient %in% c("LR", "RL")) {
      c("top", "bottom")
    } else {
      c("left", "right")
    }
    brn <- paste0("<b>", brn, ":</b> ")

    ## don't print full labels that don't add information
    df[df$split1_full == df$split1 & df$split2_full == df$split2, c("split1_full", "split2_full")] <- ""
    df$split1_full <- ifelse(df$split1_full == "", "", paste0("<br>", brn[1], gsub(",", ", ", df$split1_full)))
    df$split2_full <- ifelse(df$split2_full == "", "", paste0("<br>", brn[2], gsub(",", ", ", df$split2_full)))

    ttip_ind <- 1:(nrow(df) / 2)
    ttip <- df[ttip_ind, , drop = FALSE] %>%
      {paste0("click id", .$id, " callback \"<b>n:</b> ", format_nr(.$n, dec = 0), "<br>", pre, .$yval, .$split1_full, .$split2_full, "\"", collapse = "\n")}

    ## try to link a tooltip directly to an edge using mermaid
    ## see https://github.com/rich-iannone/DiagrammeR/issues/267
    # ttip_lev <- filter(df[-ttip_ind,], split1_full != "")
    # if (nrow(ttip_lev) == 0) {
    #   ttip_lev <- ""
    # } else {
    #   ttip_lev <- paste0("click id", ttip_lev$id, " callback \"", ttip_lev$split1_full, "\"", collapse = "\n")
    # }

    paste(paste0("graph ", orient), paste(paste0(df$from, df$edge, df$to_lab), collapse = "\n"), style, ttip, sep = "\n") %>%
      # DiagrammeR::mermaid(., width = "100%", height = "100%")
      # DiagrammeR::mermaid(., width = width, height = "100%")
      DiagrammeR::mermaid(., width = width, height = "100%")
  } else {
    plot_list <- list()
    if ("prune" %in% plots) {
      if (!is.null(x$unpruned)) {
        df <- data.frame(x$unpruned$cptable, stringsAsFactors = FALSE)
      } else {
        df <- data.frame(x$model$cptable, stringsAsFactors = FALSE)
      }

      if (nrow(df) < 2) {
        return("Evaluation of tree pruning not available for single node tree")
      }

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

      if (size2 < size1) {
        footnote <- paste0(footnote, ". Error from tree with ", size2, " nodes is within one std. of minimum")
      }

      plot_list[["prune"]] <- p + labs(caption = footnote)
    }
    if ("imp" %in% plots) {
      imp <- x$model$variable.importance
      if (is.null(imp)) return("Variable importance information not available for singlenode tree")

      df <- data.frame(
          vars = names(imp),
          imp = imp / sum(imp),
          stringsAsFactors = FALSE
        ) %>%
        arrange_at(.vars = "imp")
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
      if (custom) {
        if (length(plot_list) == 1) {
          return(plot_list[[1]])
        } else {
          return(plot_list)
        }
      }

      sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = 1)) %>% {
        if (shiny) . else print(.)
      }
    }
  }
}

#' Predict method for the crtree function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/crtree.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{crtree}}
#' @param pred_data Provide the dataframe to generate predictions (e.g., titanic). The dataset must contain all columns used in the estimation
#' @param pred_cmd Generate predictions using a command. For example, `pclass = levels(pclass)` would produce predictions for the different levels of factor `pclass`. To add another variable, create a vector of prediction strings, (e.g., c('pclass = levels(pclass)', 'age = seq(0,100,20)')
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param se Logical that indicates if prediction standard errors should be calculated (default = FALSE)
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- crtree(titanic, "survived", c("pclass", "sex"), lev = "Yes")
#' predict(result, pred_cmd = "pclass = levels(pclass)")
#' result <- crtree(titanic, "survived", "pclass", lev = "Yes")
#' predict(result, pred_data = titanic) %>% head()
#'
#' @seealso \code{\link{crtree}} to generate the result
#' @seealso \code{\link{summary.crtree}} to summarize results
#'
#' @export
predict.crtree <- function(
  object, pred_data = NULL, pred_cmd = "", conf_lev = 0.95,
  se = FALSE, dec = 3, ...
) {

  if (is.character(object)) return(object)
  if (is.data.frame(pred_data)) {
    df_name <- deparse(substitute(pred_data))
  } else {
    df_name <- pred_data
  }

  pfun <- function(model, pred, se, conf_lev) {
    pred_val <- try(sshhr(predict(model, pred)), silent = TRUE)

    if (!is(pred_val, "try-error")) {
      pred_val %<>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        select(1) %>%
        set_colnames("Prediction")
    }

    pred_val
  }

  predict_model(object, pfun, "crtree.predict", pred_data, pred_cmd, conf_lev, se, dec) %>%
    set_attr("pred_data", df_name)
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

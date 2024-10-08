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
#' @param cp Minimum proportion of root node deviance required for split (default = 0.001)
#' @param pcp Complexity parameter to use for pruning
#' @param nodes Maximum size of tree in number of nodes to return
#' @param K Number of folds use in cross-validation
#' @param seed Random seed used for cross-validation
#' @param split Splitting criterion to use (i.e., "gini" or "information")
#' @param prior Adjust the initial probability for the selected level (e.g., set to .5 in unbalanced samples)
#' @param adjprob Setting a prior will rescale the predicted probabilities. Set adjprob to TRUE to adjust the probabilities back to their original scale after estimation
#' @param cost Cost for each treatment (e.g., mailing)
#' @param margin Margin associated with a successful treatment (e.g., a purchase)
#' @param check Optional estimation parameters (e.g., "standardize")
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param arr Expression to arrange (sort) the data on (e.g., "color, desc(price)")
#' @param rows Rows to select from the specified dataset
#' @param envir Environment to extract data from
#'
#' @return A list with all variables defined in crtree as an object of class tree
#'
#' @examples
#' crtree(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>% summary()
#' result <- crtree(titanic, "survived", c("pclass", "sex")) %>% summary()
#' result <- crtree(diamonds, "price", c("carat", "clarity"), type = "regression") %>% str()
#' @seealso \code{\link{summary.crtree}} to summarize results
#' @seealso \code{\link{plot.crtree}} to plot results
#' @seealso \code{\link{predict.crtree}} for prediction
#'
#' @importFrom rpart rpart rpart.control prune.rpart
#'
#' @export
crtree <- function(dataset, rvar, evar, type = "", lev = "", wts = "None",
                   minsplit = 2, minbucket = round(minsplit / 3), cp = 0.001,
                   pcp = NA, nodes = NA, K = 10, seed = 1234, split = "gini",
                   prior = NA, adjprob = TRUE, cost = NA, margin = NA, check = "",
                   data_filter = "", arr = "", rows = NULL, envir = parent.frame()) {
  if (rvar %in% evar) {
    return("Response variable contained in the set of explanatory variables.\nPlease update model specification." %>%
      add_class("crtree"))
  }

  ## allow cp to be negative so full tree is built http://stackoverflow.com/q/24150058/1974918
  if (is.empty(cp)) {
    return("Please provide a complexity parameter to split the data." %>% add_class("crtree"))
  } else if (!is.empty(nodes) && nodes < 2) {
    return("The (maximum) number of nodes in the tree should be larger than or equal to 2." %>% add_class("crtree"))
  }

  vars <- c(rvar, evar)

  if (is.empty(wts, "None")) {
    wts <- NULL
  } else if (is_string(wts)) {
    wtsname <- wts
    vars <- c(rvar, evar, wtsname)
  }

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, vars, filt = data_filter, arr = arr, rows = rows, envir = envir)

  if (!is.empty(wts)) {
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

  not_vary <- colnames(dataset)[summarise_all(dataset, does_vary) == FALSE]
  if (length(not_vary) > 0) {
    return(paste0("The following variable(s) show no variation. Please select other variables.\n\n** ", paste0(not_vary, collapse = ", "), " **") %>%
      add_class("crtree"))
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
    dataset <- scale_df(dataset, wts = wts)
  }

  vars <- evar
  ## in case : is used
  if (length(vars) < (ncol(dataset) - 1)) vars <- evar <- colnames(dataset)[-1]

  form <- paste(rvar, "~ . ")

  seed %>%
    gsub("[^0-9]", "", .) %>%
    (function(x) if (!is.empty(x)) set.seed(seed))

  minsplit <- ifelse(is.empty(minsplit), 2, minsplit)
  minbucket <- ifelse(is.empty(minbucket), round(minsplit / 3), minbucket)

  ## make max tree
  # http://stackoverflow.com/questions/24150058/rpart-doesnt-build-a-full-tree-problems-with-cp
  control <- rpart::rpart.control(
    cp = cp,
    xval = K,
    minsplit = minsplit,
    minbucket = minbucket,
  )

  parms <- list(split = split)
  if (type == "classification") {
    ind <- if (which(lev %in% levels(dataset[[rvar]])) == 1) c(1, 2) else c(2, 1)
    if (!is.empty(prior) && !is_not(cost) && !is_not(cost)) {
      return("Choose either a prior or cost and margin values but not both.\nPlease adjust your settings and try again" %>% add_class("crtree"))
    }

    if (!is_not(cost) && !is_not(margin)) {
      loss2 <- as_numeric(cost)
      loss1 <- as_numeric(margin) - loss2

      if (loss1 <= 0) {
        return("Cost must be smaller than the specied margin.\nPlease adjust the settings and try again" %>% add_class("crtree"))
      } else if (loss2 <= 0) {
        return("Cost must be larger than zero.\nPlease adjust the settings and try again" %>% add_class("crtree"))
      } else {
        parms[["loss"]] <- c(loss1, loss2) %>%
          .[ind] %>%
          {
            matrix(c(0, .[1], .[2], 0), byrow = TRUE, nrow = 2)
          }
      }
    } else if (!is.empty(prior)) {
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

  ## using an input list with do.call ensure that a full "call" is available for cross-validation
  crtree_input <- list(
    formula = as.formula(form),
    data = dataset,
    method = method,
    parms = parms,
    weights = wts,
    control = control
  )

  model <- do.call(rpart::rpart, crtree_input)

  if (!is_not(nodes)) {
    unpruned <- model
    if (nrow(model$frame) > 1) {
      cptab <- as.data.frame(model$cptable, stringsAsFactors = FALSE)
      cptab$nodes <- cptab$nsplit + 1
      ind <- max(which(cptab$nodes <= nodes))
      model <- sshhr(rpart::prune.rpart(model, cp = cptab$CP[ind]))
    }
  } else if (!is_not(pcp)) {
    unpruned <- model
    if (nrow(model$frame) > 1) {
      model <- sshhr(rpart::prune.rpart(model, cp = pcp))
    }
  }

  ## rpart::rpart does not return residuals by default
  model$residuals <- residuals(model, type = "pearson")

  if (is_not(cost) && is_not(margin) &&
    !is.empty(prior) && !is.empty(adjprob)) {

    ## when prior = 0.5 can use pp <- p / (p + (1 - p) * (1 - bp) / bp)
    ## more generally, use Theorem 2 from "The Foundations of Cost-Sensitive Learning" by Charles Elkan
    ## in the equation below prior equivalent of b
    p <- model$frame$yval2[, 4]
    bp <- mean(dataset[[rvar]] == lev)
    model$frame$yval2[, 4] <- bp * (p - p * prior) / (prior - p * prior + bp * p - prior * bp)
    model$frame$yval2[, 5] <- 1 - model$frame$yval2[, 4]
  }

  ## tree model object does not include the data by default
  model$model <- dataset

  ## passing on variable classes for plotting
  model$var_types <- sapply(dataset, class)

  rm(dataset, envir) ## dataset not needed elsewhere

  as.list(environment()) %>% add_class(c("crtree", "model"))
}

#' Summary method for the crtree function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/crtree.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{crtree}}
#' @param prn Print tree in text form
#' @param splits Print the tree splitting metrics used
#' @param cptab Print the cp table
#' @param modsum Print the model summary
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- crtree(titanic, "survived", c("pclass", "sex"), lev = "Yes")
#' summary(result)
#' result <- crtree(diamonds, "price", c("carat", "color"), type = "regression")
#' summary(result)
#' @seealso \code{\link{crtree}} to generate results
#' @seealso \code{\link{plot.crtree}} to plot results
#' @seealso \code{\link{predict.crtree}} for prediction
#'
#' @export
summary.crtree <- function(object, prn = TRUE, splits = FALSE, cptab = FALSE, modsum = FALSE, ...) {
  if (is.character(object)) {
    return(object)
  }

  if (object$type == "classification") {
    cat("Classification tree")
  } else {
    cat("Regression tree")
  }
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
  if (!is.empty(object$cost) && !is.empty(object$margin) && object$type == "classification") {
    cat("Cost:Margin          :", object$cost, ":", object$margin, "\n")
    if (!is.empty(object$prior)) object$prior <- "Prior ignored when cost and margin set"
  }
  if (!is.empty(object$prior) && object$type == "classification") {
    cat("Priors               :", object$prior, "\n")
    cat("Adjusted prob.       :", object$adjprob, "\n")
  }
  if (!is.empty(object$wts, "None") && inherits(object$wts, "integer")) {
    cat("Nr obs               :", format_nr(sum(object$wts), dec = 0), "\n\n")
  } else {
    cat("Nr obs               :", format_nr(length(object$rv), dec = 0), "\n\n")
  }

  ## extra output
  if (splits) {
    print(object$model$split)
  }

  if (cptab) {
    print(object$model$cptable)
  }

  if (modsum) {
    object$model$call <- NULL
    print(summary(object$model))
  } else if (prn) {
    cat(paste0(capture.output(print(object$model))[c(-1, -2)], collapse = "\n"))
  }
}

#' Plot method for the crtree function
#'
#' @details Plot a decision tree using mermaid, permutation plots , prediction plots, or partial dependence plots. For regression trees, a residual dashboard can be plotted. See \url{https://radiant-rstats.github.io/docs/model/crtree.html} for an example in Radiant.
#'
#' @param x Return value from \code{\link{crtree}}
#' @param plots Plots to produce for the specified rpart tree. "tree" shows a tree diagram. "prune" shows a line graph to evaluate appropriate tree pruning. "imp" shows a variable importance plot
#' @param orient Plot orientation for tree: LR for vertical and TD for horizontal
#' @param width Plot width in pixels for tree (default is "900px")
#' @param labs Use factor labels in plot (TRUE) or revert to default letters used by tree (FALSE)
#' @param nrobs Number of data points to show in dashboard scatter plots (-1 for all)
#' @param dec Decimal places to round results to
#' @param incl Which variables to include in a coefficient plot or PDP plot
#' @param incl_int Which interactions to investigate in PDP plots
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{https://ggplot2.tidyverse.org} for options.
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
#' @importFrom rlang .data
#'
#' @seealso \code{\link{crtree}} to generate results
#' @seealso \code{\link{summary.crtree}} to summarize results
#' @seealso \code{\link{predict.crtree}} for prediction
#'
#' @export
plot.crtree <- function(x, plots = "tree", orient = "LR",
                        width = "900px", labs = TRUE,
                        nrobs = Inf, dec = 2,
                        incl = NULL, incl_int = NULL,
                        shiny = FALSE, custom = FALSE, ...) {
  if (is.empty(plots) || "tree" %in% plots) {
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
    df <- gather(df, "level", "to", !!c("to1", "to2"))

    df$split1 <- nlabs[, 1]
    df$split2 <- nlabs[, 2]

    isInt <- x$model$var_types %>%
      (function(x) names(x)[x == "integer"])
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
    df$edge <- ifelse(df$level == "to1", df$split1, df$split2) %>%
      (function(x) paste0(" --- |", x, "|"))
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
      {
        paste0("click id", .$id, " callback \"<b>n:</b> ", format_nr(.$n, dec = 0), "<br>", pre, .$yval, .$split1_full, .$split2_full, "\"", collapse = "\n")
      }

    ## try to link a tooltip directly to an edge using mermaid
    ## see https://github.com/rich-iannone/DiagrammeR/issues/267
    # ttip_lev <- filter(df[-ttip_ind,], split1_full != "")
    # if (nrow(ttip_lev) == 0) {
    #   ttip_lev <- ""
    # } else {
    #   ttip_lev <- paste0("click id", ttip_lev$id, " callback \"", ttip_lev$split1_full, "\"", collapse = "\n")
    # }

    paste(paste0("graph ", orient), paste(paste0(df$from, df$edge, df$to_lab), collapse = "\n"), style, ttip, sep = "\n") %>%
      DiagrammeR::mermaid(., width = width, height = "100%")
  } else {
    if ("character" %in% class(x)) {
      return(x)
    }
    plot_list <- list()
    nrCol <- 1
    if ("prune" %in% plots) {
      if (is.null(x$unpruned)) {
        df <- data.frame(x$model$cptable, stringsAsFactors = FALSE)
      } else {
        df <- data.frame(x$unpruned$cptable, stringsAsFactors = FALSE)
      }

      if (nrow(df) < 2) {
        return("Evaluation of tree pruning not available for single node tree")
      }

      df$CP <- sqrt(df$CP * c(Inf, head(df$CP, -1))) %>% round(5)
      df$nsplit <- as.integer(df$nsplit + 1)
      ind1 <- min(which(df$xerror == min(df$xerror)))
      size1 <- c(df$nsplit[ind1], df$CP[ind1])
      ind2 <- min(which(df$xerror < (df$xerror[ind1] + df$xstd[ind1])))
      size2 <- c(df$nsplit[ind2], df$CP[ind2])

      p <- ggplot(data = df, aes(x = .data$nsplit, y = .data$xerror)) +
        geom_line() +
        geom_vline(xintercept = size1[1], linetype = "dashed") +
        geom_hline(yintercept = min(df$xerror), linetype = "dashed") +
        labs(
          title = "Evaluate tree pruning based on cross-validation",
          x = "Number of nodes",
          y = "Relative error"
        )
      if (nrow(df) < 10) p <- p + scale_x_continuous(breaks = df$nsplit)

      ## http://stats.stackexchange.com/questions/13471/how-to-choose-the-number-of-splits-in-rpart
      if (size1[1] == Inf) size1[2] <- NA
      footnote <- paste0("\nMinimum error achieved at prune complexity ", format(size1[2], scientific = FALSE), " (", size1[1], " nodes)")
      ind2 <- min(which(df$xerror < (df$xerror[ind1] + df$xstd[ind1])))
      p <- p +
        geom_vline(xintercept = size2[1], linetype = "dotdash", color = "blue") +
        geom_hline(yintercept = df$xerror[ind1] + df$xstd[ind1], linetype = "dotdash", color = "blue")

      if (size2[1] < size1[1]) {
        footnote <- paste0(footnote, ".\nError at pruning complexity ", format(size2[2], scientific = FALSE), " (", size2[1], " nodes) is within one std. of minimum")
      }

      plot_list[["prune"]] <- p + labs(caption = footnote)
    }

    if ("imp" %in% plots) {
      imp <- x$model$variable.importance
      if (is.null(imp)) {
        return("Variable importance information not available for singlenode tree")
      }

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

    if ("vip" %in% plots) {
      # imp <- x$model$variable.importance
      # if (is.null(imp)) {
      #   return("Variable importance information not available for singlenode tree")
      # } else {
      vi_scores <- varimp(x)
      plot_list[["vip"]] <-
        visualize(vi_scores, yvar = "Importance", xvar = "Variable", type = "bar", custom = TRUE) +
        labs(
          title = "Permutation Importance",
          x = NULL,
          y = ifelse(x$type == "regression", "Importance (R-square decrease)", "Importance (AUC decrease)")
        ) +
        coord_flip() +
        theme(axis.text.y = element_text(hjust = 0))
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
      if (length(incl) > 0 || length(incl_int) > 0) {
        plot_list <- pdp_plot(x, plot_list, incl, incl_int, ...)
      } else {
        return("Select one or more variables to generate Partial Dependence Plots")
      }
    }

    if (x$type == "regression" && "dashboard" %in% plots) {
      plot_list <- plot.regress(x, plots = "dashboard", lines = "line", nrobs = nrobs, custom = TRUE)
      nrCol <- 2
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
#' @param envir Environment to extract data from
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- crtree(titanic, "survived", c("pclass", "sex"), lev = "Yes")
#' predict(result, pred_cmd = "pclass = levels(pclass)")
#' result <- crtree(titanic, "survived", "pclass", lev = "Yes")
#' predict(result, pred_data = titanic) %>% head()
#' @seealso \code{\link{crtree}} to generate the result
#' @seealso \code{\link{summary.crtree}} to summarize results
#'
#' @export
predict.crtree <- function(object, pred_data = NULL, pred_cmd = "",
                           conf_lev = 0.95, se = FALSE, dec = 3,
                           envir = parent.frame(), ...) {
  if (is.character(object)) {
    return(object)
  }
  if (is.data.frame(pred_data)) {
    df_name <- deparse(substitute(pred_data))
  } else {
    df_name <- pred_data
  }

  pfun <- function(model, pred, se, conf_lev) {
    pred_val <- try(sshhr(predict(model, pred)), silent = TRUE)

    if (!inherits(pred_val, "try-error")) {
      pred_val %<>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        select(1) %>%
        set_colnames("Prediction")
    }

    pred_val
  }

  predict_model(object, pfun, "crtree.predict", pred_data, pred_cmd, conf_lev, se, dec, envir = envir) %>%
    set_attr("radiant_pred_data", df_name)
}

#' Print method for predict.crtree
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#'
#' @export
print.crtree.predict <- function(x, ..., n = 10) {
  print_predict_model(x, ..., n = n, header = "Classification and regression trees")
}

#' Cross-validation for Classification and Regression Trees
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/crtree.html} for an example in Radiant
#'
#' @param object Object of type "rpart" or "crtree" to use as a starting point for cross validation
#' @param K Number of cross validation passes to use
#' @param repeats Number of times to repeat the K cross-validation steps
#' @param cp Complexity parameter used when building the (e.g., 0.0001)
#' @param pcp Complexity parameter to use for pruning
#' @param seed Random seed to use as the starting point
#' @param trace Print progress
#' @param fun Function to use for model evaluation (e.g., auc for classification or RMSE for regression)
#' @param ... Additional arguments to be passed to 'fun'
#'
#' @return A data.frame sorted by the mean, sd, min, and max of the performance metric
#'
#' @seealso \code{\link{crtree}} to generate an initial model that can be passed to cv.crtree
#' @seealso \code{\link{Rsq}} to calculate an R-squared measure for a regression
#' @seealso \code{\link{RMSE}} to calculate the Root Mean Squared Error for a regression
#' @seealso \code{\link{MAE}} to calculate the Mean Absolute Error for a regression
#' @seealso \code{\link{auc}} to calculate the area under the ROC curve for classification
#' @seealso \code{\link{profit}} to calculate profits for classification at a cost/margin threshold
#'
#' @importFrom rpart prune.rpart
#' @importFrom shiny getDefaultReactiveDomain withProgress incProgress
#'
#' @examples
#' \dontrun{
#' result <- crtree(dvd, "buy", c("coupon", "purch", "last"))
#' cv.crtree(result, cp = 0.0001, pcp = seq(0, 0.01, length.out = 11))
#' cv.crtree(result, cp = 0.0001, pcp = c(0, 0.001, 0.002), fun = profit, cost = 1, margin = 5)
#' result <- crtree(diamonds, "price", c("carat", "color", "clarity"), type = "regression", cp = 0.001)
#' cv.crtree(result, cp = 0.001, pcp = seq(0, 0.01, length.out = 11), fun = MAE)
#' }
#'
#' @export
cv.crtree <- function(object, K = 5, repeats = 1, cp, pcp = seq(0, 0.01, length.out = 11), seed = 1234, trace = TRUE, fun, ...) {
  if (inherits(object, "crtree")) object <- object$model
  if (inherits(object, "rpart")) {
    dv <- as.character(object$call$formula[[2]])
    m <- eval(object$call[["data"]])
    if (is.numeric(m[[dv]])) {
      type <- "regression"
    } else {
      type <- "classification"
      if (is.factor(m[[dv]])) {
        lev <- levels(m[[dv]])[1]
      } else if (is.logical(m[[dv]])) {
        lev <- TRUE
      } else {
        stop("The level to use for classification is not clear. Use a factor of logical as the response variable")
      }
    }
  } else {
    stop("The model object does not seems to be a decision tree")
  }

  set.seed(seed)
  if (missing(cp)) cp <- object$call$control$cp
  tune_grid <- expand.grid(cp = cp, pcp = pcp)
  out <- data.frame(mean = NA, std = NA, min = NA, max = NA, cp = tune_grid[["cp"]], pcp = tune_grid[["pcp"]])

  if (missing(fun)) {
    if (type == "classification") {
      fun <- radiant.model::auc
      cn <- "AUC (mean)"
    } else {
      fun <- radiant.model::RMSE
      cn <- "RMSE (mean)"
    }
  } else {
    cn <- glue("{deparse(substitute(fun))} (mean)")
  }

  if (length(shiny::getDefaultReactiveDomain()) > 0) {
    trace <- FALSE
    incProgress <- shiny::incProgress
    withProgress <- shiny::withProgress
  } else {
    incProgress <- function(...) {}
    withProgress <- function(...) list(...)[["expr"]]
  }

  nitt <- nrow(tune_grid)
  withProgress(message = "Running cross-validation (crtree)", value = 0, {
    for (i in seq_len(nitt)) {
      perf <- double(K * repeats)
      object$call[["cp"]] <- tune_grid[i, "cp"]
      if (trace) cat("Working on cp", format(tune_grid[i, "cp"], scientific = FALSE), "pcp", format(tune_grid[i, "pcp"], scientific = FALSE), "\n")
      for (j in seq_len(repeats)) {
        rand <- sample(K, nrow(m), replace = TRUE)
        for (k in seq_len(K)) {
          object$call[["data"]] <- quote(m[rand != k, , drop = FALSE])
          pred <- try(rpart::prune(eval(object$call), tune_grid[i, "pcp"]), silent = TRUE)
          if (inherits(pred, "try-error")) next
          if (length(object$call$parms$prior) > 0) {
            pred <- prob_adj(pred, object$call$parms$prior[1], mean(m[rand != k, dv, drop = FALSE] == lev))
          }
          pred <- try(predict(pred, m[rand == k, , drop = FALSE]), silent = TRUE)
          if (inherits(pred, "try-error")) next

          if (type == "classification") {
            if (missing(...)) {
              perf[k + (j - 1) * K] <- fun(pred[, lev], unlist(m[rand == k, dv]), lev)
            } else {
              perf[k + (j - 1) * K] <- fun(pred[, lev], unlist(m[rand == k, dv]), lev, ...)
            }
          } else {
            if (missing(...)) {
              perf[k + (j - 1) * K] <- fun(pred, unlist(m[rand == k, dv]))
            } else {
              perf[k + (j - 1) * K] <- fun(pred, unlist(m[rand == k, dv]), ...)
            }
          }
        }
      }
      out[i, 1:4] <- c(mean(perf, na.rm = TRUE), sd(perf, na.rm = TRUE), min(perf, na.rm = TRUE), max(perf, na.rm = TRUE))
      incProgress(1 / nitt, detail = paste("\nCompleted run", i, "out of", nitt))
    }
  })

  if (type == "classification") {
    out <- arrange(out, desc(mean))
  } else {
    out <- arrange(out, mean)
  }

  ## show evaluation metric in column name
  colnames(out)[1] <- cn

  object$call[["cp"]] <- out[1, "cp"]
  object$call[["data"]] <- m
  object <- rpart::prune(eval(object$call), out[1, "pcp"])
  cat("\nGiven the provided tuning grid, the pruning complexity parameter\nshould be set to", out[1, "pcp"], "or the number of nodes set to", max(object$cptable[, "nsplit"]) + 1, "\n")
  out
}

prob_adj <- function(mod, prior, bp) {
  p <- mod$frame$yval2[, 4]
  mod$frame$yval2[, 4] <- bp * (p - p * prior) / (prior - p * prior + bp * p - prior * bp)
  mod$frame$yval2[, 5] <- 1 - mod$frame$yval2[, 4]
  mod
}
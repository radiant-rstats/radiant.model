#' Evaluate the performance of different (binary) classification models
#'
#' @details Evaluate different (binary) classification models based on predictions. See \url{https://radiant-rstats.github.io/docs/model/evalbin.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param pred Predictions or predictors
#' @param rvar Response variable
#' @param lev The level in the response variable defined as success
#' @param qnt Number of bins to create
#' @param cost Cost for each connection (e.g., email or mailing)
#' @param margin Margin on each customer purchase
#' @param train Use data from training ("Training"), validation ("Validation"), both ("Both"), or all data ("All") to evaluate model evalbin
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of results
#'
#' @seealso \code{\link{summary.evalbin}} to summarize results
#' @seealso \code{\link{plot.evalbin}} to plot results
#'
#' @examples
#' data.frame(buy = dvd$buy, pred1 = runif(20000), pred2 = ifelse(dvd$buy == "yes", 1, 0)) %>%
#'   evalbin(c("pred1", "pred2"), "buy") %>%
#'   str()
#'
#' @export
evalbin <- function(
  dataset, pred, rvar, lev = "",
  qnt = 10, cost = 1, margin = 2,
  train = "All", data_filter = ""
) {

  ## in case no inputs were provided
  if (is.na(cost)) cost <- 0
  if (is.na(margin)) margin <- 0

  if (!train %in% c("", "All") && is_empty(data_filter)) {
    return("** Filter required. To set a filter go to Data > View and click\n   the filter checkbox **" %>% add_class("evalbin"))
  }

  if (is_empty(qnt)) qnt <- 10

  df_name <- if (!is_string(dataset)) deparse(substitute(dataset)) else dataset
  dat_list <- list()
  vars <- c(pred, rvar)
  if (train == "Both") {
    dat_list[["Training"]] <- get_data(dataset, vars, filt = data_filter)
    dat_list[["Validation"]] <- get_data(dataset, vars, filt = paste0("!(", data_filter, ")"))
  } else if (train == "Training") {
    dat_list[["Training"]] <- get_data(dataset, vars, filt = data_filter)
  } else if (train == "Validation") {
    dat_list[["Validation"]] <- get_data(dataset, vars, filt = paste0("!(", data_filter, ")"))
  } else {
    dat_list[["All"]] <- get_data(dataset, vars, filt = "")
  }

  qnt_name <- "bins"
  auc_list <- list()
  prof_list <- c()
  pdat <- list()
  pext <- c(All = "", Training = " (train)", Validation = " (val)")

  for (i in names(dat_list)) {
    lg_list <- list()
    pl <- c()
    dataset <- dat_list[[i]]

    if (nrow(dataset) == 0) {
      return(
        paste0("Data for ", i, " has zero rows. Please correct the filter used and try again") %>%
          add_class("evalbin")
      )
    }

    rv <- dataset[[rvar]]
    if (is.factor(rv)) {
      levs <- levels(rv)
    } else {
      levs <- rv %>% as.character() %>% as.factor() %>% levels()
    }

    if (lev == "") {
      lev <- levs[1]
    } else {
      if (!lev %in% levs) return(add_class("Level provided not found", "evalbin"))
    }

    ## transformation to TRUE/FALSE depending on the selected level (lev)
    dataset[[rvar]] <- dataset[[rvar]] == lev

    ## tip for summarise_ from http://stackoverflow.com/a/27592077/1974918
    ## put summaries in list so you can print and plot
    tot_resp <- sum(dataset[[rvar]])
    tot_obs <- nrow(dataset)
    tot_rate <- tot_resp / tot_obs

    for (j in seq_along(pred)) {
      pname <- paste0(pred[j], pext[i])
      auc_list[[pname]] <- auc(dataset[[pred[j]]], dataset[[rvar]], TRUE)
      lg_list[[pname]] <-
        dataset %>%
        select_at(.vars = c(pred[j], rvar)) %>%
        # mutate_(.dots = setNames(paste0(method,"(",pred[j],",", qnt,", rev = TRUE)"), pred[j])) %>%
        mutate(!! pred[j] := radiant.data::xtile(.data[[pred[j]]], n = qnt, rev = TRUE)) %>%
        setNames(c(qnt_name, rvar)) %>%
        group_by_at(.vars = qnt_name) %>%
        summarise(
          nr_obs = n(),
          nr_resp = sum(.data[[rvar]])
        ) %>%
        mutate(
          resp_rate = nr_resp / nr_obs,
          gains = nr_resp / tot_resp
        ) %>%
        {if (first(.$resp_rate) < last(.$resp_rate)) {
           mutate_all(., funs(rev))
         } else {
           .
         }
        } %>%
        mutate(
          profit = margin * cumsum(nr_resp) - cost * cumsum(nr_obs),
          ROME = profit / (cost * cumsum(nr_obs)),
          cum_prop = cumsum(nr_obs / tot_obs),
          cum_resp = cumsum(nr_resp),
          cum_resp_rate = cum_resp / cumsum(nr_obs),
          cum_lift = cum_resp_rate / tot_rate,
          cum_gains = cum_resp / tot_resp
        ) %>%
        mutate(pred = pname) %>%
        mutate(ROME = ifelse(is.na(ROME), 0, ROME)) %>%
        select(pred, everything())

      pl <- c(pl, max(lg_list[[pname]]$profit))
    }
    prof_list <- c(prof_list, pl / abs(max(pl)))
    pdat[[i]] <- bind_rows(lg_list) %>% mutate(profit = profit)
  }
  dataset <- bind_rows(pdat) %>% mutate(profit = ifelse(is.na(profit), 0, profit))
  dataset$pred <- factor(dataset$pred, levels = unique(dataset$pred))

  names(prof_list) <- names(auc_list)

  list(
    dataset = dataset, df_name = df_name, data_filter = data_filter,
    train = train, pred = pred, rvar = rvar, lev = lev,
    qnt = qnt, cost = cost, margin = margin
  ) %>% add_class("evalbin")
}

#' Summary method for the evalbin function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/evalbin.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{evalbin}}
#' @param prn Print full table of measures per model and bin
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{evalbin}} to summarize results
#' @seealso \code{\link{plot.evalbin}} to plot results
#'
#' @examples
#' data.frame(buy = dvd$buy, pred1 = runif(20000), pred2 = ifelse(dvd$buy == "yes", 1, 0)) %>%
#'   evalbin(c("pred1", "pred2"), "buy") %>%
#'   summary()
#'
#' @export
summary.evalbin <- function(object, prn = TRUE, dec = 3, ...) {
  if (is.character(object)) return(object)

  cat("Evaluate predictions for binary response models\n")
  cat("Data        :", object$df_name, "\n")
  if (object$data_filter %>% gsub("\\s", "", .) != "") {
    cat("Filter      :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Results for :", object$train, "\n")
  cat("Predictors  :", paste0(object$pred, collapse = ", "), "\n")
  cat("Response    :", object$rvar, "\n")
  cat("Level       :", object$lev, "in", object$rvar, "\n")
  cat("Bins        :", object$qnt, "\n")
  cat("Cost:Margin :", object$cost, ":", object$margin, "\n\n")

  if (prn) {
    as.data.frame(object$dataset, stringsAsFactors = FALSE) %>%
      format_df(dec = dec, mark = ",") %>%
      print(row.names = FALSE)
  }
}

#' Plot method for the evalbin function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/evalbin.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{evalbin}}
#' @param plots Plots to return
#' @param size Font size used
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{evalbin}} to generate results
#' @seealso \code{\link{summary.evalbin}} to summarize results
#'
#' @examples
#' data.frame(buy = dvd$buy, pred1 = runif(20000), pred2 = ifelse(dvd$buy == "yes", 1, 0)) %>%
#'   evalbin(c("pred1", "pred2"), "buy") %>%
#'   plot()
#'
#' @export
plot.evalbin <- function(
  x, plots = c("lift", "gains"),
  size = 13, shiny = FALSE,
  custom = FALSE, ...
) {

  if (is.character(x) || is.null(x$dataset) || any(is.na(x$dataset$cum_lift)) ||
    is.null(plots)) {
    return(invisible())
  }

  plot_list <- list()
  if ("lift" %in% plots) {
    plot_list[["lift"]] <-
      visualize(x$dataset, xvar = "cum_prop", yvar = "cum_lift", type = "line", color = "pred", custom = TRUE) +
      geom_point() +
      geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1), size = .1, color = "black") +
      labs(y = "Cumulative lift", x = "Proportion of customers")
  }

  if ("gains" %in% plots) {
    dataset <- x$dataset %>%
      select(pred, cum_prop, cum_gains) %>%
      group_by(pred) %>%
      mutate(obs = 1:n())

    init <- filter(dataset, obs == 1)
    init[, c("cum_prop", "cum_gains", "obs")] <- 0
    dataset <- bind_rows(init, dataset) %>% arrange(pred, obs)

    plot_list[["gains"]] <-
      visualize(dataset, xvar = "cum_prop", yvar = "cum_gains", type = "line", color = "pred", custom = TRUE) +
      geom_point() +
      geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), size = .1, color = "black") +
      labs(y = "Cumulative gains", x = "Proportion of customers")
  }

  if ("profit" %in% plots) {
    dataset <- select(x$dataset, pred, cum_prop, profit) %>%
      group_by(pred) %>%
      mutate(obs = 1:n())

    init <- filter(dataset, obs == 1)
    init[, c("profit", "cum_prop", "obs")] <- 0
    dataset <- bind_rows(init, dataset) %>% arrange(pred, obs)

    plot_list[["profit"]] <- visualize(
      dataset,
      xvar = "cum_prop",
      yvar = "profit",
      type = "line",
      color = "pred",
      custom = TRUE
    ) +
      geom_point() +
      geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0), size = .1, color = "black") +
      labs(y = "Profit", x = "Proportion of customers")
  }

  if ("rome" %in% plots) {
    plot_list[["rome"]] <- visualize(
      x$dataset,
      xvar = "cum_prop",
      yvar = "ROME",
      type = "line",
      color = "pred",
      custom = TRUE
    ) +
      geom_point() +
      geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0), size = .1, color = "black") +
      labs(y = "Return on Marketing Expenditures (ROME)", x = "Proportion of customers")
  }

  for (i in names(plot_list)) {
    plot_list[[i]] <- plot_list[[i]] + theme_set(theme_gray(base_size = size))
    if (length(x$pred) < 2 && x$train != "Both") {
      plot_list[[i]] <- plot_list[[i]] + theme(legend.position = "none")
    } else {
      plot_list[[i]] <- plot_list[[i]] + labs(color = "Predictor")
    }
  }

  if (custom) {
    if (length(plot_list) == 1) return(plot_list[[1]]) else return(plot_list)
  }

  sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = 1)) %>%
    {if (shiny) . else print(.)}
}


#' Confusion matrix
#'
#' @details Confusion matrix and additional metrics to evaluate binary classification models. See \url{https://radiant-rstats.github.io/docs/model/evalbin.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param pred Predictions or predictors
#' @param rvar Response variable
#' @param lev The level in the response variable defined as success
#' @param cost Cost for each connection (e.g., email or mailing)
#' @param margin Margin on each customer purchase
#' @param train Use data from training ("Training"), validation ("Validation"), both ("Both"), or all data ("All") to evaluate model evalbin
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param ... further arguments passed to or from other methods
#'
#' @return A list of results
#'
#' @seealso \code{\link{summary.confusion}} to summarize results
#' @seealso \code{\link{plot.confusion}} to plot results
#'
#' @examples
#' data.frame(buy = dvd$buy, pred1 = runif(20000), pred2 = ifelse(dvd$buy == "yes", 1, 0)) %>%
#'   confusion(c("pred1", "pred2"), "buy") %>%
#'   str()
#'
#' @importFrom psych cohen.kappa
#'
#' @export
confusion <- function(
  dataset, pred, rvar, lev = "", cost = 1, margin = 2,
  train = "All", data_filter = "", ...
) {

  if (!train %in% c("", "All") && is_empty(data_filter)) {
    return("** Filter required. To set a filter go to Data > View and click the filter checkbox **" %>% add_class("confusion"))
  }

  ## in case no inputs were provided
  if (is_not(margin) || is_not(cost)) {
    break_even <- 0.5
  } else if (margin == 0) {
    break_even <- cost / 1
  } else {
    break_even <- cost / margin
  }

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dat_list <- list()
  vars <- c(pred, rvar)
  if (train == "Both") {
    dat_list[["Training"]] <- get_data(dataset, vars, filt = data_filter)
    dat_list[["Validation"]] <- get_data(dataset, vars, filt = paste0("!(", data_filter, ")"))
  } else if (train == "Training") {
    dat_list[["Training"]] <- get_data(dataset, vars, filt = data_filter)
  } else if (train == "Validation") {
    dat_list[["Validation"]] <- get_data(dataset, vars, filt = paste0("!(", data_filter, ")"))
  } else {
    dat_list[["All"]] <- get_data(dataset, vars, filt = "")
  }

  pdat <- list()
  for (i in names(dat_list)) {
    dataset <- dat_list[[i]]
    rv <- dataset[[rvar]]

    if (lev == "") {
      if (is.factor(rv)) {
        lev <- levels(rv)[1]
      } else {
        lev <- as.character(rv) %>% as.factor() %>% levels() %>% .[1]
      }
    } else {
      if (!lev %in% dataset[[rvar]]) return(add_class("Please update the selected level in the response variable", "confusion"))
    }

    ## transformation to TRUE/FALSE depending on the selected level (lev)
    dataset[[rvar]] <- dataset[[rvar]] == lev

    auc_vec <- rep(NA, length(pred)) %>% set_names(pred)
    for (p in pred) auc_vec[p] <- auc(dataset[[p]], dataset[[rvar]], TRUE)

    p_vec <- colMeans(dataset[, pred, drop = FALSE]) / mean(dataset[[rvar]])

    dataset[, pred] <- select_at(dataset, .vars = pred) > break_even

    if (length(pred) > 1) {
      dataset <- mutate_at(dataset, .vars = c(rvar, pred), .funs = funs(factor(., levels = c("FALSE", "TRUE"))))
    } else {
      dataset[, pred] %<>% apply(2, function(x) factor(x, levels = c("FALSE", "TRUE")))
    }

    make_tab <- function(x) {
      ret <- rep(0L, 4) %>% set_names(c("TN", "FN", "FP", "TP"))
      tab <- table(dataset[[rvar]], x) %>% as.data.frame(stringsAsFactors = FALSE)
      ## ensure a value is availble for all four options
      for (i in 1:nrow(tab)) {
        if (tab[i, 1] == "TRUE") {
          if (tab[i, 2] == "TRUE") {
            ret["TP"] <- tab[i, 3]
          } else {
            ret["FN"] <- tab[i, 3]
          }
        } else {
          if (tab[i, 2] == "TRUE") {
            ret["FP"] <- tab[i, 3]
          } else {
            ret["TN"] <- tab[i, 3]
          }
        }
      }
      return(ret)
    }
    ret <- lapply(select_at(dataset, .vars = pred), make_tab) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      t() %>%
      as.data.frame(stringsAsFactors = FALSE)
    ret <- bind_cols(
      data.frame(
        Type = rep(i, length(pred)),
        Predictor = pred,
        stringsAsFactors = FALSE
      ),
      ret,
      data.frame(
        AUC = auc_vec,
        p.ratio = p_vec,
        stringsAsFactors = FALSE
      )
    )

    pdat[[i]] <- ret
  }

  dataset <- bind_rows(pdat) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    mutate(
      total = TN + FN + FP + TP,
      TPR = TP / (TP + FN),
      TNR = TN / (TN + FP),
      precision = TP / (TP + FP),
      Fscore = 2 * (precision * TPR) / (precision + TPR),
      accuracy = (TP + TN) / total,
      profit = margin * TP - cost * (TP + FP),
      ROME = profit / (cost * (TP + FP)),
      contact = (TP + FP) / total,
      kappa = 0
    )

  dataset <- group_by_at(dataset, .vars = "Type") %>%
    mutate(index = profit / max(profit)) %>%
    ungroup()
  dataset <- mutate(dataset, profit = as.integer(round(profit, 0)))

  for (i in 1:nrow(dataset)) {
    tmp <- slice(dataset, i)
    dataset$kappa[i] <- psych::cohen.kappa(matrix(with(tmp, c(TN, FP, FN, TP)), ncol = 2))[["kappa"]]
  }

  dataset <- select_at(
    dataset,
    .vars = c(
      "Type", "Predictor", "TP", "FP", "TN", "FN", "total",
      "TPR", "TNR", "precision", "Fscore", "accuracy",
      "kappa", "profit", "index", "ROME", "contact", "AUC"
    )
  )

  list(
    dataset = dataset, df_name = df_name, data_filter = data_filter, train = train,
    pred = pred, rvar = rvar, lev = lev, cost = cost, margin = margin
  ) %>% add_class("confusion")
}

#' Summary method for the confusion matrix
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/evalbin.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{confusion}}
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{confusion}} to generate results
#' @seealso \code{\link{plot.confusion}} to visualize result
#'
#' @examples
#' data.frame(buy = dvd$buy, pred1 = runif(20000), pred2 = ifelse(dvd$buy == "yes", 1, 0)) %>%
#'   confusion(c("pred1", "pred2"), "buy") %>%
#'   summary()
#'
#' @export
summary.confusion <- function(object, dec = 3, ...) {
  if (is.character(object)) return(object)

  cat("Confusion matrix\n")
  cat("Data       :", object$df_name, "\n")
  if (object$data_filter %>% gsub("\\s", "", .) != "") {
    cat("Filter     :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Results for:", object$train, "\n")
  cat("Predictors :", paste0(object$pred, collapse = ", "), "\n")
  cat("Response   :", object$rvar, "\n")
  cat("Level      :", object$lev, "in", object$rvar, "\n")
  cat("Cost:Margin:", object$cost, ":", object$margin, "\n")
  cat("\n")

  as.data.frame(object$dataset[, 1:11], stringsAsFactors = FALSE) %>%
    format_df(dec = dec, mark = ",") %>%
    print(row.names = FALSE)
  cat("\n")
  as.data.frame(object$dataset[, c(1, 2, 12:18)], stringsAsFactors = FALSE) %>%
    format_df(dec = dec, mark = ",") %>%
    print(row.names = FALSE)
}

#' Plot method for the confusion matrix
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/evalbin.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{confusion}}
#' @param vars Measures to plot, i.e., one or more of "TP", "FP", "TN", "FN", "total", "TPR", "TNR", "precision", "accuracy", "kappa", "profit", "index", "ROME", "contact", "AUC"
#' @param scale_y Free scale in faceted plot of the confusion matrix (TRUE or FALSE)
#' @param size Font size used
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{confusion}} to generate results
#' @seealso \code{\link{summary.confusion}} to summarize results
#'
#' @examples
#' data.frame(buy = dvd$buy, pred1 = runif(20000), pred2 = ifelse(dvd$buy == "yes", 1, 0)) %>%
#'   confusion(c("pred1", "pred2"), "buy") %>%
#'   plot()
#'
#' @export
plot.confusion <- function(
  x, vars = c("kappa", "index", "ROME", "AUC"),
  scale_y = TRUE, size = 13, ...
) {

  if (is.character(x) || is.null(x)) return(invisible())
  dataset <- x$dataset %>%
    mutate_at(.vars = c("TN", "FN", "FP", "TP"), .funs = funs(if (is.numeric(.)) . / total else .)) %>%
    gather("Metric", "Value", !! vars, factor_key = TRUE) %>%
    mutate(Predictor = factor(Predictor, levels = unique(Predictor)))

  ## what data was used in evaluation? All, Training, Validation, or Both
  type <- unique(dataset$Type)

  if (scale_y) {
    p <- visualize(
      dataset, xvar = "Predictor", yvar = "Value", type = "bar",
      facet_row = "Metric", fill = "Type", axes = "scale_y", custom = TRUE
    )
  } else {
    p <- visualize(
      dataset, xvar = "Predictor", yvar = "Value", type = "bar",
      facet_row = "Metric", fill = "Type", custom = TRUE
    )
  }

  p <- p + labs(
    title = paste0("Classification performance plots (", paste0(type, collapse = ", "), ")"),
    y = "",
    x = "Predictor",
    fill = ""
  ) + theme_set(theme_gray(base_size = size))

  if (length(type) < 2) {
    p <- p + theme(legend.position = "none")
  }

  p
}

#' Area Under the Curve (AUC)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/evalbin.html} for an example in Radiant
#'
#' @param pred Prediction or predictor
#' @param rvar Response variable
#' @param lev The level in the response variable defined as success
#'
#' @return AUC statistic
#'
#' @seealso \code{\link{evalbin}} to calculate results
#' @seealso \code{\link{summary.evalbin}} to summarize results
#' @seealso \code{\link{plot.evalbin}} to plot results
#'
#' @examples
#' auc(runif(20000), dvd$buy, "yes")
#' auc(ifelse(dvd$buy == "yes", 1, 0), dvd$buy, "yes")
#'
#' @export
auc <- function(pred, rvar, lev) {
  ## based on examples in colAUC at ...
  ## https://cran.r-project.org/web/packages/caTools/caTools.pdf
  if (missing(lev) && is.factor(rvar)) lev <- levels(rvar)[1]
  stopifnot(length(lev) == 1, lev %in% rvar)
  x1 <- pred[rvar == lev]
  x2 <- pred[rvar != lev]
  ## need as.numeric to avoid integer-overflows
  denom <- as.numeric(length(x1)) * length(x2)
  wt <- wilcox.test(x1, x2, exact = FALSE)$statistic / denom
  ifelse(wt < .5, 1 - wt, wt)[["W"]]
}

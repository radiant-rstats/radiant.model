#' Collaborative Filtering
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/crs.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param id String with name of the variable containing user ids
#' @param prod String with name of the variable with product ids
#' @param pred Products to predict for
#' @param rate String with name of the variable with product ratings
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "training == 1")
#' @param envir Environment to extract data from
#'
#' @return A data.frame with the original data and a new column with predicted ratings
#'
#' @seealso \code{\link{summary.crs}} to summarize results
#' @seealso \code{\link{plot.crs}} to plot results if the actual ratings are available
#'
#' @examples
#' crs(ratings, id = "Users", prod = "Movies", pred = c("M6", "M7", "M8", "M9", "M10"),
#'     rate = "Ratings", data_filter = "training == 1") %>% str()
#'
#' @importFrom dplyr distinct_at
#'
#' @export
crs <- function(
  dataset, id, prod, pred, rate, 
  data_filter = "", envir = parent.frame()
) {

  vars <- c(id, prod, rate)
  df_name <- if (!is_string(dataset)) deparse(substitute(dataset)) else dataset
  uid <- get_data(dataset, id, filt = data_filter, na.rm = FALSE, envir = envir) %>% unique()
  dataset <- get_data(dataset, vars, na.rm = FALSE, envir = envir)

  ## creating a matrix layout
  ## will not be efficient for very large and sparse datasets
  ## improvement possible with dplyr or sparse matrix?

  ## make sure spread doesn't complain
  cn <- colnames(dataset)
  nr <- dplyr::distinct_at(dataset, .vars = base::setdiff(cn, rate), .keep_all = TRUE) %>%
    nrow()
  if (nr < nrow(dataset)) {
    return("Rows are not unique. Data not appropriate for collaborative filtering" %>% add_class("crs"))
  }

  dataset <- spread(dataset, !! prod, !! rate) %>%
    as.data.frame(stringsAsFactors = FALSE)

  idv <- select_at(dataset, .vars = id)
  uid <- seq_len(nrow(dataset))[idv[[1]] %in% uid[[1]]]
  dataset <- select_at(dataset, .vars = base::setdiff(colnames(dataset), id))

  ## can use : for long sets of products to predict for
  if (any(grepl(":", pred))) {
    pred <- select(
      dataset[1, , drop = FALSE],
      !!! rlang::parse_exprs(paste0(pred, collapse = ";"))
    ) %>% colnames()
  }

  ## stop if insufficient overlap in ratings
  if (length(pred) >= (ncol(dataset) - 1)) {
    return("Cannot predict for all products. Ratings must overlap on at least two products." %>% add_class("crs"))
  }

  if (length(vars) < (ncol(dataset) - 1)) {
    vars <- evar <- colnames(dataset)[-1]
  }

  ## indices
  cn <- colnames(dataset)
  nind <- which(cn %in% pred)
  ind <- (1:length(cn))[-nind]

  ## average scores and rankings
  avg <- dataset[uid, , drop = FALSE] %>%
    select(nind) %>%
    summarise_all(mean, na.rm = TRUE)
  ravg <- avg
  ravg[1, ] <- min_rank(desc(avg))
  ravg <- mutate_all(ravg, as.integer)

  ## actual scores and rankings (if available, else will be NA)
  act <- dataset[-uid, , drop = FALSE] %>% select(nind)
  ract <- act

  if (nrow(act) == 0) {
    return("Invalid filter used. Users to predict for should not be in the training set." %>%
      add_class("crs"))
  }

  rank <- apply(act, 1, function(x) as.integer(min_rank(desc(x)))) %>%
    {if (length(pred) == 1) . else t(.)}
  ract[, pred] <- rank
  ract <- bind_cols(idv[-uid, , drop = FALSE], ract)
  act <- bind_cols(idv[-uid, , drop = FALSE], act)

  ## CF calculations per row
  ms <- apply(select(dataset, -nind), 1, function(x) mean(x, na.rm = TRUE))
  sds <- apply(select(dataset, -nind), 1, function(x) sd(x, na.rm = TRUE))

  ## to forego standardization
  # ms <- ms * 0
  # sds <- sds/sds

  ## standardized ratings
  if (length(nind) < 2) {
    srate <- (dataset[uid, nind] - ms[uid]) / sds[uid]
  } else {
    srate <- sweep(dataset[uid, nind], 1, ms[uid], "-") %>% sweep(1, sds[uid], "/")
  }
  ## comfirmed to produce consistent results -- see cf-demo-missing-state.rda and cf-demo-missing.xlsx
  srate[is.na(srate)] <- 0
  srate <- mutate_all(as.data.frame(srate, stringsAsFactors = FALSE), ~ ifelse(is.infinite(.), 0, .))
  cors <- sshhr(cor(t(dataset[uid, ind]), t(dataset[-uid, ind]), use = "pairwise.complete.obs"))

  ## comfirmed to produce correct results -- see cf-demo-missing-state.rda and cf-demo-missing.xlsx
  cors[is.na(cors)] <- 0
  dnom <- apply(cors, 2, function(x) sum(abs(x), na.rm = TRUE))
  wts <- sweep(cors, 2, dnom, "/")
  cf <- (crossprod(wts, as.matrix(srate)) * sds[-uid] + ms[-uid]) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    bind_cols(idv[-uid, , drop = FALSE], .) %>%
    set_colnames(c(id, pred))

  ## Ranking based on CF
  rcf <- cf
  rank <- apply(select(cf, -1), 1, function(x) as.integer(min_rank(desc(x)))) %>% {
    if (length(pred) == 1) . else t(.)
  }
  rcf[, pred] <- rank

  recommendations <-
    inner_join(
      bind_cols(
        gather(act, "product", "rating", -1, factor_key = TRUE),
        select_at(gather(ract, "product", "ranking", -1, factor_key = TRUE), .vars = "ranking"),
        select_at(gather(cf, "product", "cf", -1, factor_key = TRUE),, .vars = "cf"),
        select_at(gather(rcf, "product", "cf_rank", -1, factor_key = TRUE), .vars = "cf_rank")
      ),
      data.frame(
        product = names(avg) %>% factor(., levels = .),
        average = t(avg),
        avg_rank = t(ravg)
      ),
      by = "product"
    ) %>%
    arrange_at(.vars = c(id, "product")) %>%
    select_at(.vars = c(id, "product", "rating", "average", "cf", "ranking", "avg_rank", "cf_rank"))

  rm(dataset, ms, sds, srate, cors, dnom, wts, cn, ind, nind, nr, uid, idv, envir)

  as.list(environment()) %>% add_class("crs")
}

#' Summary method for Collaborative Filter
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/crs.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{crs}}
#' @param n Number of lines of recommendations to print. Use -1 to print all lines
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{crs}} to generate the results
#' @seealso \code{\link{plot.crs}} to plot results if the actual ratings are available
#'
#' @examples
#' crs(ratings, id = "Users", prod = "Movies", pred = c("M6", "M7", "M8", "M9", "M10"),
#'     rate = "Ratings", data_filter = "training == 1") %>% summary()
#'
#' @export
summary.crs <- function(object, n = 36, dec = 2, ...) {
  if (is.character(object)) return(cat(object))

  cat("Collaborative filtering")
  cat("\nData       :", object$df_name)
  if (!is_empty(object$data_filter)) {
    cat("\nFilter     :", gsub("\\n", "", object$data_filter))
  }
  cat("\nUser id    :", object$id)
  cat("\nProduct id :", object$prod)
  cat("\nPredict for:", paste0(object$pred, collapse = ", "), "\n")
  if (nrow(object$recommendations) > n) {
    cat("Rows shown :", n, "out of", format_nr(nrow(object$recommendations), dec = 0), "\n")
  }

  if (nrow(object$act) > 0 && !any(is.na(object$act))) {
    cat("\nSummary:\n")

    ## From FZs do file output, calculate if actual ratings are available
    ## best based on highest average rating
    best <- which(object$ravg == 1)
    ar1 <- mean(object$ract[, best + 1] == 1)
    cat("\n- Average rating picks the best product", format_nr(ar1, dec = 1, perc = TRUE), "of the time")

    ## best based on cf
    best <- which(object$rcf == 1, arr.ind = TRUE)
    cf1 <- mean(object$ract[best] == 1)
    cat("\n- Collaborative filtering picks the best product", format_nr(cf1, dec = 1, perc = TRUE), "of the time")

    ## best based on highest average rating in top 3
    best <- which(object$ravg == 1)
    ar3 <- mean(object$ract[, best + 1] < 4)
    cat("\n- Pick based on average rating is in the top 3 products", format_nr(ar3, dec = 1, perc = TRUE), "of the time")

    ## best based on cf in top 3
    best <- which(object$rcf == 1, arr.ind = TRUE)
    cf3 <- mean(object$ract[best] < 4)
    cat("\n- Pick based on collaborative filtering is in the top 3 products", format_nr(cf3, dec = 1, perc = TRUE), "of the time")

    ## best 3 based on highest average rating contains best product
    best <- which(object$ravg < 4)
    inar3 <- mean(rowSums(object$ract[, best + 1, drop = FALSE] == 1) > 0)
    cat("\n- Top 3 based on average ratings contains the best product", format_nr(inar3, dec = 1, perc = TRUE), "of the time")

    ## best 3 based on cf contains best product
    best <- which(!object$rcf[, -1, drop = FALSE] < 4, arr.ind = TRUE)
    best[, "col"] <- best[, "col"] + 1
    object$ract[best] <- NA
    incf3 <- mean(rowSums(object$ract == 1, na.rm = TRUE) > 0)
    cat("\n- Top 3 based on collaborative filtering contains the best product", format_nr(incf3, dec = 1, perc = TRUE), "of the time\n")
  }

  cat("\nRecommendations:\n\n")
  if (n == -1) {
    cat("\n")
    format_df(object$recommendations, dec = dec) %>%
      {.[. == "NA"] <- ""; .} %>%
      print(row.names = FALSE)
  } else {
    head(object$recommendations, n) %>%
      format_df(dec = dec) %>%
      {.[. == "NA"] <- ""; .} %>%
      print(row.names = FALSE)
  }
}

#' Plot method for the crs function
#'
#' @details Plot that compares actual to predicted ratings. See \url{https://radiant-rstats.github.io/docs/model/crs.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{crs}}
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{crs}} to generate results
#' @seealso \code{\link{summary.crs}} to summarize results
#'
#' @export
plot.crs <- function(x, ...) {
  if (is.character(x)) return(x)
  if (any(is.na(x$act)) || all(is.na(x$cf))) {
    return("Plotting for Collaborative Filter requires the actual ratings associated\nwith the predictions")
  }

  ## use quantile to avoid plotting extreme predictions
  lim <- quantile(x$recommendations[, c("rating", "cf")], probs = c(.025, .975), na.rm = TRUE)


  p <- visualize(
    x$recommendations, xvar = "cf", yvar = "rating",
    type = "scatter", facet_col = "product", check = "line",
    custom = TRUE
  ) +
    geom_segment(aes(x = 1, y = 1, xend = 5, yend = 5), color = "blue", size = .05) +
    coord_cartesian(xlim = lim, ylim = lim) +
    labs(
      title = "Recommendations based on Collaborative Filtering",
      x = "Predicted ratings",
      y = "Actual ratings"
    ) +
    theme(legend.position = "none")

  sshhr(p)
}

#' Deprecated: Store method for the crs function
#'
#' @details Return recommendations See \url{https://radiant-rstats.github.io/docs/model/crs.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param object Return value from \code{\link{crs}}
#' @param name Name to assign to the dataset
#' @param ... further arguments passed to or from other methods
#'
#' @export
store.crs <- function(dataset, object, name, ...) {
  if (missing(name)) {
    object$recommendations
  } else {
     stop(
      paste0(
        "This function is deprecated. Use the code below instead:\n\n",
        name, " <- ", deparse(substitute(object)), "$recommendations\nregister(\"",
        name, ")"
      ),
      call. = FALSE
    )
  }
}

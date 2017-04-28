#' Collaborative Filtering
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/crs.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param id String with name of the variable containing user ids
#' @param prod String with name of the variable with product ids
#' @param pred Products to predict for
#' @param rate String with name of the variable with product ratings
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "training == 1")
#'
#' @return A data.frame with the original data and a new column with predicted ratings
#'
#' @importFrom dplyr distinct_
#'
#' @export
crs <- function(dataset, id, prod, pred, rate, data_filter = "") {

  vars <- c(id, prod, rate)
  dat <- getdata(dataset, vars, na.rm = FALSE)
  if (!is_string(dataset)) dataset <- deparse(substitute(dataset)) %>% set_attr("df", TRUE)

  ## creating a matrix layout
  ## will not be efficient for very large and sparse datasets
  ## improvement possible with dplyr or sparse matrix?

  ## make sure spread doesn't complain
  cn <- colnames(dat)
  nr <- distinct_(dat, .dots = setdiff(cn, rate), .keep_all = TRUE) %>% nrow
  if (nr < nrow(dat))
    return("Rows are not unique. Data not appropriate for collaborative filtering" %>% add_class("crs"))

  dat <- spread_(dat, prod, rate)

  idv <- select_(dat,id)
  uid <- getdata(dataset, id, filt = data_filter, na.rm = FALSE) %>% unique
  uid <- seq_len(nrow(dat))[idv[[1]] %in% uid[[1]]]

  dat <- select_(dat, paste0("-",id))

  ## stop if insufficient overlap in ratings
  if (length(pred) >= (ncol(dat) - 1))
    return("Cannot predict for all products. Ratings must overlap on at least two products." %>% add_class("crs"))

  ## indices
  cn <- colnames(dat)
  nind <- which(cn %in% pred)
  ind <- (1:length(cn))[-nind]

  ## average scores and rankings
  avg <- dat[uid,,drop = FALSE] %>% select(nind) %>% summarise_all(funs(mean_rm))
  ravg <- avg
  ravg[1,] <- min_rank(desc(avg))
  ravg <- mutate_all(ravg, funs(as.integer))

  ## actual scores and rankings (if available, else will be NA)
  act <- dat[-uid,, drop = FALSE] %>% select(nind)
  ract <- act

  if (nrow(act) == 0)
    return("Invalid filter used. Users to predict for should not be in the training set." %>% add_class("crs"))

  rank <- apply(act,1, function(x) as.integer(min_rank(desc(x)))) %>%
    {if(length(pred) == 1) . else t(.)}
  ract[,pred] <- rank
  ract <- bind_cols(idv[-uid,, drop = FALSE], ract) # %>% as_data_frame
  act <- bind_cols(idv[-uid,, drop = FALSE],act) # %>% as.data.frame

  ## CF calculations per row
  ms <- apply(select(dat,-nind), 1, function(x) mean(x, na.rm = TRUE))
  sds <- apply(select(dat,-nind), 1, function(x) sd(x, na.rm = TRUE))

  ## to forego standardization
  # ms <- ms * 0
  # sds <- sds/sds

  ## standardized ratings
  if (length(nind) < 2) {
    srate <- (dat[uid,nind] - ms[uid]) / sds[uid]
  } else {
    srate <- sweep(dat[uid,nind], 1, ms[uid], "-") %>% sweep(1, sds[uid] ,"/")
  }
  ## comfirmed to produce consistent results -- see cf-demo-missing-state.rda and cf-demo-missing.xlsx
  srate[is.na(srate)] <- 0
  srate <- mutate_all(as.data.frame(srate), funs(ifelse (is.infinite(.), 0, .)))

  cors <- sshhr(cor(t(dat[uid, ind]), t(dat[-uid, ind]), use = "pairwise.complete.obs"))

  ## comfirmed to produce correct results -- see cf-demo-missing-state.rda and cf-demo-missing.xlsx
  cors[is.na(cors)] <- 0
  dnom <- apply(cors, 2, function(x) sum(abs(x), na.rm = TRUE))
  wts <- sweep(cors, 2, dnom, "/")
  cf <-
    (crossprod(wts, as.matrix(srate)) * sds[-uid] + ms[-uid]) %>%
    as.data.frame %>%
    bind_cols(idv[-uid,, drop = FALSE],.) %>%
    set_colnames(c(id, pred)) # %>% as.data.frame

  ## Ranking based on CF
  rcf <- cf
  rank <- apply(select(cf,-1),1, function(x) as.integer(min_rank(desc(x)))) %>%
    {if(length(pred) == 1) . else t(.)}
  rcf[,pred] <- rank

  recommendations <-
    inner_join(
      bind_cols(gather(act, "product", "rating", -1),
                select_(gather(ract, "product", "ranking", -1), .dots = "ranking"),
                select_(gather(cf, "product", "cf", -1), .dots = "cf"),
                select_(gather(rcf, "product", "cf_rank", -1), .dots = "cf_rank")),
      data.frame(product = names(avg), average = t(avg), avg_rank = t(ravg)),
      by = "product") %>%
    arrange_(c(id, "product")) %>%
    select_(.dots = c(id, "product", "rating", "average", "cf", "ranking", "avg_rank", "cf_rank"))

  rm(dat, ms, sds, srate, cors, dnom, wts, cn, ind, nind)

  as.list(environment()) %>% add_class("crs")
}

#' Summary method for Collaborative Filter
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/crs.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{crs}}
#' @param n Number of lines of recommendations to print. Use -1 to print all lines
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{crs}} to generate the results
#' @seealso \code{\link{plot.crs}} to plot results
#'
#' @export
summary.crs <- function(object, n = 36, ...) {

  if (is.character(object)) return(cat(object))

  cat("Collaborative filtering")
  cat("\nData       :", object$dataset)
  if (object$data_filter %>% gsub("\\s","",.) != "")
    cat("\nFilter     :", gsub("\\n","", object$data_filter))
  cat("\nUser id    :", object$id)
  cat("\nProduct id :", object$prod)
  cat("\nPredict for:", paste0(object$pred, collapse=", "),"\n")
  if (nrow(object$recommendations) > n)
    cat("Rows shown :", n, "out of", formatnr(nrow(object$recommendations), dec = 0), "\n")

  if (nrow(object$act) > 0 && !any(is.na(object$act))) {

    cat("\nSummary:\n")

    ## From FZs do file output, calculate if actual ratings are available
    ## best based on highest average rating
    best <- which(object$ravg == 1)
    ar1 <- mean(object$ract[,best + 1] == 1)
    cat("\n- Average rating picks the best product", formatnr(ar1, dec = 1, perc = TRUE), "of the time")

    ## best based on cf
    best <- which(object$rcf == 1, arr.ind = TRUE)
    cf1 <- mean(object$ract[best] == 1)
    cat("\n- Collaborative filtering picks the best product", formatnr(cf1, dec = 1, perc = TRUE), "of the time")

    ## best based on highest average rating in top 3
    best <- which(object$ravg == 1)
    ar3 <- mean(object$ract[,best + 1] < 4)
    cat("\n- Pick based on average rating is in the top 3 products", formatnr(ar3, dec = 1, perc = TRUE), "of the time")

    ## best based on cf in top 3
    best <- which(object$rcf == 1, arr.ind = TRUE)
    cf3 <- mean(object$ract[best] < 4)
    cat("\n- Pick based on collaborative filtering is in the top 3 products", formatnr(cf3, dec = 1, perc = TRUE), "of the time")

    ## best 3 based on highest average rating contains best product
    best <- which(object$ravg < 4)
    inar3 <- mean(rowSums(object$ract[,best + 1, drop = FALSE] == 1) > 0)
    cat("\n- Top 3 based on average ratings contains the best product", formatnr(inar3, dec = 1, perc = TRUE), "of the time")

    ## best 3 based on cf contains best product
    best <- which(object$rcf < 4, arr.ind = TRUE)
    ract <- object$ract
    ract[!object$rcf < 4] <- NA
    incf3 <- mean(rowSums(ract == 1, na.rm = TRUE) > 0)
    cat("\n- Top 3 based on collaborative filtering contains the best product", formatnr(incf3, dec = 1, perc = TRUE), "of the time\n")
  }

  object$recommendations[is.na(object$recommendations)] <- ""
  cat("\nRecommendations:\n\n")
  if (n == -1) {
    cat("\n")
    print(formatdf(object$recommendations, dec = 2), row.names = FALSE)
  } else {
    head(object$recommendations, n) %>% formatdf(dec = 2) %>% print(row.names = FALSE)
  }
}

#' Plot method for the crs function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/crs.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{crs}}
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{crs}} to generate results
#' @seealso \code{\link{summary.crs}} to summarize results
#'
#' @export
plot.crs <- function(x, ...) {

  object <- x; rm(x)
  if (is.character(object)) return(object)

  if (any(is.na(object$act)))
    return("Plotting for Collaborative Filter requires the actual ratings associated\nwith the predictions")

  ## use quantile to avoid plotting extreme predictions
  lim <- quantile(object$recommendations[, c("rating", "cf")], probs = c(.025,.975), na.rm = TRUE)

  p <- visualize(object$recommendations, xvar = "cf", yvar = "rating",
                 type = "scatter", facet_col = "product", check = "line",
                 custom = TRUE) +
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

#' Store predicted values generated in the crs function
#'
#' @details Store data frame with predictions in Radiant r_data list if available. See \url{https://radiant-rstats.github.io/docs/model/crs.html} for an example in Radiant
#'
#' @param object Return value from crs
#' @param name Name of the dataset to store
#' @param envir Environment to assign 'new' dataset (optional). Used when an r_data list is not available
#' @param ... further arguments passed to or from other methods
#'
#' @importFrom pryr where
#'
#' @export
store.crs <- function(object, name = "predict_cf", envir = parent.frame(), ...) {

  object <- object$recommendations
  if (exists("r_environment")) {
    env <- r_environment
  } else if (exists("r_data")) {
    env <- pryr::where("r_data")
  } else {
    assign(name, object, envir = envir)
    message("Dataset ", name, " created in ", environmentName(envir), " environment")
    return(invisible())
  }

  # ## use data description from the original if available
  if (is_empty(env$r_data[[paste0(name, "_descr")]])) {
    attr(object, "description") <- paste0("## Collaborative Filtering\n\nThis dataset contains predicted ratings and ranking based on collaborative filter applied to the ", attr(object,"dataset"), "dataset.")
  } else {
    attr(object, "description") <- env$r_data[[paste0(name, "_descr")]]
  }

  env$r_data[[name]] <- object
  env$r_data[[paste0(name,"_descr")]] <- attr(object, "description")
  env$r_data[["datasetlist"]] <- c(name, env$r_data[["datasetlist"]]) %>% unique
}


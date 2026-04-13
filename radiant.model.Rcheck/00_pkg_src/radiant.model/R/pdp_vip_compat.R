# Internal replacements for the pdp and vip packages.
#
# Provides:
#   pdp_partial()      - brute-force partial dependence (replaces pdp::partial)
#   autoplot.partial() - ggplot2 S3 method for "partial" objects
#   vi_radiant()       - permutation importance (replaces vip::vi with method="permute")

# ── Metric helpers ─────────────────────────────────────────────────────────────

# R-squared via correlation (matches yardstick::rsq_vec)
.rsq_internal <- function(truth, estimate) {
  cor(truth, estimate, use = "pairwise.complete.obs")^2
}

# ROC AUC via Mann-Whitney formula (matches yardstick::roc_auc_vec)
.roc_auc_internal <- function(truth, estimate, event_level = "first") {
  lev <- levels(truth)
  pos_class <- if (event_level == "first") lev[1L] else lev[2L]
  truth_bin <- truth == pos_class
  n_pos <- sum(truth_bin, na.rm = TRUE)
  n_neg <- sum(!truth_bin, na.rm = TRUE)
  if (n_pos == 0L || n_neg == 0L) return(NA_real_)
  U <- sum(rank(estimate)[truth_bin]) - n_pos * (n_pos + 1L) / 2L
  U / (n_pos * n_neg)
}

# ── Prediction helper for supported model types ────────────────────────────────

.pdp_predict <- function(object, newdata, prob = FALSE) {
  if (inherits(object, "xgb.Booster")) {
    stats::predict(object, newdata = newdata)
  } else if (inherits(object, "ranger")) {
    preds <- stats::predict(object, data = newdata)$predictions
    if (is.matrix(preds)) preds[, 1L] else preds
  } else if (inherits(object, "glm")) {
    stats::predict(object, newdata = newdata,
                   type = if (prob) "response" else "link")
  } else if (inherits(object, "rpart")) {
    preds <- stats::predict(object, newdata = newdata)
    if (is.matrix(preds)) preds[, 1L] else preds
  } else if (inherits(object, "nnet")) {
    preds <- stats::predict(object, newdata = newdata, type = "raw")
    if (is.matrix(preds) && ncol(preds) == 1L) preds[, 1L] else as.numeric(preds)
  } else {
    # Default: use stats::predict with newdata
    preds <- stats::predict(object, newdata = newdata)
    if (is.matrix(preds) || is.data.frame(preds)) preds[, 1L] else preds
  }
}

# ── Grid generation ────────────────────────────────────────────────────────────

.make_pred_grid <- function(train, pred.var, quantiles = FALSE, probs = 1:9/10) {
  val_list <- lapply(pred.var, function(v) {
    col <- if (is.matrix(train)) train[, v, drop = TRUE] else train[[v]]
    if (is.factor(col)) {
      levels(col)
    } else if (is.character(col)) {
      sort(unique(col))
    } else if (quantiles) {
      unique(stats::quantile(col, probs = probs, na.rm = TRUE, names = FALSE))
    } else {
      n <- min(length(unique(col)), 51L)
      seq(min(col, na.rm = TRUE), max(col, na.rm = TRUE), length.out = n)
    }
  })
  grid <- expand.grid(val_list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  names(grid) <- pred.var
  grid
}

# ── pdp_partial: minimal pdp::partial replacement ─────────────────────────────

#' Compute partial dependence for supported model types
#'
#' Brute-force implementation of partial dependence that covers the model types
#' used within radiant.model (xgb.Booster, lm, glm, ranger, rpart, nnet).
#' This is an internal replacement for \code{pdp::partial()}.
#'
#' @param object A fitted model object.
#' @param pred.var Character vector of predictor variable names.
#' @param pred.grid Optional data frame with the grid of values to use.
#' @param train Training data (data frame or matrix).
#' @param prob Logical; if \code{TRUE} return probabilities for classification.
#' @param quantiles Logical; use sample quantiles for the grid instead of
#'   equally-spaced values.
#' @param probs Numeric vector of probabilities when \code{quantiles = TRUE}.
#' @param plot Logical; if \code{TRUE} return a \code{ggplot2} object.
#' @param plot.engine Ignored (kept for API compatibility).
#' @param rug Logical; add rug marks when \code{plot = TRUE}.
#' @param ... Ignored (kept for API compatibility).
#'
#' @return A data frame with class \code{c("partial","data.frame")} when
#'   \code{plot = FALSE}, or a \code{ggplot} object when \code{plot = TRUE}.
#'
#' @importFrom ggplot2 autoplot
#' @keywords internal
pdp_partial <- function(object, pred.var, pred.grid, train,
                        prob = FALSE, quantiles = FALSE, probs = 1:9/10,
                        plot = FALSE, plot.engine = "ggplot2",
                        rug = FALSE, ...) {
  # xgboost requires a numeric matrix
  if (inherits(object, "xgb.Booster") && is.data.frame(train)) {
    train <- data.matrix(train)
  }

  # Build grid if not supplied
  if (missing(pred.grid)) {
    pred.grid <- .make_pred_grid(train, pred.var, quantiles, probs)
  }

  # Convert grid to matrix row-access friendly form
  pred.grid <- as.data.frame(pred.grid, stringsAsFactors = FALSE)

  # Compute partial dependence via brute-force averaging
  yhat <- vapply(seq_len(nrow(pred.grid)), function(i) {
    temp <- train
    for (v in pred.var) {
      temp[, v] <- pred.grid[[v]][i]
    }
    mean(.pdp_predict(object, temp, prob), na.rm = TRUE)
  }, numeric(1L))

  pd.df <- cbind(pred.grid[, pred.var, drop = FALSE], yhat = yhat)
  class(pd.df) <- c("partial", "data.frame")

  if (isTRUE(plot)) {
    ggplot2::autoplot(pd.df, rug = rug, train = train)
  } else {
    pd.df
  }
}

# ── autoplot.partial: ggplot2 method for partial dependence objects ────────────

#' Plot a partial dependence object
#'
#' \code{ggplot2::autoplot} method for objects of class \code{"partial"} as
#' returned by \code{\link{pdp_partial}}.
#'
#' @param object A data frame with class \code{"partial"}.
#' @param rug Logical; add quantile rug marks on the x-axis.
#' @param train Training data used for rug marks (required when \code{rug = TRUE}).
#' @param ... Additional arguments passed to \code{geom_line} or \code{geom_point}.
#'
#' @return A \code{ggplot} object.
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_rug xlab ylab
#' @importFrom rlang .data
#' @export
autoplot.partial <- function(object, rug = FALSE, train = NULL, ...) {
  xvar <- names(object)[1L]
  col <- object[[xvar]]

  p <- if (is.numeric(col)) {
    ggplot2::ggplot(object, ggplot2::aes(.data[[xvar]], .data[["yhat"]])) +
      ggplot2::geom_line(...)
  } else {
    # Categorical: connect points with a line (group = 1 needed for discrete x-axis)
    ggplot2::ggplot(object, ggplot2::aes(.data[[xvar]], .data[["yhat"]], group = 1)) +
      ggplot2::geom_point(...) +
      ggplot2::geom_line()
  }

  if (isTRUE(rug) && !is.null(train) && is.numeric(col)) {
    x_col <- if (is.matrix(train)) train[, xvar, drop = TRUE] else train[[xvar]]
    rug_df <- data.frame(
      v = as.numeric(stats::quantile(x_col, probs = 0:10 / 10, na.rm = TRUE))
    )
    names(rug_df) <- xvar
    p <- p + ggplot2::geom_rug(
      data = rug_df,
      ggplot2::aes(.data[[xvar]]),
      sides = "b", inherit.aes = FALSE
    )
  }

  p + ggplot2::xlab(xvar) + ggplot2::ylab("yhat")
}

# ── vi_radiant: minimal vip::vi(method="permute") replacement ─────────────────

#' Compute permutation-based variable importance
#'
#' Internal replacement for \code{vip::vi(..., method = "permute")} that
#' supports the \code{"rsq"} and \code{"roc_auc"} metrics used in radiant.model.
#' No external package dependencies are required.
#'
#' @param object A fitted model object.
#' @param target Character string naming the response column in \code{train}.
#' @param method Must be \code{"permute"} (only supported method).
#' @param metric Character string; either \code{"rsq"} or \code{"roc_auc"}.
#' @param pred_wrapper A function with arguments \code{object} and \code{newdata}
#'   that returns a numeric prediction vector.
#' @param train Data frame containing both features and the target column.
#' @param event_level \code{"first"} (default) or \code{"second"}; which factor
#'   level of \code{target} is the positive class (used for \code{"roc_auc"}).
#' @param nsim Number of permutation replications per feature; results are
#'   averaged across replications. Default is 3.
#' @param ... Ignored.
#'
#' @return A data frame with columns \code{Variable} and \code{Importance}
#'   (and class \code{c("vi","data.frame")}).
#'
#' @keywords internal
vi_radiant <- function(object, target, method = "permute",
                       metric, pred_wrapper, train,
                       event_level = NULL, nsim = 3L, ...) {
  if (method != "permute") {
    stop("vi_radiant only supports method = 'permute'", call. = FALSE)
  }

  feature_names <- setdiff(colnames(train), target)
  train_x <- train[, feature_names, drop = FALSE]
  train_y <- train[[target]]

  # Build metric function
  if (metric == "rsq") {
    smaller_is_better <- FALSE
    metric_fun <- function(truth, estimate) .rsq_internal(truth, estimate)
  } else if (metric == "roc_auc") {
    smaller_is_better <- FALSE
    el <- if (is.null(event_level)) "first" else event_level
    metric_fun <- function(truth, estimate) .roc_auc_internal(truth, estimate, el)
  } else {
    stop("vi_radiant supports only 'rsq' and 'roc_auc' metrics", call. = FALSE)
  }

  baseline <- metric_fun(train_y, pred_wrapper(object, train_x))

  imp <- vapply(feature_names, function(fn) {
    mean(vapply(seq_len(nsim), function(s) {
      permx <- train_x
      permx[[fn]] <- permx[[fn]][sample(nrow(permx))]
      permuted <- metric_fun(train_y, pred_wrapper(object, permx))
      if (smaller_is_better) permuted - baseline else baseline - permuted
    }, numeric(1L)))
  }, numeric(1L))

  out <- data.frame(
    Variable   = feature_names,
    Importance = imp,
    stringsAsFactors = FALSE
  )
  # Sort descending by importance to match vip::vi() default (sort = TRUE, decreasing = TRUE)
  out <- out[order(out$Importance, decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  class(out) <- c("vi", "data.frame")
  out
}

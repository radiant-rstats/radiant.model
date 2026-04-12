# library(radiant.model)
# library(testthat)

context("Partial Dependence Plots (pdp_plot) and Variable Importance (varimp)")

# ── varimp ────────────────────────────────────────────────────────────────────

test_that("varimp - linear regression returns correct structure", {
  result <- regress(diamonds, "price", c("carat", "clarity"))
  vi <- varimp(result, "price")
  expect_true(is.data.frame(vi))
  expect_true(all(c("Variable", "Importance") %in% colnames(vi)))
  expect_gt(nrow(vi), 0)
  expect_true(all(vi$Importance > 0))
})

test_that("varimp - logistic classification returns correct structure", {
  result <- logistic(titanic, "survived", c("pclass", "sex"))
  vi <- varimp(result, "survived")
  expect_true(is.data.frame(vi))
  expect_true(all(c("Variable", "Importance") %in% colnames(vi)))
  expect_gt(nrow(vi), 0)
})

test_that("varimp - neural network (regression) returns correct structure", {
  result <- nn(diamonds, "price", c("carat", "clarity"), type = "regression", seed = 1234)
  vi <- varimp(result, "price")
  expect_true(is.data.frame(vi))
  expect_true(all(c("Variable", "Importance") %in% colnames(vi)))
  expect_gt(nrow(vi), 0)
  expect_true(all(vi$Importance > 0))
})

test_that("varimp - neural network (classification) returns correct structure", {
  result <- nn(titanic, "survived", c("pclass", "sex"), seed = 1234)
  vi <- varimp(result, "survived")
  expect_true(is.data.frame(vi))
  expect_true(all(c("Variable", "Importance") %in% colnames(vi)))
  expect_gt(nrow(vi), 0)
})

test_that("varimp - crtree classification returns correct structure", {
  result <- crtree(titanic, "survived", c("pclass", "sex"), lev = "Yes")
  vi <- varimp(result, "survived")
  expect_true(is.data.frame(vi))
  expect_true(all(c("Variable", "Importance") %in% colnames(vi)))
  expect_gt(nrow(vi), 0)
})

# ── varimp_plot ───────────────────────────────────────────────────────────────

test_that("varimp_plot - regression returns ggplot", {
  result <- regress(diamonds, "price", c("carat", "clarity"))
  p <- varimp_plot(result, "price")
  expect_true(inherits(p, "gg"))
})

test_that("varimp_plot - nn regression returns ggplot", {
  result <- nn(diamonds, "price", c("carat", "clarity"), type = "regression", seed = 1234)
  p <- varimp_plot(result, "price")
  expect_true(inherits(p, "gg"))
})

# ── pdp_plot ──────────────────────────────────────────────────────────────────

test_that("pdp_plot - regress continuous predictor returns ggplot", {
  result <- regress(diamonds, "price", c("carat", "clarity"))
  p <- plot(result, plots = "pdp", incl = "carat", custom = TRUE)
  expect_true(inherits(p, "gg"))
})

test_that("pdp_plot - regress categorical predictor returns ggplot", {
  result <- regress(diamonds, "price", c("carat", "clarity"))
  p <- plot(result, plots = "pdp", incl = "clarity", custom = TRUE)
  expect_true(inherits(p, "gg"))
})

test_that("pdp_plot - logistic continuous predictor returns ggplot", {
  result <- logistic(titanic, "survived", c("pclass", "sex"))
  p <- plot(result, plots = "pdp", incl = "pclass", custom = TRUE)
  expect_true(inherits(p, "gg"))
})

test_that("pdp_plot - nn regression returns ggplot", {
  result <- nn(diamonds, "price", c("carat", "clarity"), type = "regression", seed = 1234)
  p <- plot(result, plots = "pdp", incl = "carat", custom = TRUE)
  expect_true(inherits(p, "gg"))
})

test_that("pdp_plot - crtree classification returns ggplot", {
  result <- crtree(titanic, "survived", c("pclass", "sex"), lev = "Yes")
  p <- plot(result, plots = "pdp", incl = "pclass", custom = TRUE)
  expect_true(inherits(p, "gg"))
})

test_that("pdp_plot - rforest classification returns ggplot", {
  result <- rforest(titanic, "survived", c("pclass", "sex"), lev = "Yes")
  p <- plot(result, plots = "pdp", incl = "pclass", custom = TRUE)
  expect_true(inherits(p, "gg"))
})

# ── plot.gbt ──────────────────────────────────────────────────────────────────

test_that("plot.gbt - vip returns ggplot", {
  result <- gbt(titanic, "survived", c("pclass", "sex"), lev = "Yes",
                early_stopping_rounds = 0, seed = 1234)
  p <- plot(result, plots = "vip", custom = TRUE)
  expect_true(inherits(p, "gg"))
})

# library(radiant.model)
# library(testthat)

######### tests ########
context("Neural network prediction rescaling")

## nn standardizes the data before estimation (check = "standardize" is the
## default). For a regression the response is scaled as well, so predictions
## from the underlying nnet model are on the standardized scale and have to be
## converted back to the scale of the original data. These tests pin down that
## round trip.

nn_reg <- function(...) {
  nn(
    diamonds[, c("price", "carat", "clarity")], "price", c("carat", "clarity"),
    type = "regression", size = 1, decay = .5, seed = 1234, ...
  )
}

test_that("nn stores the information needed to undo standardization", {
  result <- nn_reg()
  dat <- result$model$model

  expect_equal(attr(dat, "radiant_sf"), 2)
  expect_equal(attr(dat, "radiant_ms")$price, mean(diamonds$price))
  expect_equal(attr(dat, "radiant_sds")$price, sd(diamonds$price))

  ## the data the model was fit on really is standardized
  expect_equal(
    dat$carat,
    (diamonds$carat - mean(diamonds$carat)) / (2 * sd(diamonds$carat))
  )
  ## factors are left alone
  expect_true(is.factor(dat$clarity))
})

test_that("nn regression predictions are returned on the original scale", {
  result <- nn_reg()
  sf <- attr(result$model$model, "radiant_sf")
  ms <- attr(result$model$model, "radiant_ms")$price
  sds <- attr(result$model$model, "radiant_sds")$price

  pred <- predict(result, pred_data = diamonds)$Prediction

  ## predictions are the (standardized) fitted values converted back. Note that
  ## sf is read from the model rather than hard-coded, so a change in the
  ## scaling factor cannot silently break the round trip
  expect_equal(
    as.numeric(pred),
    as.numeric(result$model$fitted.values) * sf * sds + ms
  )

  ## and they are in the neighborhood of the response, not of the z-scores
  expect_true(mean(pred) > 1000)
  expect_true(abs(mean(pred) - mean(diamonds$price)) < sd(diamonds$price))
})

test_that("nn regression matches an independent implementation end-to-end", {
  ## standardize, fit, predict, and convert back without using any radiant code
  dat <- diamonds[, c("price", "carat", "clarity")]
  manual <- dat
  for (v in c("price", "carat")) {
    manual[[v]] <- (manual[[v]] - mean(dat[[v]])) / (2 * sd(dat[[v]]))
  }
  set.seed(1234)
  mod <- nnet::nnet(
    price ~ ., data = manual, rang = .1, size = 1, decay = .5, weights = NULL,
    maxit = 10000, linout = TRUE, entropy = FALSE, skip = FALSE, trace = FALSE
  )
  expected <- as.numeric(predict(mod, manual)) * 2 * sd(dat$price) + mean(dat$price)

  result <- nn_reg()
  expect_equal(as.numeric(predict(result, pred_data = dat)$Prediction), expected)
})

test_that("nn predictions from pred_cmd are on the original scale", {
  result <- nn_reg()
  sf <- attr(result$model$model, "radiant_sf")
  ms <- attr(result$model$model, "radiant_ms")
  sds <- attr(result$model$model, "radiant_sds")

  pred <- predict(result, pred_cmd = "carat = c(0.5, 1, 1.5)")

  ## the predictor values reported back are on the original scale ...
  expect_setequal(unique(pred$carat), c(0.5, 1, 1.5))

  ## ... so scaling them by hand and running them through the underlying nnet
  ## model must reproduce the reported predictions
  scaled <- pred[, c("carat", "clarity")]
  scaled$carat <- (scaled$carat - ms$carat) / (sf * sds$carat)
  expected <- as.numeric(predict(result$model, scaled)) * sf * sds$price + ms$price

  expect_equal(as.numeric(pred$Prediction), expected)

  ## larger diamonds are predicted to be more expensive
  avg <- tapply(pred$Prediction, pred$carat, mean)
  expect_true(all(diff(avg) > 0))
})

test_that("nn predictions are not rescaled when standardization is off", {
  result <- nn_reg(check = "")

  expect_null(attr(result$model$model, "radiant_sf"))
  expect_null(attr(result$model$model, "radiant_sds"))

  ## without standardization the fitted values are already on the price scale
  pred <- predict(result, pred_data = diamonds)$Prediction
  expect_equal(as.numeric(pred), as.numeric(result$model$fitted.values))
})

test_that("nn classification predictions are probabilities and not rescaled", {
  dat <- titanic[, c("survived", "pclass", "sex", "age")]
  result <- nn(dat, "survived", c("pclass", "sex", "age"), lev = "Yes", size = 1, seed = 1234)

  ## the response is logical after the level is applied, so it is not scaled
  expect_false("survived" %in% names(attr(result$model$model, "radiant_sds")))
  ## numeric explanatory variables still are
  expect_equal(attr(result$model$model, "radiant_sds")$age, sd(dat$age))

  pred <- predict(result, pred_data = dat)$Prediction
  expect_true(all(pred >= 0 & pred <= 1))
  expect_equal(as.numeric(pred), as.numeric(result$model$fitted.values))
})

test_that("the scaling factor stored with the model drives the rescaling", {
  ## guards the bug where prediction rescaling assumed a factor of 2 rather
  ## than reading the factor the model was actually estimated with
  result <- nn_reg()
  ms <- attr(result$model$model, "radiant_ms")
  sds <- attr(result$model$model, "radiant_sds")

  pred2 <- predict(result, pred_data = diamonds)$Prediction

  ## pretend the model had been standardized by 1 X SD
  attr(result$model$model, "radiant_sf") <- 1
  pred1 <- predict(result, pred_data = diamonds)$Prediction

  ## predictors are now scaled with sf = 1 and the response is converted back
  ## with sf = 1, which has to be what the prediction code does
  scaled <- diamonds[, c("carat", "clarity")]
  scaled$carat <- (scaled$carat - ms$carat) / (1 * sds$carat)
  expected <- as.numeric(predict(result$model, scaled)) * 1 * sds$price + ms$price

  expect_equal(as.numeric(pred1), expected)
  expect_false(isTRUE(all.equal(as.numeric(pred1), as.numeric(pred2))))
})

test_that("nn honors the standardize-1sd and standardize-2sd options", {
  ## before this was handled, nn silently skipped standardization altogether
  ## when given a value it did not recognize
  for (chk in c("standardize", "standardize-2sd")) {
    result <- nn_reg(check = chk)
    expect_equal(attr(result$model$model, "radiant_sf"), 2)
    expect_equal(result$check, "standardize")
  }

  result <- nn_reg(check = "standardize-1sd")
  expect_equal(attr(result$model$model, "radiant_sf"), 1)
  expect_equal(result$check, "standardize")
  expect_equal(
    result$model$model$carat,
    (diamonds$carat - mean(diamonds$carat)) / (1 * sd(diamonds$carat))
  )
})

test_that("nn predictions round-trip for any scaling factor", {
  for (chk in c("standardize-1sd", "standardize-2sd")) {
    result <- nn_reg(check = chk)
    sf <- attr(result$model$model, "radiant_sf")
    ms <- attr(result$model$model, "radiant_ms")$price
    sds <- attr(result$model$model, "radiant_sds")$price
    expect_equal(
      as.numeric(predict(result, pred_data = diamonds)$Prediction),
      as.numeric(result$model$fitted.values) * sf * sds + ms
    )
  }
})

test_that("nn standardized by 1 X SD matches an independent implementation", {
  dat <- diamonds[, c("price", "carat", "clarity")]
  manual <- dat
  for (v in c("price", "carat")) {
    manual[[v]] <- (manual[[v]] - mean(dat[[v]])) / (1 * sd(dat[[v]]))
  }
  set.seed(1234)
  mod <- nnet::nnet(
    price ~ ., data = manual, rang = .1, size = 1, decay = .5, weights = NULL,
    maxit = 10000, linout = TRUE, entropy = FALSE, skip = FALSE, trace = FALSE
  )
  expected <- as.numeric(predict(mod, manual)) * 1 * sd(dat$price) + mean(dat$price)

  result <- nn_reg(check = "standardize-1sd")
  expect_equal(as.numeric(predict(result, pred_data = dat)$Prediction), expected)
})

# library(radiant.model)
# library(testthat)

######### tests ########
context("Prediction intervals for linear regression")

## "confidence" gives an interval around the mean of y given x, "prediction"
## gives an interval around a single new value of y. The second is wider
## because it also carries the residual variance.

reg_res <- function(...) regress(diamonds, "price", c("carat", "clarity"), ...)

newdat <- function() {
  data.frame(
    carat = c(0.5, 1, 1.5),
    clarity = factor("SI1", levels = levels(diamonds$clarity))
  )
}

test_that("confidence intervals match predict.lm", {
  result <- reg_res()
  pred <- predict(result, pred_data = newdat(), interval = "confidence")
  ref <- predict(result$model, newdata = newdat(), interval = "confidence", level = 0.95)

  expect_equal(as.numeric(pred$Prediction), as.numeric(ref[, "fit"]))
  expect_equal(as.numeric(pred[["2.5%"]]), as.numeric(ref[, "lwr"]))
  expect_equal(as.numeric(pred[["97.5%"]]), as.numeric(ref[, "upr"]))
  expect_equal(attr(pred, "radiant_interval"), "confidence")
})

test_that("prediction intervals match predict.lm", {
  result <- reg_res()
  pred <- predict(result, pred_data = newdat(), interval = "prediction")
  ref <- predict(result$model, newdata = newdat(), interval = "prediction", level = 0.95)

  expect_equal(as.numeric(pred$Prediction), as.numeric(ref[, "fit"]))
  expect_equal(as.numeric(pred[["2.5%"]]), as.numeric(ref[, "lwr"]))
  expect_equal(as.numeric(pred[["97.5%"]]), as.numeric(ref[, "upr"]))
  expect_equal(attr(pred, "radiant_interval"), "prediction")

  ## the reported +/- is half the width of the interval
  expect_equal(as.numeric(pred[["+/-"]]), as.numeric(ref[, "upr"] - ref[, "fit"]))
})

test_that("confidence is the default and prediction intervals are wider", {
  result <- reg_res()
  ci <- predict(result, pred_data = newdat())
  pi <- predict(result, pred_data = newdat(), interval = "prediction")

  expect_equal(attr(ci, "radiant_interval"), "confidence")

  ## same point estimate, strictly wider interval
  expect_equal(as.numeric(ci$Prediction), as.numeric(pi$Prediction))
  expect_true(all(pi[["2.5%"]] < ci[["2.5%"]]))
  expect_true(all(pi[["97.5%"]] > ci[["97.5%"]]))
  expect_true(all(pi[["+/-"]] > ci[["+/-"]]))
})

test_that("the confidence level is respected for both interval types", {
  result <- reg_res()
  for (int in c("confidence", "prediction")) {
    p90 <- predict(result, pred_data = newdat(), interval = int, conf_lev = 0.90)
    p95 <- predict(result, pred_data = newdat(), interval = int, conf_lev = 0.95)
    ref <- predict(result$model, newdata = newdat(), interval = int, level = 0.90)

    expect_equal(as.numeric(p90[["5%"]]), as.numeric(ref[, "lwr"]))
    expect_equal(as.numeric(p90[["95%"]]), as.numeric(ref[, "upr"]))
    ## a 90% interval is narrower than a 95% interval
    expect_true(all(p90[["+/-"]] < p95[["+/-"]]))
  }
})

test_that("no interval is reported when se is FALSE or interval is none", {
  result <- reg_res()
  for (pred in list(
    predict(result, pred_data = newdat(), interval = "none"),
    predict(result, pred_data = newdat(), se = FALSE)
  )) {
    expect_equal(attr(pred, "radiant_interval"), "none")
    expect_false("2.5%" %in% colnames(pred))
    expect_true("Prediction" %in% colnames(pred))
  }
})

test_that("intervals are not available for standardized or centered models", {
  for (chk in c("standardize", "standardize-1sd", "center")) {
    result <- reg_res(check = chk)
    pred <- suppressMessages(predict(result, pred_data = newdat(), interval = "prediction"))

    ## no interval columns ...
    expect_equal(attr(pred, "radiant_interval"), "none")
    expect_false("2.5%" %in% colnames(pred))

    ## ... and the output says why rather than staying silent about it
    note <- attr(pred, "radiant_interval_note")
    expect_true(grepl("standardized or centered", note))
    expect_true(any(grepl(
      "Interval             : none (not available when coefficients are standardized or centered)",
      capture.output(print(pred)), fixed = TRUE
    )))
  }

  ## predictions themselves are still correct, they are just interval-free
  expect_equal(
    as.numeric(suppressMessages(predict(reg_res(check = "standardize-1sd"), pred_data = newdat()))$Prediction),
    as.numeric(predict(reg_res(), pred_data = newdat())$Prediction)
  )
})

test_that("an unstandardized model still reports the interval type it used", {
  result <- reg_res()
  out <- capture.output(print(predict(result, pred_data = newdat(), interval = "prediction")))
  expect_true(any(grepl("Interval             : prediction", out, fixed = TRUE)))
  expect_equal(attr(predict(result, pred_data = newdat()), "radiant_interval_note"), "")
})

context("Prediction intervals for logistic regression")

## A prediction interval for a single new value of y is not available for a
## binary response. A linear model adds the residual variance on top of the
## uncertainty about the regression line, but for a binomial GLM the variance
## of y is p * (1 - p), which is already determined by the predicted
## probability. A 95% interval for a single 0/1 draw is the whole 0-1 range
## unless p is very close to 0 or 1, which is why predict.glm has no
## "prediction" option. What is reported is a confidence interval for the
## predicted probability, computed on the link scale and transformed back.

logit_res <- function(...) logistic(titanic, "survived", c("pclass", "sex", "age"), lev = "Yes", ...)

logit_newdat <- function() {
  data.frame(
    pclass = factor("1st", levels = levels(titanic$pclass)),
    sex = factor(c("female", "male"), levels = levels(titanic$sex)),
    age = c(30, 30)
  )
}

test_that("logistic confidence intervals match a Wald interval on the link scale", {
  result <- logit_res()
  pred <- predict(result, pred_data = logit_newdat(), interval = "confidence")

  lp <- predict(result$model, logit_newdat(), type = "link", se.fit = TRUE)
  z <- qnorm(.5 + 0.95 / 2)
  expect_equal(as.numeric(pred$Prediction), as.numeric(plogis(lp$fit)))
  expect_equal(as.numeric(pred[["2.5%"]]), as.numeric(plogis(lp$fit - z * lp$se.fit)))
  expect_equal(as.numeric(pred[["97.5%"]]), as.numeric(plogis(lp$fit + z * lp$se.fit)))

  ## bounds are probabilities and bracket the point estimate
  expect_true(all(pred[["2.5%"]] >= 0 & pred[["97.5%"]] <= 1))
  expect_true(all(pred[["2.5%"]] < pred$Prediction & pred$Prediction < pred[["97.5%"]]))
})

test_that("logistic falls back to a confidence interval if prediction is requested", {
  result <- logit_res()
  ci <- predict(result, pred_data = logit_newdat(), interval = "confidence")
  pi <- expect_message(predict(result, pred_data = logit_newdat(), interval = "prediction"))

  ## the numbers are the confidence interval ...
  expect_equal(as.numeric(pi[["2.5%"]]), as.numeric(ci[["2.5%"]]))
  expect_equal(as.numeric(pi[["97.5%"]]), as.numeric(ci[["97.5%"]]))

  ## ... and the output must say "confidence", not "prediction", so the
  ## reported interval is never mislabelled
  expect_equal(attr(pi, "radiant_interval"), "confidence")
  expect_true(any(grepl("Interval             : confidence", capture.output(print(pi)), fixed = TRUE)))
  expect_false(any(grepl("prediction", capture.output(print(pi)), fixed = TRUE)))
})

test_that("logistic confidence level is respected", {
  result <- logit_res()
  p90 <- predict(result, pred_data = logit_newdat(), conf_lev = 0.90)
  p95 <- predict(result, pred_data = logit_newdat(), conf_lev = 0.95)

  lp <- predict(result$model, logit_newdat(), type = "link", se.fit = TRUE)
  expect_equal(as.numeric(p90[["5%"]]), as.numeric(plogis(lp$fit - qnorm(.95) * lp$se.fit)))
  expect_true(all((p90[["95%"]] - p90[["5%"]]) < (p95[["97.5%"]] - p95[["2.5%"]])))
})

test_that("logistic reports no interval when asked, or when standardized", {
  result <- logit_res()
  none <- predict(result, pred_data = logit_newdat(), interval = "none")
  expect_equal(attr(none, "radiant_interval"), "none")
  expect_false("2.5%" %in% colnames(none))

  for (chk in c("standardize", "standardize-1sd", "center")) {
    pred <- suppressMessages(predict(logit_res(check = chk), pred_data = logit_newdat()))
    expect_equal(attr(pred, "radiant_interval"), "none")
    expect_false("2.5%" %in% colnames(pred))
    expect_true(any(grepl(
      "Interval             : none (not available when coefficients are standardized or centered)",
      capture.output(print(pred)), fixed = TRUE
    )))
  }

  ## the probabilities themselves are unaffected by standardization
  expect_equal(
    as.numeric(suppressMessages(predict(logit_res(check = "standardize-1sd"), pred_data = logit_newdat()))$Prediction),
    as.numeric(predict(result, pred_data = logit_newdat(), interval = "none")$Prediction)
  )
})

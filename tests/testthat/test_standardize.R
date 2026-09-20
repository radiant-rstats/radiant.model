# library(radiant.model)
# library(testthat)

######### tests ########
context("Standardization by 1 or 2 standard deviations")

test_that("scale_df uses the requested scaling factor", {
  dat <- data.frame(x = c(1, 2, 3, 4, 10), y = c(2, 4, 6, 8, 10))

  s2 <- scale_df(dat)
  s1 <- scale_df(dat, sf = 1)

  expect_equal(attr(s2, "radiant_sf"), 2)
  expect_equal(attr(s1, "radiant_sf"), 1)

  expect_equal(s2$x, (dat$x - mean(dat$x)) / (2 * sd(dat$x)))
  expect_equal(s1$x, (dat$x - mean(dat$x)) / (1 * sd(dat$x)))

  ## scaling by 1 X SD gives values that are twice as large
  expect_equal(s1$x, 2 * s2$x)
  expect_equal(s1$y, 2 * s2$y)

  ## means and sds stored are not affected by the scaling factor
  expect_equal(attr(s1, "radiant_ms"), attr(s2, "radiant_ms"))
  expect_equal(attr(s1, "radiant_sds"), attr(s2, "radiant_sds"))
})

test_that("scale_df falls back to 2 X SD for invalid scaling factors", {
  dat <- data.frame(x = c(1, 2, 3, 4, 10))
  ref <- scale_df(dat)
  for (sf in list(0, -1, NA, "", "abc", NULL)) {
    expect_equal(scale_df(dat, sf = sf)$x, ref$x)
  }
  expect_equal(scale_df(dat, sf = "1")$x, scale_df(dat, sf = 1)$x)
})

test_that("regress accepts the standardize options offered in the UI", {
  ## the option values used in the check boxes shown in Radiant
  r1 <- regress(diamonds, "price", c("carat", "clarity"), check = "standardize-1sd")
  r2 <- regress(diamonds, "price", c("carat", "clarity"), check = "standardize-2sd")

  expect_equal(r1$sf, 1)
  expect_equal(r2$sf, 2)
  expect_equal(attr(r1$model$model, "radiant_sf"), 1)
  expect_equal(attr(r2$model$model, "radiant_sf"), 2)

  ## check is simplified so the rest of the code only sees "standardize"
  expect_equal(r1$check, "standardize")
  expect_equal(r2$check, "standardize")

  res1 <- capture.output(summary(r1))
  res2 <- capture.output(summary(r2))
  expect_true(any(grepl("Standardized coefficients shown (1 X SD)", res1, fixed = TRUE)))
  expect_true(any(grepl("Standardized coefficients shown (2 X SD)", res2, fixed = TRUE)))
})

test_that("regress treats standardize as standardize-2sd", {
  r0 <- regress(diamonds, "price", c("carat", "clarity"), check = "standardize")
  r2 <- regress(diamonds, "price", c("carat", "clarity"), check = "standardize-2sd")
  expect_equal(r0$sf, 2)
  expect_equal(coef(r0$model), coef(r2$model))
})

test_that("regress standardize options combine with other check options", {
  result <- regress(
    diamonds, "price", c("carat", "clarity"),
    check = c("standardize-1sd", "robust")
  )
  expect_equal(result$sf, 1)
  expect_setequal(result$check, c("standardize", "robust"))
  res <- capture.output(summary(result))
  expect_true(any(grepl("Standardized coefficients shown (1 X SD)", res, fixed = TRUE)))
  expect_true(any(grepl("Robust standard errors used", res, fixed = TRUE)))
})

test_that("regress coefficients scale as expected with 1 vs 2 SD", {
  r2 <- regress(diamonds, "price", c("carat", "clarity"), check = "standardize-2sd")
  r1 <- regress(diamonds, "price", c("carat", "clarity"), check = "standardize-1sd")

  cf2 <- coef(r2$model)
  cf1 <- coef(r1$model)
  expect_equal(names(cf1), names(cf2))

  ## response and numeric explanatory variables are both scaled so the
  ## coefficient for a numeric variable does not change
  expect_equal(cf1[["carat"]], cf2[["carat"]])

  ## only the response is scaled for dummy variables so those coefficients,
  ## and the intercept, double when standardizing by 1 rather than 2 X SD
  dummies <- setdiff(names(cf2), "carat")
  expect_equal(cf1[dummies], 2 * cf2[dummies])
})

test_that("regress standardized by 1 X SD matches manual calculation", {
  dat <- dplyr::select(diamonds, price, carat, table)
  manual <- dat
  for (v in colnames(manual)) {
    manual[[v]] <- (manual[[v]] - mean(manual[[v]])) / sd(manual[[v]])
  }
  cf_manual <- coef(lm(price ~ carat + table, data = manual))
  cf_radiant <- coef(
    regress(dat, "price", c("carat", "table"), check = "standardize-1sd")$model
  )
  expect_equal(unname(cf_radiant), unname(cf_manual))
})

test_that("regress predictions are unaffected by the number of SDs used", {
  r0 <- regress(diamonds, "price", c("carat", "clarity"))
  r2 <- regress(diamonds, "price", c("carat", "clarity"), check = "standardize-2sd")
  r1 <- regress(diamonds, "price", c("carat", "clarity"), check = "standardize-1sd")

  p0 <- predict(r0, pred_cmd = "carat = 1:3")$Prediction
  expect_equal(predict(r2, pred_cmd = "carat = 1:3")$Prediction, p0)
  expect_equal(predict(r1, pred_cmd = "carat = 1:3")$Prediction, p0)
})

test_that("regress is unaffected when no standardize option is selected", {
  r0 <- regress(diamonds, "price", c("carat", "clarity"))
  expect_equal(r0$sf, 2)

  c0 <- regress(diamonds, "price", c("carat", "clarity"), check = "center")
  c1 <- regress(diamonds, "price", c("carat", "clarity"), check = "center", sf = 1)
  expect_equal(coef(c1$model), coef(c0$model))
})

test_that("logistic accepts the standardize options offered in the UI", {
  l1 <- logistic(titanic, "survived", c("pclass", "sex", "age"), lev = "Yes", check = "standardize-1sd")
  l2 <- logistic(titanic, "survived", c("pclass", "sex", "age"), lev = "Yes", check = "standardize-2sd")

  expect_equal(l1$sf, 1)
  expect_equal(l2$sf, 2)
  expect_equal(attr(l1$model$model, "radiant_sf"), 1)
  expect_equal(attr(l2$model$model, "radiant_sf"), 2)
  expect_equal(l1$check, "standardize")
  expect_equal(l2$check, "standardize")

  res1 <- capture.output(summary(l1))
  res2 <- capture.output(summary(l2))
  expect_true(any(grepl("Standardized odds-ratios and coefficients shown (1 X SD)", res1, fixed = TRUE)))
  expect_true(any(grepl("Standardized odds-ratios and coefficients shown (2 X SD)", res2, fixed = TRUE)))
})

test_that("logistic treats standardize as standardize-2sd", {
  l0 <- logistic(titanic, "survived", c("pclass", "sex", "age"), lev = "Yes", check = "standardize")
  l2 <- logistic(titanic, "survived", c("pclass", "sex", "age"), lev = "Yes", check = "standardize-2sd")
  expect_equal(l0$sf, 2)
  expect_equal(coef(l0$model), coef(l2$model))
})

test_that("logistic coefficients scale as expected with 1 vs 2 SD", {
  l2 <- logistic(titanic, "survived", c("pclass", "sex", "age"), lev = "Yes", check = "standardize-2sd")
  l1 <- logistic(titanic, "survived", c("pclass", "sex", "age"), lev = "Yes", check = "standardize-1sd")

  cf2 <- coef(l2$model)
  cf1 <- coef(l1$model)

  ## the response is binary so only numeric explanatory variables are scaled.
  ## the coefficient for age is half as large when standardizing by 1 X SD
  expect_equal(cf1[["age"]], cf2[["age"]] / 2)

  ## coefficients for dummy variables and the intercept are unchanged
  not_num <- setdiff(names(cf2), "age")
  expect_equal(cf1[not_num], cf2[not_num])

  ## odds-ratios reported follow the coefficients
  or1 <- l1$coeff$OR[l1$coeff$label == "age"]
  expect_equal(or1, exp(cf1[["age"]]))
})

test_that("logistic standardized by 1 X SD matches manual calculation", {
  dat <- dplyr::select(titanic, survived, age, fare)
  manual <- dat
  manual$survived <- manual$survived == "Yes"
  for (v in c("age", "fare")) {
    manual[[v]] <- (manual[[v]] - mean(manual[[v]])) / sd(manual[[v]])
  }
  cf_manual <- coef(glm(survived ~ age + fare, family = binomial(link = "logit"), data = manual))
  cf_radiant <- coef(
    logistic(dat, "survived", c("age", "fare"), lev = "Yes", check = "standardize-1sd")$model
  )
  expect_equal(unname(cf_radiant), unname(cf_manual))
})

test_that("logistic predictions are unaffected by the number of SDs used", {
  l0 <- logistic(titanic, "survived", c("pclass", "sex", "age"), lev = "Yes")
  l2 <- logistic(titanic, "survived", c("pclass", "sex", "age"), lev = "Yes", check = "standardize-2sd")
  l1 <- logistic(titanic, "survived", c("pclass", "sex", "age"), lev = "Yes", check = "standardize-1sd")

  p0 <- predict(l0, pred_cmd = "age = 10:20")$Prediction
  expect_equal(predict(l2, pred_cmd = "age = 10:20")$Prediction, p0)
  expect_equal(predict(l1, pred_cmd = "age = 10:20")$Prediction, p0)
})

test_that("the sf argument is used when check does not specify a number of SDs", {
  ref <- regress(diamonds, "price", c("carat", "clarity"), check = "standardize-1sd")
  alt <- regress(diamonds, "price", c("carat", "clarity"), check = "standardize", sf = 1)
  expect_equal(alt$sf, 1)
  expect_equal(coef(alt$model), coef(ref$model))

  ## an explicit number of SDs in check takes precedence over sf
  ovr <- regress(diamonds, "price", c("carat", "clarity"), check = "standardize-2sd", sf = 1)
  expect_equal(ovr$sf, 2)
})

test_that("invalid sf values fall back to the default of 2", {
  ref <- coef(regress(diamonds, "price", c("carat", "clarity"), check = "standardize")$model)
  for (sf in list(0, -2, NA, "abc")) {
    result <- regress(diamonds, "price", c("carat", "clarity"), check = "standardize", sf = sf)
    expect_equal(result$sf, 2)
    expect_equal(coef(result$model), ref)
  }
})

test_that("crtree and mnl also understand the standardize options", {
  ## these share the standardization code path, so an unrecognized value used
  ## to mean no standardization at all rather than an error
  ct1 <- crtree(titanic, "survived", c("pclass", "sex", "age"), lev = "Yes", check = "standardize-1sd")
  ct2 <- crtree(titanic, "survived", c("pclass", "sex", "age"), lev = "Yes", check = "standardize")
  expect_equal(attr(ct1$model$model, "radiant_sf"), 1)
  expect_equal(attr(ct2$model$model, "radiant_sf"), 2)
  expect_equal(ct1$check, "standardize")
  expect_equal(
    ct1$model$model$age,
    (titanic$age - mean(titanic$age)) / (1 * sd(titanic$age))
  )

  ml1 <- mnl(ketchup, "choice", c("price.heinz28", "price.hunts32"), lev = "heinz28", check = "standardize-1sd")
  ml2 <- mnl(ketchup, "choice", c("price.heinz28", "price.hunts32"), lev = "heinz28", check = "standardize-2sd")
  expect_equal(attr(ml1$model$model, "radiant_sf"), 1)
  expect_equal(attr(ml2$model$model, "radiant_sf"), 2)

  ## the summary reports the factor that was actually used
  expect_true(any(grepl("Standardized RRRs and coefficients shown (1 X SD)", capture.output(summary(ml1)), fixed = TRUE)))
  expect_true(any(grepl("Standardized RRRs and coefficients shown (2 X SD)", capture.output(summary(ml2)), fixed = TRUE)))

  ## bare "standardize" stays equivalent to 2 X SD
  ml0 <- mnl(ketchup, "choice", c("price.heinz28", "price.hunts32"), lev = "heinz28", check = "standardize")
  expect_equal(coef(ml0$model), coef(ml2$model))
})

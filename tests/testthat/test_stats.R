trim <- function(x) gsub("^\\s+|\\s+$", "", x)

######### tests ########
context("Linear regression (regress)")

test_that("regress", {
  result <- regress("diamonds", "price", c("carat", "clarity"))
  res1 <- capture.output(summary(result))[10] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "carat           8438.030    51.101 165.125  < .001 ***"
  expect_equal(res1,res2)

  result <- regress("diamonds", "price", "carat:clarity")
  res1 <- capture.output(summary(result))[10] %>% trim
  expect_equal(res1,res2)

  res1 <- capture.output(summary(result)) %>% trim
  # cat(paste0(res1,"\n"), file = "~/gh/radiant/tests/testthat/output/regression1.txt")
  ## full output - cannot open file when testing the tests
  res2 <- paste0(readLines("output/regress1.txt")) %>% trim
  expect_equal(res1,res2)
})

test_that("regress - predict", {
  result <- regress("diamonds", "price", c("carat","clarity"))
  res1 <- capture.output(predict(result, pred_cmd = "carat = 1:10"))[16] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "SI1     9  72769.811 70229.110 75310.513 2540.701"
  expect_equal(res1,res2)

  result <- regress("diamonds", "price", "carat:clarity")
  res1 <- capture.output(predict(result, pred_cmd = "carat = 1:10"))[16] %>% trim
  expect_equal(res1,res2)
})

context("Logistic regression (logistic)")

test_that("logistic", {
  result <- logistic("titanic", "survived", c("pclass","sex"))
  res1 <- capture.output(summary(result))[13] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "sex|male    0.080      -2.522     0.163 -15.447  < .001 ***"
  expect_equal(res1,res2)

  result <- logistic("titanic", "survived", "pclass:sex")
  res1 <- capture.output(summary(result))[13] %>% trim
  expect_equal(res1,res2)
})

test_that("logistic - predict", {
  result <- logistic("titanic", "survived", c("pclass","sex"))
  res1 <- capture.output(predict(result, pred_cmd = "pclass = levels(pclass); sex = 'female'"))[10] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "2nd female      0.779"
  expect_equal(res1,res2)

  result <- logistic("titanic", "survived", "pclass:sex")
  res1 <- capture.output(predict(result, pred_cmd = "pclass = levels(pclass); sex = 'female'"))[10] %>% trim
  expect_equal(res1,res2)

  res1 <- capture.output(predict(result, pred_data = "titanic"))[10] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "1st female      0.896"
  expect_equal(res1,res2)
})

context("ANN (ann)")

test_that("ann - predict for classification", {
  result <- ann("titanic", "survived", c("pclass","sex"), seed = 1234)
  res1 <- capture.output(predict(result, pred_cmd = "pclass = levels(pclass); sex = 'female'", dec = 1))[10] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "2nd female        0.8"
  expect_equal(res1,res2)

  result <- ann("titanic", "survived", "pclass:sex", seed = 1234)
  res1 <- capture.output(predict(result, pred_cmd = "pclass = levels(pclass); sex = 'female'", dec = 1))[10] %>% trim
  expect_equal(res1,res2)

  res1 <- capture.output(predict(result, pred_data = "titanic", dec = 1))[10] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "1st female        0.9"
  expect_equal(res1,res2)
})

test_that("ann - predict for regression", {
  result <- ann("diamonds", "price", c("carat","clarity"), type = "regression",  seed = 1234)
  res1 <- capture.output(predict(result, pred_cmd = "carat = 1:10", dec = 1))[16] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "SI1     9    18466.7"
  expect_equal(res1,res2)

  result <- ann("diamonds", "price", "carat:clarity", type = "regression",  seed = 1234)
  res1 <- capture.output(predict(result, pred_cmd = "carat = 1:10", dec = 1))[16] %>% trim
  expect_equal(res1,res2)

  res1 <- capture.output(predict(result, pred_data = "diamonds", dec = 1))[16] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "0.9     SI1     3997.9"
  expect_equal(res1,res2)
})

context("Linear regression (plot.regress)")

test_that("regress - plots", {
  result <- regress("diamonds", "price", c("carat", "clarity"))
  grb <- plot(result, plots = "dashboard", shiny = TRUE)
  expect_true(all(c("gtable","grob") %in% class(grb)))
  expect_equal(try(print(grb), silent = TRUE), NULL)
  unlink("Rplots.pdf")
})

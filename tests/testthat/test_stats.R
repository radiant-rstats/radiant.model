trim_trailing <- function(x) sub("\\s+$", "", x)
trim_leading <- function(x) sub("^\\s+", "", x)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

compare_output <- function(res1, res2) {
  for (i in 1:length(res2)) {
    if (res1[i] != res2[i]) {
      print(i)
      print(res1[i])
      print(res2[i])
    }
  }
}

######### tests ########
context("Regress")

test_that("regress", {
  result <- regress("diamonds", "price", c("carat", "clarity"))
  res1 <- capture.output(summary(result))[10] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "carat           8438.030    51.101 165.125  < .001 ***"
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
})

test_that("logistic", {
  result <- logistic("titanic", "survived", c("pclass","sex"))
  res1 <- capture.output(summary(result))[13] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "sex|male    0.080      -2.522     0.163 -15.447  < .001 ***"
  expect_equal(res1,res2)
})

test_that("logistic - predict", {
  result <- logistic("titanic", "survived", c("pclass","sex"))
  res1 <- capture.output(predict(result, pred_cmd = "pclass = levels(pclass); sex = 'female'"))[9] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "2nd female      0.779"
  expect_equal(res1,res2)

  res1 <- capture.output(predict(result, pred_data = "titanic"))[9] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "1st female      0.896"
  expect_equal(res1,res2)
})

test_that("ann - predict for classification", {
  result <- ann("titanic", "survived", c("pclass","sex"), seed = 1234)
  res1 <- capture.output(predict(result, pred_cmd = "pclass = levels(pclass); sex = 'female'"))[9] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "2nd female      0.845"
  expect_equal(res1,res2)

  res1 <- capture.output(predict(result, pred_data = "titanic"))[9] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "1st female      0.912"
  expect_equal(res1,res2)
})

test_that("ann - predict for regression", {
  result <- ann("diamonds", "price", c("carat","clarity"), type = "regression", seed = 1234)
  res1 <- capture.output(predict(result, pred_cmd = "carat = 1:10"))[16] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "SI1     9  16869.413"
  expect_equal(res1,res2)

  res1 <- capture.output(predict(result, pred_data = "diamonds"))[16] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "0.930     SI1   3810.016"
  expect_equal(res1,res2)
})

test_that("regress - plots", {
  result <- regress("diamonds", "price", c("carat", "clarity"))
  grb <- plot(result, plots = "dashboard", shiny = TRUE)
  # class(grb)
  # library(testthat)
  expect_true(all(c("gtable","grob") %in% class(grb)))
  expect_equal(try(print(grb), silent = TRUE), NULL)
  # expect_true(file.exists("Rplots.pdf"))  # not always created it seems
  unlink("Rplots.pdf")

  # useful for interactive testing - keep plots out of build
#   png("output/regression1.png")
#     plot(result, reg_plots = "dashboard")
#   dev.off()
#   res1 <- readPNG("output/regression1.png")
#   res2 <- readPNG("output/regression1-correct.png")
#   expect_equal(res1,res2)
#   unlink("output/regression1.png")
#   rm(res1, res2)
})

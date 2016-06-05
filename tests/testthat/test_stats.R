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

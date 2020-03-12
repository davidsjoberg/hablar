library(testthat)
library(hablar)

context("cumulative")
test_that("cumulative", {
  expect_equal(cumsum_(c(1, 1, 1, 1)), c(1, 2, 3, 4))
  expect_equal(cumsum_(c(1, 1, NA, 1)), c(1, 2, 2, 3))

  expect_equal(cummean_(c(1, 1, NA, 1)), c(1, 1, 1, 1))
  expect_equal(cummean_(c(NA, NA, NA, NA)), as.double(c(NA, NA, NA, NA)))
  
  expect_equal(cum_unique(c(1, 2, 3, 4)), c(1, 2, 3, 4))
  expect_equal(cum_unique(c(1, 2, NA, 4)), c(1, 2, 3, 4))
  expect_equal(cum_unique_(c(1, 2, NA, 4)), c(1, 2, 2, 3))
})


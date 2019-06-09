library(testthat)
library(hablar)

context("n_unique")

test_that("n_unique", {
  expect_equal(n_unique(c(1, 1, 2)), 2L)
  expect_equal(n_unique(c(1, 1, 2, NA)), 3L)
})

test_that("n_unique_", {
  expect_equal(n_unique_(c(1, 1, 2)), 2L)
  expect_equal(n_unique_(c(1, 1, 2, NA)), 2L)
})


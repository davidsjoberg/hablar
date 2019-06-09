library(testthat)
library(hablar)

context("create_dummy")
test_that("dummy", {
  expect_equal(
    dummy(c(T, F, NA)), 
    as.integer(c(1, 0, NA))
  )
  expect_equal(
    dummy(c(T, F, NA)), 
    as.integer(c(1, 0, NA))
  )
  expect_equal(
    dummy(c(T, F, NA), missing = 99), 
    as.integer(c(1, 0, 99))
  )
  
  expect_error(
    dummy(c(1, 2, 3))
  )
})

context("create_dummy")
test_that("dummy_", {
  expect_equal(
    dummy_(c(T, F, NA)), 
    as.integer(c(1, 0, 0))
  )
  expect_equal(
    dummy_(c(T, F, NA)), 
    as.integer(c(1, 0, 0))
  )
  expect_equal(
    dummy_(c(T, F, NA), missing = 99), 
    as.integer(c(1, 0, 99))
  )
  
  expect_error(
    dummy(c(1, 2, 3))
  )
})


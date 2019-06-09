library(testthat)
library(hablar)

context("math")
test_that("two sided minus_", {
  expect_equal(1 %minus_% 2, 1-2)
  expect_equal(1 %minus_% NA, 1)
  expect_equal(NA %minus_% 3, -3)
  expect_equal(NaN %minus_% 3, -3)
  expect_equal(Inf %minus_% 3, -3)
})

test_that("two sided plus_", {
  expect_equal(1 %plus_% 2, 1+2)
  expect_equal(1 %plus_% NA, 1)
  expect_equal(NA %plus_% 3, 3)
  expect_equal(NaN %plus_% 3, 3)
  expect_equal(Inf %plus_% 3, 3)
})


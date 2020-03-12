library(testthat)
library(hablar)

context("math")
test_that("two sided minus_", {
  expect_equal(1 %minus_% 2, 1-2)
  expect_equal(1 %minus_% as.numeric(NA), 1)
  expect_equal(as.numeric(NA) %minus_% 3, -3)
})

test_that("two sided plus_", {
  expect_equal(1 %plus_% 2, 1+2)
  expect_equal(1 %plus_% as.numeric(NA), 1)
  expect_equal(as.numeric(NA) %plus_% 3, 3)
  
  df <- tibble(x = c(1, 2, 3),
               y = c(-1, -2, NA))
  expect_equal(df %>% mutate(z = x %plus_% y),
               tibble(x = c(1, 2, 3),
                      y = c(-1, -2, NA),
                      z = c(0, 0, 3)))
})


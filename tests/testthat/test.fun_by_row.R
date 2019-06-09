library(testthat)
library(hablar)
library(dplyr)

context("fun_by_row")

test_that("row_sum", {
  expect_equal(data.frame(a = 1, b = 2, c = NA) %>% mutate(sum = row_sum()),
               data.frame(a = 1, b = 2, c = NA, sum = as.numeric(NA))
  )
  expect_equal(data.frame(a = 1, b = 2, c = NaN) %>% mutate(sum = row_sum()),
               data.frame(a = 1, b = 2, c = NaN, sum = as.numeric(NA))
  )
  expect_equal(data.frame(a = 1, b = 2, c = -3) %>% mutate(sum = row_sum()),
               data.frame(a = 1, b = 2, c = -3, sum = as.numeric(0))
  )
  expect_equal(data.frame(a = 1, b = 2, c = -3) %>% mutate(sum = row_sum(a, b)),
               data.frame(a = 1, b = 2, c = -3, sum = as.numeric(3))
  )
  expect_equal(data.frame(a = 1, b = 2, c = -3) %>% mutate(sum = row_sum(b:c)),
               data.frame(a = 1, b = 2, c = -3, sum = as.numeric(-1))
  )
  
  expect_error(data.frame(a = 1, b = 2, c = -3) %>% group_by(cyl) %>% mutate(sum = row_sum(b:c)))
})

test_that("row_sum_", {
  expect_equal(data.frame(a = 1, b = 2, c = NA) %>% mutate(sum = row_sum_()),
               data.frame(a = 1, b = 2, c = NA, sum = as.numeric(3))
  )
  expect_equal(data.frame(a = 1, b = 2, c = NaN) %>% mutate(sum = row_sum_()),
               data.frame(a = 1, b = 2, c = NaN, sum = as.numeric(3))
  )
  expect_equal(data.frame(a = 1, b = 2, c = -3) %>% mutate(sum = row_sum_()),
               data.frame(a = 1, b = 2, c = -3, sum = as.numeric(0))
  )
  expect_equal(data.frame(a = 1, b = 2, c = -3) %>% mutate(sum = row_sum_(a, b)),
               data.frame(a = 1, b = 2, c = -3, sum = as.numeric(3))
  )
  expect_equal(data.frame(a = 1, b = 2, c = -3) %>% mutate(sum = row_sum_(b:c)),
               data.frame(a = 1, b = 2, c = -3, sum = as.numeric(-1))
  )
  
  expect_error(data.frame(a = 1, b = 2, c = -3) %>% group_by(cyl) %>% mutate(sum = row_sum_(b:c)))
})

test_that("row_mean", {
  expect_equal(data.frame(a = 1, b = 2, c = NA) %>% mutate(sum = row_mean()),
               data.frame(a = 1, b = 2, c = NA, sum = as.numeric(NA))
  )
  expect_equal(data.frame(a = 1, b = 2, c = NaN) %>% mutate(sum = row_mean()),
               data.frame(a = 1, b = 2, c = NaN, sum = as.numeric(NA))
  )
  expect_equal(data.frame(a = 1, b = 2, c = -3) %>% mutate(sum = row_mean()),
               data.frame(a = 1, b = 2, c = -3, sum = as.numeric(0))
  )
  expect_equal(data.frame(a = 1, b = 2, c = -3) %>% mutate(sum = row_mean(a, b)),
               data.frame(a = 1, b = 2, c = -3, sum = as.numeric(1.5))
  )
  expect_equal(data.frame(a = 1, b = 2, c = -3) %>% mutate(sum = row_mean(b:c)),
               data.frame(a = 1, b = 2, c = -3, sum = as.numeric(-0.5))
  )
  
  expect_error(data.frame(a = 1, b = 2, c = -3) %>% group_by(cyl) %>% mutate(sum = row_mean(b:c)))
})

test_that("row_mean_", {
  expect_equal(data.frame(a = 1, b = 2, c = NA) %>% mutate(sum = row_mean_()),
               data.frame(a = 1, b = 2, c = NA, sum = as.numeric(1.5))
  )
  expect_equal(data.frame(a = 1, b = 2, c = NaN) %>% mutate(sum = row_mean_()),
               data.frame(a = 1, b = 2, c = NaN, sum = as.numeric(1.5))
  )
  expect_equal(data.frame(a = 1, b = 2, c = -3) %>% mutate(sum = row_mean_()),
               data.frame(a = 1, b = 2, c = -3, sum = as.numeric(0))
  )
  expect_equal(data.frame(a = 1, b = 2, c = -3) %>% mutate(sum = row_mean_(a, b)),
               data.frame(a = 1, b = 2, c = -3, sum = as.numeric(1.5))
  )
  expect_equal(data.frame(a = 1, b = 2, c = -3) %>% mutate(sum = row_mean_(b:c)),
               data.frame(a = 1, b = 2, c = -3, sum = as.numeric(-0.5))
  )
  
  expect_error(data.frame(a = 1, b = 2, c = -3) %>% group_by(cyl) %>% mutate(sum = row_mean_(b:c)))
})


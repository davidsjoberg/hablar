library(testthat)
library(hablar)

###################################################
# s
###################################################

context("s()")
test_that("vectors", {
  expect_equal(s(as.numeric(c(1, 2))), as.numeric(c(1, 2)))
  expect_equal(s(as.numeric(c(1, NA))), as.numeric(c(1)))
  expect_equal(s(as.numeric(c(1, NA)), ignore_na = F), as.numeric(c(1, NA)))
  expect_equal(s(as.numeric(c(1.67, NA)), ignore_na = F), as.numeric(c(1.67, NA)))
  expect_equal(s(as.numeric(c(1.67, NA)), ignore_na = F), as.numeric(c(1.67, NA)))
  expect_equal(s(as.numeric(c(1.67, NaN)), ignore_na = F), as.numeric(c(1.67, NA)))
  expect_equal(s(as.numeric(c(NA, 1.67, NaN))), as.numeric(c(1.67)))
  expect_equal(s(as.numeric(c(NA, 1.67, NaN)), ignore_na = F), as.numeric(c(NA, 1.67, NA)))

  expect_equal(s(as.integer(c(9, NA))), as.integer(c(9)))
  expect_equal(s(as.integer(c(9, NA)), ignore_na = F), as.integer(c(9, NA)))

  expect_equal(s(as.Date(c("2018-05-18", NA))), as.Date(c("2018-05-18")))
  expect_equal(s(as.Date(c("2018-05-18", NA)), ignore_na = F), as.Date(c("2018-05-18", NA)))

  expect_equal(s(as.POSIXct(c("2018-05-18", NA))), as.POSIXct(c("2018-05-18")))
  expect_equal(s(as.POSIXct(c("2018-05-18", NA)), ignore_na = F), as.POSIXct(c("2018-05-18", NA)))

  expect_equal(s(as.logical(c(T, NA))), as.logical(c(T)))
  expect_equal(s(as.logical(c(T, NA)), ignore_na = F), as.logical(c(T, NA)))

  expect_equal(s(as.numeric(c(NA, NA)), ignore_na = F), NA)
  expect_equal(s(as.Date(c(NA, NA))), NA)
  expect_equal(s(as.POSIXct(c(NA, NA))), NA)
  expect_equal(s(as.character(c(NA, NA))), NA)
  expect_equal(s(as.integer(c(NA, NA))), NA)

  expect_error(s(as.factor(c(NA, NA))))
  expect_error(s(as.factor(c(2, 3, NA))))
})


test_that("s() and aggregators", {
  expect_equal(min(s(as.numeric(c(1, 2)))), as.numeric(c(1)))
  expect_equal(min(s(as.numeric(c()))), min(as.numeric(c(NA))))
  expect_equal(min(s(as.numeric(c(1, 2, NA)))), as.numeric(c(1)))
  expect_equal(min(s(as.numeric(c(NaN, 2, NA)))), as.numeric(c(2)))
  expect_equal(min(s(as.numeric(c(NaN, 2, Inf)))), as.numeric(c(2)))
  expect_equal(min(s(as.numeric(c(NaN, NA, Inf)))), as.numeric(NA))
  expect_equal(min(s(as.numeric(c(NaN, 2, NA)), ignore_na = F)), as.numeric(NA))

  expect_equal(max(s(as.numeric(c(1, 2)))), as.numeric(c(2)))
  expect_equal(max(s(as.numeric(c()))), max(as.numeric(c(NA))))
  expect_equal(max(s(as.numeric(c(1, 2, NA)))), as.numeric(c(2)))
  expect_equal(max(s(as.numeric(c(NaN, 2, NA)))), as.numeric(c(2)))
  expect_equal(max(s(as.numeric(c(NaN, 2, Inf)))), as.numeric(c(2)))
  expect_equal(max(s(as.numeric(c(NaN, NA, Inf)))), as.numeric(NA))
  expect_equal(max(s(as.numeric(c(NaN, 2, NA)), ignore_na = F)), as.numeric(NA))

  expect_equal(mean(s(as.numeric(c(1, 2)))), as.numeric(c(1.5)))
  expect_equal(mean(s(as.numeric(c()))), mean(as.numeric(c(NA))))
  expect_equal(mean(s(as.numeric(c(1, 2, NA)))), as.numeric(c(1.5)))
  expect_equal(mean(s(as.numeric(c(NaN, 2, NA)))), as.numeric(c(2)))
  expect_equal(mean(s(as.numeric(c(NaN, 2, Inf)))), as.numeric(c(2)))
  expect_equal(mean(s(as.numeric(c(NaN, NA, Inf)))), as.numeric(NA))
  expect_equal(mean(s(as.numeric(c(NaN, 2, NA)), ignore_na = F)), as.numeric(NA))

  expect_equal(first(s(as.numeric(c(1, 2)))), as.numeric(c(1)))
  expect_equal(first(s(as.numeric(c()))), first(c(NA)))
  expect_equal(first(s(as.numeric(c(1, 2, NA)))), as.numeric(c(1)))
  expect_equal(first(s(as.numeric(c(NaN, 2, NA)))), as.numeric(c(2)))
  expect_equal(first(s(as.numeric(c(NaN, 2, Inf)))), as.numeric(c(2)))
  expect_equal(first(s(as.numeric(c(NaN, NA, Inf)))), NA)
  expect_equal(first(s(as.numeric(c(NaN, 2, NA)), ignore_na = F)), as.numeric(NA))

  expect_error(s(as.factor(c(2, 3, NA))))
})


test_that("s() and aggregators - wrappers", {
  expect_equal(min_(as.numeric(c(1, 2))), as.numeric(c(1)))
  expect_equal(min_(as.numeric(c())), min(as.numeric(c(NA))))
  expect_equal(min_(as.numeric(c(1, 2, NA))) , as.numeric(c(1)))
  expect_equal(min_(as.numeric(c(NaN, 2, NA))), as.numeric(c(2)))

  expect_equal(min_(as.numeric(c(NaN, 2, Inf))), as.numeric(c(2)))
  expect_equal(min_(as.numeric(c(NaN, NA, Inf))), as.numeric(NA))
  expect_equal(min_(as.numeric(c(NaN, 2, NA)), ignore_na = F), as.numeric(NA))

  expect_equal(max_(as.numeric(c(1, 2))), as.numeric(c(2)))
  expect_equal(max_(as.numeric(c())), max(as.numeric(c(NA))))
  expect_equal(max_(as.numeric(c(1, 2, NA))), as.numeric(c(2)))
  expect_equal(max_(as.numeric(c(NaN, 2, NA))), as.numeric(c(2)))
  expect_equal(max_(as.numeric(c(NaN, 2, Inf))), as.numeric(c(2)))
  expect_equal(max_(as.numeric(c(NaN, NA, Inf))), as.numeric(NA))
  expect_equal(max_(as.numeric(c(NaN, 2, NA)), ignore_na = F), as.numeric(NA))

  expect_equal(mean_(as.numeric(c(1, 2))), as.numeric(c(1.5)))
  expect_equal(mean_(as.numeric(c())), mean(as.numeric(c(NA))))
  expect_equal(mean_(as.numeric(c(1, 2, NA))), as.numeric(c(1.5)))
  expect_equal(mean_(as.numeric(c(NaN, 2, NA))), as.numeric(c(2)))
  expect_equal(mean_(as.numeric(c(NaN, 2, Inf))), as.numeric(c(2)))
  expect_equal(mean_(as.numeric(c(NaN, NA, Inf))), as.numeric(NA))
  expect_equal(mean_(as.numeric(c(NaN, 2, NA)), ignore_na = F), as.numeric(NA))

  expect_equal(first_(as.numeric(c(1, 2))), as.numeric(c(1)))
  expect_equal(first_(as.numeric(c())), first(c(NA)))
  expect_equal(first_(as.numeric(c(1, 2, NA))), as.numeric(c(1)))
  expect_equal(first_(as.numeric(c(NaN, 2, NA))), as.numeric(c(2)))
  expect_equal(first_(as.numeric(c(NaN, 2, Inf))), as.numeric(c(2)))
  expect_equal(first_(as.numeric(c(NaN, NA, Inf))), NA)
  expect_equal(first_(as.numeric(c(NaN, 2, NA)), ignore_na = F), as.numeric(NA))

  expect_error(min_(as.factor(c(2, 3, NA))))
  expect_equal(length(mean_(as.numeric(c()))), 1)
})


test_that("s() and aggregators - dplyr", {
  expect_equal(mtcars %>% mutate(max_gear = max_(gear[vs == 2])), mtcars %>% mutate(max_gear = as.numeric(NA)))
})

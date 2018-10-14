library(testthat)
library(hablar)


###################################################
# rationalize
###################################################

context("rationalize")
test_that("vectors", {
  expect_equal(rationalize(as.numeric(c(Inf))), as.numeric(c(NA)))
  expect_equal(rationalize(as.numeric(c(-Inf))), as.numeric(c(NA)))
  expect_equal(rationalize(as.numeric(c(NaN))), as.numeric(c(NA)))
  expect_equal(rationalize(as.numeric(c(NaN, 1, NA))), as.numeric(c(NA, 1, NA)))
  expect_equal(rationalize(as.numeric(c(NaN, Inf, 5))), as.numeric(c(NA, NA, 5)))
  expect_equal(rationalize(as.numeric(c(NaN, Inf, 5.67))), as.numeric(c(NA, NA, 5.67)))
  expect_equal(rationalize(as.numeric()), as.numeric())
  expect_equal(rationalize(as.Date(c(NA))), as.Date(c(NA)))

  expect_equal(rationalize(as.integer(c(1, 2, 3))), as.integer(c(1, 2, 3)))
  expect_equal(rationalize(as.integer(c(1, 2, 3, NA))), as.integer(c(1, 2, 3, NA)))
  expect_equal(rationalize(as.character(c("a", "Inf", "b", NA))), as.character(c("a", "Inf", "b", NA)))

  expect_equal(rationalize(list("a", "b")), list("a", "b"))
})

test_that("data.frame", {
  df <- tibble(a = as.numeric(c(NA, Inf, 3.67, NaN, -Inf)),
               b = as.numeric(c(NA, Inf, 3.67, 3, 4)),
               c = as.numeric(c(NA, NA, NA, NA, NA)),
               d = as.character(c(rep("foo", 3), rep("bar", 2))),
               e = as.integer(c(1, 2, 3, NA, 5)))

  expect_equal(rationalize(df), tibble(a = as.numeric(c(NA, NA, 3.67, NA, NA)),
                                       b = as.numeric(c(NA, NA, 3.67, 3, 4)),
                                       c = as.numeric(c(NA, NA, NA, NA, NA)),
                                       d = as.character(c(rep("foo", 3), rep("bar", 2))),
                                       e = as.integer(c(1, 2, 3, NA, 5))))

  expect_equal(rationalize(df, a), tibble(a = as.numeric(c(NA, NA, 3.67, NA, NA)),
                                          b = as.numeric(c(NA, Inf, 3.67, 3, 4)),
                                          c = as.numeric(c(NA, NA, NA, NA, NA)),
                                          d = as.character(c(rep("foo", 3), rep("bar", 2))),
                                          e = as.integer(c(1, 2, 3, NA, 5))))

  expect_equal(rationalize(df, -a), tibble(a = as.numeric(c(NA, Inf, 3.67, NaN, -Inf)),
                                           b = as.numeric(c(NA, NA, 3.67, 3, 4)),
                                           c = as.numeric(c(NA, NA, NA, NA, NA)),
                                           d = as.character(c(rep("foo", 3), rep("bar", 2))),
                                           e = as.integer(c(1, 2, 3, NA, 5))))

  expect_equal(rationalize(df, 1:5), tibble(a = as.numeric(c(NA, NA, 3.67, NA, NA)),
                                            b = as.numeric(c(NA, NA, 3.67, 3, 4)),
                                            c = as.numeric(c(NA, NA, NA, NA, NA)),
                                            d = as.character(c(rep("foo", 3), rep("bar", 2))),
                                            e = as.integer(c(1, 2, 3, NA, 5))))

  expect_equal(rationalize(df, c:e), df)
  expect_equal(df %>% rationalize(c:e), df)
  expect_error(rationalize(df, q))
})

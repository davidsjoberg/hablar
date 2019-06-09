library(testthat)
library(hablar)
library(dplyr)

context("find_in_df")

test_that("find_duplicates", {
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_duplicates(),
               data.frame(a = c(1, 1),
                          b = c(1, 1),
                          c = c(3, 3))
               )
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_duplicates(a, b),
               data.frame(a = c(1, 1),
                          b = c(1, 1),
                          c = c(3, 3))
  )
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_duplicates(-c),
               data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_duplicates(a, b)
  )
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_duplicates(a:b),
               data.frame(a = c(1, 1),
                          b = c(1, 1),
                          c = c(3, 3))
  )
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_duplicates(),
               data.frame(a = c(1, 1),
                          b = c(1, 1),
                          c = c(3, 3))
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_duplicates(),
               tibble(a = c(1, 1),
                          b = c(1, 1),
                          c = c(3, 3))
  )
  
  expect_error(c(1, 2) %>% find_duplicates())
})

test_that("find_na", {
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_na(),
               data.frame(a = c(2),
                          b = as.numeric(c(NA)),
                          c = as.numeric(c(NA)))
  )
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_na(b),
               data.frame(a = c(2),
                          b = as.numeric(c(NA)),
                          c = as.numeric(c(NA)))
  )
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_na(a),
               data.frame(a = as.numeric(c()),
                          b = as.numeric(c()),
                          c = as.numeric(c()))
  )
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_na(-b),
               data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_na(a, c)
  )
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_na(a:b),
               data.frame(a = c(2),
                          b = c(NA) %>% as.numeric(),
                          c = c(NA) %>% as.numeric())
  )
  
  expect_error(c(1, 2) %>% find_na())
})

test_that("find_nan", {
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NaN)) %>% find_nan(),
               data.frame(a = c(2),
                          b = as.numeric(c(NA)),
                          c = as.numeric(c(NaN)))
  )
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NaN)) %>% find_nan(b),
               data.frame(a = as.numeric(c()),
                          b = as.numeric(c()),
                          c = as.numeric(c()))
  )
  expect_equal(data.frame(a = c(NaN, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_nan(a),
               data.frame(a = as.numeric(c(NaN)),
                          b = as.numeric(c(1)),
                          c = as.numeric(c(3)))
  )
  expect_equal(data.frame(a = c(NaN, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_nan(-b),
               data.frame(a = c(NaN, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_nan(a, c)
  )
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, NaN, NA),
                          c = c(3, 3, NA)) %>% find_nan(a:b),
               data.frame(a = c(1),
                          b = c(NaN) %>% as.numeric(),
                          c = c(3) %>% as.numeric())
  )
  
  expect_error(c(1, 2) %>% find_nan())
})

test_that("find_inf", {
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, Inf)) %>% find_inf(),
               data.frame(a = c(2),
                          b = as.numeric(c(NA)),
                          c = as.numeric(c(Inf)))
  )
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, Inf)) %>% find_inf(b),
               data.frame(a = as.numeric(c()),
                          b = as.numeric(c()),
                          c = as.numeric(c()))
  )
  expect_equal(data.frame(a = c(Inf, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_inf(a),
               data.frame(a = as.numeric(c(Inf)),
                          b = as.numeric(c(1)),
                          c = as.numeric(c(3)))
  )
  expect_equal(data.frame(a = c(Inf, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_inf(-b),
               data.frame(a = c(Inf, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_inf(a, c)
  )
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, Inf, NA),
                          c = c(3, 3, NA)) %>% find_inf(a:b),
               data.frame(a = c(1),
                          b = c(Inf) %>% as.numeric(),
                          c = c(3) %>% as.numeric())
  )
  
  expect_error(c(1, 2, Inf) %>% find_inf())
})

test_that("find_irrational", {
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, Inf)) %>% find_irrational(),
               data.frame(a = c(2),
                          b = as.numeric(c(NA)),
                          c = as.numeric(c(Inf)))
  )
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, Inf)) %>% find_irrational(b),
               data.frame(a = as.numeric(c()),
                          b = as.numeric(c()),
                          c = as.numeric(c()))
  )
  expect_equal(data.frame(a = c(Inf, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_irrational(a),
               data.frame(a = as.numeric(c(Inf)),
                          b = as.numeric(c(1)),
                          c = as.numeric(c(3)))
  )
  expect_equal(data.frame(a = c(NaN, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_irrational(-b),
               data.frame(a = c(NaN, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_irrational(a, c)
  )
  expect_equal(data.frame(a = c(1, 1, 2),
                          b = c(1, Inf, NA),
                          c = c(3, 3, NA)) %>% find_irrational(a:b),
               data.frame(a = c(1),
                          b = c(Inf) %>% as.numeric(),
                          c = c(3) %>% as.numeric())
  )
  
  expect_error(c(1, 2, Inf) %>% find_irrational())
})


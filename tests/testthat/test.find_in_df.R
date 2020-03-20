library(testthat)
library(hablar)
library(dplyr)

context("find_in_df")

test_that("find_duplicates", {
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_duplicates(),
               tibble(a = c(1, 1),
                          b = c(1, 1),
                          c = c(3, 3))
               )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_duplicates(a, b),
               tibble(a = c(1, 1),
                          b = c(1, 1),
                          c = c(3, 3))
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_duplicates(-c),
               tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_duplicates(a, b)
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_duplicates(a:b),
               tibble(a = c(1, 1),
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
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_na(),
               tibble(a = c(2),
                          b = as.numeric(c(NA)),
                          c = as.numeric(c(NA)))
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_na(b),
               tibble(a = c(2),
                          b = as.numeric(c(NA)),
                          c = as.numeric(c(NA)))
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_na(a),
               tibble(a = as.numeric(c()),
                          b = as.numeric(c()),
                          c = as.numeric(c()))
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_na(-b),
               tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_na(a, c)
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_na(a:b),
               tibble(a = c(2),
                          b = c(NA) %>% as.numeric(),
                          c = c(NA) %>% as.numeric())
  )
  
  expect_error(c(1, 2) %>% find_na())
})

test_that("find_nan", {
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NaN)) %>% find_nan(),
               tibble(a = c(2),
                          b = as.numeric(c(NA)),
                          c = as.numeric(c(NaN)))
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NaN)) %>% find_nan(b),
               tibble(a = as.numeric(c()),
                          b = as.numeric(c()),
                          c = as.numeric(c()))
  )
  expect_equal(tibble(a = c(NaN, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_nan(a),
               tibble(a = as.numeric(c(NaN)),
                          b = as.numeric(c(1)),
                          c = as.numeric(c(3)))
  )
  expect_equal(tibble(a = c(NaN, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_nan(-b),
               tibble(a = c(NaN, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_nan(a, c)
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, NaN, NA),
                          c = c(3, 3, NA)) %>% find_nan(a:b),
               tibble(a = c(1),
                          b = c(NaN) %>% as.numeric(),
                          c = c(3) %>% as.numeric())
  )
  
  expect_error(c(1, 2) %>% find_nan())
})

test_that("find_inf", {
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, Inf)) %>% find_inf(),
               tibble(a = c(2),
                          b = as.numeric(c(NA)),
                          c = as.numeric(c(Inf)))
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, Inf)) %>% find_inf(b),
               tibble(a = as.numeric(c()),
                          b = as.numeric(c()),
                          c = as.numeric(c()))
  )
  expect_equal(tibble(a = c(Inf, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_inf(a),
               tibble(a = as.numeric(c(Inf)),
                          b = as.numeric(c(1)),
                          c = as.numeric(c(3)))
  )
  expect_equal(tibble(a = c(Inf, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_inf(-b),
               tibble(a = c(Inf, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_inf(a, c)
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, Inf, NA),
                          c = c(3, 3, NA)) %>% find_inf(a:b),
               tibble(a = c(1),
                          b = c(Inf) %>% as.numeric(),
                          c = c(3) %>% as.numeric())
  )
  
  expect_error(c(1, 2, Inf) %>% find_inf())
})

test_that("find_irrational", {
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, Inf)) %>% find_irrational(),
               tibble(a = c(2),
                          b = as.numeric(c(NA)),
                          c = as.numeric(c(Inf)))
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, Inf)) %>% find_irrational(b),
               tibble(a = as.numeric(c()),
                          b = as.numeric(c()),
                          c = as.numeric(c()))
  )
  expect_equal(tibble(a = c(Inf, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_irrational(a),
               tibble(a = as.numeric(c(Inf)),
                          b = as.numeric(c(1)),
                          c = as.numeric(c(3)))
  )
  expect_equal(tibble(a = c(NaN, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_irrational(-b),
               tibble(a = c(NaN, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% find_irrational(a, c)
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, Inf, NA),
                          c = c(3, 3, NA)) %>% find_irrational(a:b),
               tibble(a = c(1),
                          b = c(Inf) %>% as.numeric(),
                          c = c(3) %>% as.numeric())
  )
  
  expect_error(c(1, 2, Inf) %>% find_irrational())
})


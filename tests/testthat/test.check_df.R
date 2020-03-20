library(testthat)
library(hablar)
library(dplyr)

context("check_df")

test_that("check_duplicates", {
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_duplicates(),
               TRUE
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_duplicates(a, b),
               TRUE
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_duplicates(-c),
               tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_duplicates(a, b)
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_duplicates(a:b),
               TRUE
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_duplicates(),
               TRUE
  )
  expect_equal(tibble(a = c(1, 2, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_duplicates(),
               FALSE
  )
  expect_equal(tibble(a = c(1, 1, 2),
                      b = c(1, 1, NA),
                      c = c(3, 3, NA)) %>% check_duplicates(),
               TRUE
  )
  
  expect_error(c(1, 2) %>% check_duplicates())
})

test_that("check_na", {
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_na(),
               TRUE
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_na(b),
               TRUE
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_na(a),
               FALSE
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_na(-b),
               tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_na(a, c)
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_na(a:b),
               TRUE
  )
  
  expect_error(c(1, 2) %>% check_na())
})

test_that("check_nan", {
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NaN)) %>% check_nan(),
               TRUE
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NaN)) %>% check_nan(b),
               FALSE
  )
  expect_equal(tibble(a = c(NaN, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_nan(a),
               TRUE
  )
  expect_equal(tibble(a = c(NaN, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_nan(-b),
               TRUE
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, NaN, NA),
                          c = c(3, 3, NA)) %>% check_nan(a:b),
               TRUE
  )
  
  expect_error(c(1, 2, NaN) %>% check_nan())
})

test_that("check_inf", {
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, Inf)) %>% check_inf(),
               TRUE
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, Inf)) %>% check_inf(b),
               FALSE
  )
  expect_equal(tibble(a = c(Inf, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_inf(a),
               TRUE
  )
  expect_equal(tibble(a = c(Inf, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_inf(-b),
               TRUE
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, Inf, NA),
                          c = c(3, 3, NA)) %>% check_inf(a:b),
               TRUE
  )
  
  expect_error(c(1, 2, Inf) %>% check_inf())
})

test_that("check_irrational", {
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, Inf)) %>% check_irrational(),
               TRUE
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, Inf)) %>% check_irrational(b),
               FALSE
  )
  expect_equal(tibble(a = c(Inf, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_irrational(a),
               TRUE
  )
  expect_equal(tibble(a = c(NaN, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_irrational(-b),
               tibble(a = c(NaN, 1, 2),
                          b = c(1, 1, NA),
                          c = c(3, 3, NA)) %>% check_irrational(a, c)
  )
  expect_equal(tibble(a = c(1, 1, 2),
                          b = c(1, Inf, NA),
                          c = c(3, 3, NA)) %>% check_irrational(a:b),
               TRUE
  )
  
  expect_error(c(1, 2, Inf) %>% check_irrational())
})


test_that("check_complete_set", {
  expect_equal(tibble(a = c(1, 2),
                          b = c(3, 4)) %>% 
                            check_complete_set(a, b), FALSE
  )
  expect_equal(tibble(a = c(1, 2),
                          b = c(3, 4)) %>% 
                            check_complete_set(a:b), FALSE
  )
  expect_equal(tibble(a = c(1, 2, 1, 2),
                          b = c(3, 4, 4, 3)) %>% 
                            check_complete_set(a, b), TRUE
  )
  expect_error(tibble(a = c(1, 2, 1, 2),
                          b = c(3, 4, 4, 3)) %>% 
                            check_complete_set(a)
  )
})




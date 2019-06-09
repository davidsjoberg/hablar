library(testthat)
library(hablar)

context("ifs")
test_that("if_else_", {
  # No missing argument
  expect_equal(if_else_(c(T, F, NA),
                        as.numeric(c(1, 2, 3)),
                        as.numeric(c(99))),
               as.numeric(c(1, 99, NA))
  )
  expect_equal(if_else_(c(T, F, NA),
                        as.numeric(c(1, 2, 3)),
                        as.numeric(c(4, 5, 6))),
               as.numeric(c(1, 5, NA))
  )
  expect_equal(if_else_(c(T, F, NA),
                        as.numeric(c(1, 2, 3)),
                        NA),
               as.numeric(c(1, NA, NA))
  )
  expect_equal(if_else_(c(T, F, NA),
                        NA,
                        "hey"),
               as.character(c(NA, "hey", NA))
  )
  expect_equal(if_else_(c(T, F, NA),
                        NA,
                        c(1L, 2L, 3L)),
               as.integer(c(NA, 2L, NA))
  )
  expect_equal(if_else_(c(T, F, NA),
                        NA,
                        as.factor(c("A", "B", "C"))),
               factor(c(NA, "B", NA), levels = c("A", "B", "C"))
               )
  expect_equal(if_else_(c(T, F, NA),
                        as.factor(c("A", "B", "C")),
                        NA),
               factor(c("A", NA, NA), levels = c("A", "B", "C"))
  )
  expect_equal(if_else_(c(T, F, NA),
                        as.Date(c("2019-01-01", "2019-01-01", "2019-01-01")),
                        as.Date("2018-01-01")),
               as.Date(c("2019-01-01", "2018-01-01", NA))
  )
  expect_equal(if_else_(c(T, F, NA),
                        as.Date(c("2019-01-01", "2019-01-01", "2019-01-01")),
                        NA),
               as.Date(c("2019-01-01", NA, NA)) 
  )

  expect_error(if_else_(c(T, F, NA),
                        1,
                        1L))
  expect_warning(if_else_(c(T, F, NA),
                        as.factor(1),
                        factor(1, levels = c("1", "2"))))
})

test_that("replacers if_*", {
  # No missing argument
  expect_equal(if_na(c(1, NA, 3), 99), c(1, 99, 3))
  expect_equal(if_nan(c(1, NaN, NA), 99), c(1, 99, NA))
  expect_equal(if_inf(c(1, Inf, -Inf, NA), 99), c(1, 99, 99, NA))
  expect_equal(if_zero(c(1, 0, 0, NA), 99), c(1, 99, 99, NA))

  expect_equal(if_na(c(1, NA, 3), NA), c(1, NA, 3))
  expect_equal(if_nan(c(1, NaN, NA), NA), c(1, NA, NA))
  expect_equal(if_inf(c(1, Inf, -Inf, NA), NA), c(1, NA, NA, NA))
  expect_equal(if_zero(c(1, 0, 0, NA), NA), c(1, NA, NA, NA))

  expect_equal(if_nan(c(1, NaN, NA), 99), c(1, 99, NA))
  expect_equal(if_inf(c(1, Inf, -Inf, NA), 99), c(1, 99, 99, NA))
  expect_equal(if_zero(c(1, 0, 0, NA), 99), c(1, 99, 99, NA))
  
  expect_equal(if_zero(c(1, 0, 0, NA), 99, 9999), c(1, 99, 99, 9999))

  expect_warning(if_na(as.factor(c(1, NA, 3)), 
                     as.factor(99)))
  expect_error(if_na(c(1, 2, NA), "hej"))
})

test_that("replacers *_if", {
  # No missing argument
  expect_equal(na_if(c(1, 2, 3), c(1, 2, 3) == 2), c(1, NA, 3))
  expect_equal(nan_if(c(1, 2, 3), c(1, 2, 3) == 2), c(1, NaN, 3))
  expect_equal(inf_if(c(1, 2, 3), c(1, 2, 3) == 2), c(1, Inf, 3))
  expect_equal(zero_if(c(1, 2, 3), c(1, 2, 3) == 2), c(1, 0, 3))
  
  expect_equal(na_if(c(1, 2, NA), c(1, 2, NA) == 2, replace_na = TRUE), c(1, NA, NA))

  expect_error(na_if(c(1, 2, NA),c(1, 2, NA)))
})




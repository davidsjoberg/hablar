library(testthat)
library(hablar)

###################################################
# retype
###################################################

context("retype")
test_that("vectors", {
  expect_equal(retype(as.numeric(c(3.56))), as.numeric(c(3.56)))
  expect_equal(retype(as.numeric(c(3))), as.integer(c(3)))
  expect_equal(retype(as.numeric(c(NA, NA))), as.numeric(c(NA, NA)))

  expect_equal(retype(as.logical(c(T))), as.integer(c(1)))
  expect_equal(retype(as.logical(c(NA, F))), as.integer(c(NA, 0)))

  expect_equal(retype(as.integer(c(NA, NA))), as.integer(c(NA, NA)))
  expect_equal(retype(as.integer(c(1, NA, 3))), as.integer(c(1, NA, 3)))

  expect_equal(retype(as.character(c("a"))), as.character(c("a")))
  expect_equal(retype(as.character(c(NA, NA))), as.character(c(NA, NA)))
  expect_equal(retype(as.character(c("1", "2", NA))), as.integer(c(1, 2, NA)))
  expect_equal(retype(as.character(c("1", "2,0", NA))), as.integer(c(1, 2, NA)))
  expect_equal(retype(as.character(c("1", "2.5", NA))), as.numeric(c(1, 2.5, NA)))
  expect_equal(retype(as.character(c("1", "2,5", NA))), as.numeric(c(1, 2.5, NA)))
  expect_equal(retype(as.character(c("2018-01-25", "2018-01-27", NA))), as.Date(c("2018-01-25", "2018-01-27", NA)))
  expect_equal(retype(as.character(c("2018-10-10 11:30:20 CEST", "2018-10-10 11:30:20 CEST", NA))), as.Date(c("2018-10-10", "2018-10-10", NA)))
  expect_equal(retype(as.character(c("2018-10-10 11:30:20 CEST", "2018-10-10 11:30:25 CEST", NA))), as.POSIXct(c("2018-10-10 11:30:20 CEST", "2018-10-10 11:30:25 CEST", NA)))

  expect_equal(retype(as.factor(c("a"))), as.character(c("a")))
  expect_equal(retype(as.factor(c(NA, NA))), as.character(c(NA, NA)))
  expect_equal(retype(as.factor(c("3", "2", NA))), as.integer(c(3, 2, NA)))
  expect_equal(retype(as.factor(c("1", "7,0", NA))), as.integer(c(1, 7, NA)))
  expect_equal(retype(as.factor(c("1", "2.5", NA))), as.numeric(c(1, 2.5, NA)))
  expect_equal(retype(as.factor(c("1", "2,5", NA))), as.numeric(c(1, 2.5, NA)))
  expect_equal(retype(as.factor(c("2018-01-25", "2018-01-27", NA))), as.Date(c("2018-01-25", "2018-01-27", NA)))
  expect_equal(retype(as.factor(c("2018-10-10 11:30:20 CEST", "2018-10-10 11:30:20 CEST", NA))), as.Date(c("2018-10-10", "2018-10-10", NA)))
  expect_equal(retype(as.factor(c("2018-10-10 11:30:20 CEST", "2018-10-10 11:30:25 CEST", NA))), as.POSIXct(c("2018-10-10 11:30:20 CEST", "2018-10-10 11:30:25 CEST", NA)))

  expect_equal(retype(list("a", "b")), list("a", "b"))
})



test_that("data.frame", {
  df <- tibble(a = as.numeric(c(1, Inf, 3, 2, 4)),
               b = as.numeric(c(1, 7, 3, 2, 4)),
               c = as.numeric(c(1, NA, 3, 2, 4)),
               d = as.numeric(c(Inf, Inf, Inf, Inf, Inf)),
               e = as.numeric(c(3.5, 3.7, NaN, Inf, Inf)),

               f = as.integer(c(1, 2, 3, NA, 5)),

               g = as.numeric(c(NA, NA, NA, NA, NA)),
               h = as.integer(c(NA, NA, NA, NA, NA)),
               i = as.Date(c(NA, NA, NA, NA, NA)),
               j = as.POSIXct(c(NA, NA, NA, NA, NA)),
               k = as.factor(c(NA, NA, NA, NA, NA)),

               l = as.factor(c("2018-02-01", "2018-04-15", NA, NA, "2016-05-15")),
               m = as.factor(c(1, 2, 3, 4, 5)),
               n = as.factor(c("a", "b", NA, "q", NA)),

               o = as.character(c("2018-02-01", "2018-04-15", NA, NA, "2016-05-15")),
               p = as.character(c("2018-02-01 01:00", "2018-04-15 01:00", NA, NA, "2016-05-15 01:00")),
               q = as.character(c("2018-02-01 01:00", "2018-04-15 16:56", NA, NA, "2016-05-15 17:10")),
               r = as.character(c("2018-02-01 01:00 CEST", "2018-04-15 16:56 CEST", NA, NA, "2016-05-15 17:10 CEST")),
               s = as.character(c("2018-02-01 01:00 CEST", "2018-04-15 16:56 CEST", NA, NA, "2016-05-15 17:10 CEST")),

               t = as.POSIXct(c("2018-02-01 01:00", "2018-04-15 01:00", NA, NA, "2016-05-15 01:00")),
               u = as.POSIXct(c("2018-02-01 01:00 CEST", "2018-04-15 16:56 CEST", NA, NA, "2016-05-15 17:10 CEST")),

               v = as.logical(c(T, T, NA, F, T)),
               w = as.character(c("1", "1,78", NA, "NaN", "1")))

  expect_equal(retype(df), tibble(a = as.numeric(c(1, Inf, 3, 2, 4)),
                                      b = as.integer(c(1, 7, 3, 2, 4)),
                                      c = as.integer(c(1, NA, 3, 2, 4)),
                                      d = as.numeric(c(Inf, Inf, Inf, Inf, Inf)),
                                      e = as.numeric(c(3.5, 3.7, NaN, Inf, Inf)),

                                      f = as.integer(c(1, 2, 3, NA, 5)),

                                      g = as.numeric(c(NA, NA, NA, NA, NA)),
                                      h = as.integer(c(NA, NA, NA, NA, NA)),
                                      i = as.Date(c(NA, NA, NA, NA, NA)),
                                      j = as.POSIXct(c(NA, NA, NA, NA, NA)),
                                      k = as.character(c(NA, NA, NA, NA, NA)),

                                      l = as.Date(c("2018-02-01", "2018-04-15", NA, NA, "2016-05-15")),
                                      m = as.integer(c(1, 2, 3, 4, 5)),
                                      n = as.character(c("a", "b", NA, "q", NA)),

                                      o = as.Date(c("2018-02-01", "2018-04-15", NA, NA, "2016-05-15")),
                                      p = as.Date(c("2018-02-01 01:00", "2018-04-15 01:00", NA, NA, "2016-05-15 01:00")),
                                      q = as.POSIXct(c("2018-02-01 01:00", "2018-04-15 16:56", NA, NA, "2016-05-15 17:10")),
                                      r = as.POSIXct(c("2018-02-01 01:00 CEST", "2018-04-15 16:56 CEST", NA, NA, "2016-05-15 17:10 CEST")),
                                      s = as.POSIXct(c("2018-02-01 01:00 CEST", "2018-04-15 16:56 CEST", NA, NA, "2016-05-15 17:10 CEST")),

                                      t = as.Date(c("2018-02-01 01:00", "2018-04-15 01:00", NA, NA, "2016-05-15 01:00")),
                                      u = as.POSIXct(c("2018-02-01 01:00 CEST", "2018-04-15 16:56 CEST", NA, NA, "2016-05-15 17:10 CEST")),

                                      v = as.integer(c(1, 1, NA, 0, 1)),
                                      w = as.numeric(c(1, 1.78, NA, NaN, 1))))

  expect_equal(retype(df, p), df %>% mutate_at(vars(p), funs(retype)))
  expect_equal(retype(df, p), df %>% mutate_at(vars(p), funs(as.Date)))
  expect_equal(retype(df, -t), tibble(a = as.numeric(c(1, Inf, 3, 2, 4)),
                                          b = as.integer(c(1, 7, 3, 2, 4)),
                                          c = as.integer(c(1, NA, 3, 2, 4)),
                                          d = as.numeric(c(Inf, Inf, Inf, Inf, Inf)),
                                          e = as.numeric(c(3.5, 3.7, NaN, Inf, Inf)),

                                          f = as.integer(c(1, 2, 3, NA, 5)),

                                          g = as.numeric(c(NA, NA, NA, NA, NA)),
                                          h = as.integer(c(NA, NA, NA, NA, NA)),
                                          i = as.Date(c(NA, NA, NA, NA, NA)),
                                          j = as.POSIXct(c(NA, NA, NA, NA, NA)),
                                          k = as.character(c(NA, NA, NA, NA, NA)),

                                          l = as.Date(c("2018-02-01", "2018-04-15", NA, NA, "2016-05-15")),
                                          m = as.integer(c(1, 2, 3, 4, 5)),
                                          n = as.character(c("a", "b", NA, "q", NA)),

                                          o = as.Date(c("2018-02-01", "2018-04-15", NA, NA, "2016-05-15")),
                                          p = as.Date(c("2018-02-01 01:00", "2018-04-15 01:00", NA, NA, "2016-05-15 01:00")),
                                          q = as.POSIXct(c("2018-02-01 01:00", "2018-04-15 16:56", NA, NA, "2016-05-15 17:10")),
                                          r = as.POSIXct(c("2018-02-01 01:00 CEST", "2018-04-15 16:56 CEST", NA, NA, "2016-05-15 17:10 CEST")),
                                          s = as.POSIXct(c("2018-02-01 01:00 CEST", "2018-04-15 16:56 CEST", NA, NA, "2016-05-15 17:10 CEST")),

                                          t = as.POSIXct(c("2018-02-01 01:00", "2018-04-15 01:00", NA, NA, "2016-05-15 01:00")),
                                          u = as.POSIXct(c("2018-02-01 01:00 CEST", "2018-04-15 16:56 CEST", NA, NA, "2016-05-15 17:10 CEST")),

                                          v = as.integer(c(1, 1, NA, 0, 1)),
                                          w = as.numeric(c(1, 1.78, NA, NaN, 1))))
})

library(testthat)
library(hablar)
# Keep order of hablar.R for tests


###################################################
# could_this_be_that
###################################################

context("could this be that")
test_that("could_chr_be_num", {
  expect_equal(could_chr_be_num("a"), F)
  expect_equal(could_chr_be_num("."), F)
  expect_equal(could_chr_be_num(" 3"), T)
  expect_equal(could_chr_be_num("3 0"), F)
  expect_equal(could_chr_be_num("2018-03-01"), F)
  expect_equal(could_chr_be_num("2018-10-09 19:19:26 CEST"), F)
  expect_equal(could_chr_be_num("1"), T)
  expect_equal(could_chr_be_num(".56"), T)
  expect_equal(could_chr_be_num("7.0"), T)
  expect_equal(could_chr_be_num("0003"), T)
  expect_equal(could_chr_be_num("1,98"), T)
  expect_equal(could_chr_be_num(as.character(c(NA, NA))), F)
  expect_equal(could_chr_be_num(",98"), T)
  expect_equal(could_chr_be_num(as.character(NA)), F)

  expect_error(could_chr_be_num(as.numeric(1)))
  expect_error(could_chr_be_num())
  expect_error(could_chr_be_num(c()))
  expect_error(could_chr_be_num(data.frame(a = c(1,3,4))))
  expect_error(could_chr_be_num(list(a = c(1,3,4))))
})

test_that("could_chr_be_int", {
  expect_equal(could_chr_be_int("a"), F)
  expect_equal(could_chr_be_int("."), F)
  expect_equal(could_chr_be_int(" 3"), T)
  expect_equal(could_chr_be_int("3 0"), F)
  expect_equal(could_chr_be_int("2018-03-01"), F)
  expect_equal(could_chr_be_int("2018-10-09 19:19:26 CEST"), F)
  expect_equal(could_chr_be_int("1"), T)
  expect_equal(could_chr_be_int(".56"), F)
  expect_equal(could_chr_be_int("7.0"), T)
  expect_equal(could_chr_be_int("0003"), T)
  expect_equal(could_chr_be_int("1,98"), F)
  expect_equal(could_chr_be_int(as.character(c())), F)
  expect_equal(could_chr_be_int(as.character(c(NA, NA))), F)
  expect_equal(could_chr_be_int(",98"), F)
  expect_equal(could_chr_be_int(as.character(NA)), F)

  expect_error(could_chr_be_int(as.numeric(1)))
  expect_error(could_chr_be_int())
  expect_error(could_chr_be_int(data.frame(a = c(1,3,4))))
  expect_error(could_chr_be_num(list(a = c(1,3,4))))
})

test_that("could_num_be_int", {
  expect_equal(could_num_be_int(as.numeric(1)), T)
  expect_equal(could_num_be_int(c(1, 2)), T)
  expect_equal(could_num_be_int(c(1, 2.6)), F)
  expect_equal(could_num_be_int(as.numeric(NA)), F)
  expect_equal(could_num_be_int(as.numeric(c())), F)

  expect_error(could_num_be_int(as.character(c())))
  expect_error(could_num_be_int())
  expect_error(could_num_be_int("a"))
  expect_error(could_num_be_int(",98"))
  expect_error(could_num_be_int(as.character(NA)))
  expect_error(could_num_be_int(data.frame(a = c(1,3,4))))
  expect_error(could_chr_be_num(list(a = c(1,3,4))))
})

test_that("could_chr_be_dtm", {
  expect_equal(could_chr_be_dtm("a"), F)
  expect_equal(could_chr_be_dtm("."), F)
  expect_equal(could_chr_be_dtm(" 3"), F)
  expect_equal(could_chr_be_dtm("3 0"), F)
  expect_equal(could_chr_be_dtm("2018-03-01"), T)
  expect_equal(could_chr_be_dtm("2018-10-09 19:19:26 CEST"), T)
  expect_equal(could_chr_be_dtm(as.character(c("2018-10-09 19:19:26 CEST", "2018-10-09 19:19:27 CEST", NA))), T)
  expect_equal(could_chr_be_dtm("1"), F)
  expect_equal(could_chr_be_dtm("7.0"), F)
  expect_equal(could_chr_be_dtm("0003"), F)
  expect_equal(could_chr_be_dtm(as.character(NA)), F)

  expect_error(could_chr_be_dtm(as.POSIXct(c("2018-10-09 19:19:26 CEST", "2018-10-09 19:19:27 CEST"))))
  expect_error(could_chr_be_dtm(as.Date(c(NA, NA))))
  expect_error(could_chr_be_dtm(as.numeric(1)))
  expect_error(could_chr_be_dtm(as.factor("2018-03-01")))
  expect_error(could_chr_be_dtm(data.frame(a = c(1,3,4))))
  expect_error(could_chr_be_num(list(a = c(1,3,4))))
})

test_that("could_dtm_be_dte", {
  expect_equal(could_dtm_be_dte(as.POSIXct("2018-03-01")), T)
  expect_equal(could_dtm_be_dte(as.POSIXct(c("2018-03-01", "2018-03-03"))), T)
  expect_equal(could_dtm_be_dte(as.POSIXct(c("2018-10-09 19:19:26 CEST", "2018-10-09 19:19:27 CEST"))), F)
  expect_equal(could_dtm_be_dte(as.POSIXct(c("2018-10-09 19:19:26 CEST", "2018-10-09 19:19:27 CEST", NA))), F)
  expect_equal(could_dtm_be_dte(as.POSIXct(NA)), F)

  expect_error(could_dtm_be_dte("a"))
  expect_error(could_dtm_be_dte(as.Date(c(NA, NA))))
  expect_error(could_dtm_be_dte(as.numeric(1)))
  expect_error(could_dtm_be_dte(as.factor("2018-03-01")))
  expect_error(could_dtm_be_dte(data.frame(a = c(1,3,4))))
  expect_error(could_chr_be_num(list(a = c(1,3,4))))
})


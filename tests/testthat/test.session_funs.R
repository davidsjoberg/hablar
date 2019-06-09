library(testthat)
library(hablar)

context("session_fun")
test_that("set_path*", {
  expect_error(set_wd_to_script_path())
})


library(testthat)
library(hablar)


###################################################
# convert
###################################################

context("convert")
test_that("from numeric", {
  expect_equal(tibble(a = as.numeric(c(1, 2))) %>%
                 convert(int(a)),
               tibble(a = as.integer(c(1, 2))))

  expect_equal(tibble(a = as.numeric(c(1, 2))) %>%
                 convert(num(a)),
               tibble(a = as.numeric(c(1, 2))))

  expect_equal(tibble(a = as.numeric(c(1, 2))) %>%
                 convert(dbl(a)),
               tibble(a = as.numeric(c(1, 2))))

  expect_equal(tibble(a = as.numeric(c(1, 2))) %>%
                 convert(chr(a)),
               tibble(a = as.character(c("1", "2"))))

  expect_equal(tibble(a = as.numeric(c(1, 2))) %>%
                 convert(dte(a)),
               tibble(a = as.Date(c("1970-01-02", "1970-01-03"))))

  expect_equal(tibble(a = as.numeric(c(1, 2))) %>%
                 convert(dtm(a)) %>% lapply(class),
               tibble(a = as.POSIXct(c("1970-01-01 01:00:01", "1970-01-01 01:00:02"))) %>% lapply(class))

  expect_equal(tibble(a = as.numeric(c(1, 2))) %>%
                 convert(lgl(a)),
               tibble(a = as.logical(c(T, T))))

  expect_equal(tibble(a = as.numeric(c(1, 2))) %>%
                 convert(fct(a)),
               tibble(a = as.factor(c(1, 2))))
  })

test_that("from integer", {
  expect_equal(tibble(a = as.integer(c(1, 2))) %>%
                 convert(num(a)),
               tibble(a = as.numeric(c(1, 2))))

  expect_equal(tibble(a = as.integer(c(1, 2))) %>%
                 convert(dbl(a)),
               tibble(a = as.numeric(c(1, 2))))

  expect_equal(tibble(a = as.integer(c(1, 2))) %>%
                 convert(int(a)),
               tibble(a = as.integer(c(1, 2))))

  expect_equal(tibble(a = as.integer(c(1, 2))) %>%
                 convert(chr(a)),
               tibble(a = as.character(c("1", "2"))))

  expect_equal(tibble(a = as.integer(c(1, 2))) %>%
                 convert(dte(a)),
               tibble(a = as.Date(c("1970-01-02", "1970-01-03"))))

  expect_equal(tibble(a = as.integer(c(1, 2))) %>%
                 convert(dtm(a)) %>% lapply(class),
               tibble(a = as.POSIXct(c("1970-01-01 01:00:01", "1970-01-01 01:00:02"), tz="Europe/London")) %>% lapply(class))

  expect_equal(tibble(a = as.integer(c(1, 2))) %>%
                 convert(lgl(a)),
               tibble(a = as.logical(c(T, T))))

  expect_equal(tibble(a = as.integer(c(1, 2))) %>%
                 convert(fct(a)),
               tibble(a = as.factor(c(1, 2))))
})

test_that("from logical", {
  expect_equal(tibble(a = as.logical(c(T, F, NA))) %>%
                 convert(num(a)),
               tibble(a = as.numeric(c(1, 0, NA))))

  expect_equal(tibble(a = as.logical(c(T, F, NA))) %>%
                 convert(dbl(a)),
               tibble(a = as.numeric(c(1, 0, NA))))

  expect_equal(tibble(a = as.logical(c(T, F, NA))) %>%
                 convert(chr(a)),
               tibble(a = as.character(c("TRUE", "FALSE", NA))))

  expect_error(tibble(a = as.logical(c(T, F, NA))) %>%
                 convert(dte(a)))

  expect_error(tibble(a = as.logical(c(T, F, NA))) %>%
                 convert(dtm(a)))

  expect_equal(tibble(a = as.logical(c(T, F, NA))) %>%
                 convert(lgl(a)),
               tibble(a = as.logical(c(T, F, NA))))

  expect_equal(tibble(a = as.logical(c(T, F, NA))) %>%
                 convert(fct(a)),
               tibble(a = as.factor(c(T, F, NA))))
})

test_that("from Date", {
  expect_equal(tibble(a = as.Date(c("1970-05-03", NA, "1987-04-20"))) %>%
                 convert(num(a)),
               tibble(a = as.numeric(c(122, NA, 6318))))

  expect_equal(tibble(a = as.Date(c("1970-05-03", NA, "1987-04-20"))) %>%
                 convert(dbl(a)),
               tibble(a = as.numeric(c(122, NA, 6318))))

  expect_equal(tibble(a = as.Date(c("1970-05-03", NA, "1987-04-20"))) %>%
                 convert(chr(a)),
               tibble(a = as.character(c("1970-05-03", NA, "1987-04-20"))))

  expect_equal(tibble(a = as.Date(c("1970-05-03", NA, "1987-04-20"))) %>%
                 convert(dte(a)),
               tibble(a = as.Date(c("1970-05-03", NA, "1987-04-20"))))

  expect_equal(tibble(a = as.Date(c("1970-05-03", NA, "1987-04-20"))) %>%
                 convert(dtm(a)),
               tibble(a = as.POSIXct(as.Date(c("1970-05-03", NA, "1987-04-20")))))

  expect_equal(tibble(a = as.Date(c("1970-05-03", NA, "1987-04-20"))) %>%
                 convert(dte(a)),
               tibble(a = as.Date(c("1970-05-03", NA, "1987-04-20"))))

  expect_equal(tibble(a = as.Date(c("1970-05-03", NA, "1987-04-20"))) %>%
                 convert(fct(a)),
               tibble(a = as.factor(c("1970-05-03", NA, "1987-04-20"))))
})


test_that("from POSIXct", {
  expect_equal(tibble(a = as.POSIXct(c("1970-05-03 01:00", NA, "1987-04-20 03:45"), tz = "UTC")) %>%
                 convert(num(a)),
               tibble(a = as.numeric(c(10544400, NA, 545888700))))

  expect_equal(tibble(a = as.POSIXct(c("1970-05-03 01:00", NA, "1987-04-20 03:45"), tz = "UTC")) %>%
                 convert(dbl(a)),
               tibble(a = as.numeric(c(10544400, NA, 545888700))))

  expect_equal(tibble(a = as.POSIXct(c("1970-05-03 01:00", NA, "1987-04-20 03:45"))) %>%
                 convert(chr(a)),
               tibble(a = as.character(c("1970-05-03 01:00:00", NA, "1987-04-20 03:45:00"))))

  expect_equal(tibble(a = as.POSIXct(c("1970-05-03 01:00", NA, "1987-04-20 03:45"))) %>%
                 convert(dte(a)),
               tibble(a = as.Date(c("1970-05-03", NA, "1987-04-20"))))

  expect_equal(tibble(a = as.POSIXct(c("1970-05-03 01:00", NA, "1987-04-20 03:45"))) %>%
                 convert(dtm(a)),
               tibble(a = as.POSIXct(c("1970-05-03 01:00", NA, "1987-04-20 03:45"))))

  expect_equal(tibble(a = as.POSIXct(c("1970-05-03 01:00", NA, "1987-04-20 03:45"))) %>%
                 convert(dte(a)),
               tibble(a = as.Date(c("1970-05-03", NA, "1987-04-20"))))

  expect_equal(tibble(a = as.POSIXct(c("1970-05-03 01:00", NA, "1987-04-20 03:45"))) %>%
                 convert(fct(a)),
               tibble(a = as.factor(c("1970-05-03 01:00:00", NA, "1987-04-20 03:45:00"))))
})


test_that("from factor", {
  expect_warning(tibble(a = as.factor(c("1970-05-03", NA, "1"))) %>%
                 convert(num(a)))

  expect_warning(tibble(a = as.factor(c("1970-05-03", NA, "1"))) %>%
                 convert(dbl(a)))

  expect_equal(tibble(a = as.factor(c("1970-05-03", NA, "1"))) %>%
                 convert(chr(a)),
               tibble(a = as.character(c("1970-05-03", NA, "1"))))

  expect_equal(tibble(a = as.factor(c("1970-05-03", NA, "1"))) %>%
                 convert(dte(a)),
               tibble(a = as.Date(c("1970-05-03", NA, NA))))

  expect_error(tibble(a = as.factor(c("1970-05-03", NA, "1"))) %>%
                 convert(dtm(a)))

  expect_equal(tibble(a = as.factor(c("1970-05-03", NA, "1"))) %>%
                 convert(dte(a)),
               tibble(a = as.Date(c("1970-05-03", NA, NA))))

  expect_equal(tibble(a = as.factor(c("1970-05-03", NA, "1"))) %>%
                 convert(fct(a)),
               tibble(a = as.factor(c("1970-05-03", NA, "1")))) 
})

test_that("Other tests", {
  expect_equal(dplyr::starwars %>% select(1:6) %>% convert(lgl(height), int(mass)),
               dplyr::starwars %>% select(1:6) %>%
                 mutate_at(vars(height), funs(as.logical)) %>%
                 mutate_at(vars(mass), funs(as.integer)))

  expect_error(convert(as.numeric(1)))
  })

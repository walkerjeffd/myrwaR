library(myrwaR)
context("Timeseries utilities")

test_that("is.regular.hourly works with a zoo object", {
  x <- zoo::zoo(x = runif(n = 48),
                order.by = seq.POSIXt(lubridate::ymd_hm("2000-01-01 00:00"),
                                      lubridate::ymd_hm("2000-01-02 23:00"),
                                      by="hour"))
  expect_true(is.regular_hourly(x))
  expect_true(is.regular_hourly(x[1:2]))
  expect_false(is.regular_hourly(x[-5]))
  expect_message(is.regular_hourly(x[-5]), 'Time series is not regular, check timeseries around')
})

test_that("is.regular.hourly works with a vector of POSIXct", {
  x <- seq.POSIXt(lubridate::ymd_hm("2000-01-01 00:00"),
                  lubridate::ymd_hm("2000-01-02 23:00"),
                  by="hour")

  expect_true(is.regular_hourly(x))
  expect_true(is.regular_hourly(x[1:2]))
  expect_false(is.regular_hourly(x[-5]))
  expect_message(is.regular_hourly(x[-5]), 'Time series is not regular, check timeseries around')
})

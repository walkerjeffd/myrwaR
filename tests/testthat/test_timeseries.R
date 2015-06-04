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
  expect_message(is.regular_hourly(x[-5]), "Timeseries is not regular, check timesteps around")
})

test_that("is.regular.hourly works with a vector of POSIXct", {
  x <- seq.POSIXt(lubridate::ymd_hm("2000-01-01 00:00"),
                  lubridate::ymd_hm("2000-01-02 23:00"),
                  by="hour")

  expect_true(is.regular_hourly(x))
  expect_true(is.regular_hourly(x[1:2]))
  expect_false(is.regular_hourly(x[-5]))
  expect_message(is.regular_hourly(x[-5]), "Timeseries is not regular, check timesteps around")
})

test_that("water_year works with POSIXct", {
  expect_equal(water_year(lubridate::ymd(c("2000-09-30", "2000-10-01", "2000-10-01"))),
               c(2000, 2001, 2001))
})

test_that("water_year works with Date", {
  expect_equal(water_year(as.Date(c("2000-09-30", "2000-10-01", "2000-10-01"))),
               c(2000, 2001, 2001))
})

test_that("water_year works with January as start month", {
  expect_equal(water_year(as.Date(c("2000-09-30", "2000-10-01", "2000-10-01")),
                          start_month = 1),
               c(2000, 2000, 2000))
})

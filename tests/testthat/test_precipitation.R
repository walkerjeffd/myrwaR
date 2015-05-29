library(myrwaR)
context("Precipitation tools")

test_that("antecedent_precip() works with data frame", {
  prcp <- data.frame(Datetime=seq.POSIXt(lubridate::ymd_hm("2000-01-01 00:00"),
                                         lubridate::ymd_hm("2000-01-02 23:00"),
                                         by = "hour"),
                     Precip=runif(n=48))
  prcp.24.0 <- antecedent_precip(prcp, period=24, delay=0, fun=sum)
  expect_equal(nrow(prcp), length(prcp.24.0))
  expect_true(all(is.na(prcp.24.0[1:23])))
  expect_true(all(!is.na(prcp.24.0[24:length(prcp.24.0)])))
  expect_equal(sum(prcp$Precip[1:24]), prcp.24.0[24])

  prcp.24.1 <- antecedent_precip(prcp, period=24, delay=1, fun=sum)
  expect_equal(nrow(prcp), length(prcp.24.1))
  expect_true(all(is.na(prcp.24.1[1:24])))
  expect_true(all(!is.na(prcp.24.0[25:length(prcp.24.0)])))
  expect_equal(sum(prcp$Precip[1:24]), prcp.24.1[25])
})

test_that("antecedent_precip() returns original values with 1 hr period and 0 delay", {
  prcp <- data.frame(Datetime=seq.POSIXt(lubridate::ymd_hm("2000-01-01 00:00"),
                                         lubridate::ymd_hm("2000-01-02 23:00"),
                                         by = "hour"),
                     Precip=runif(n=48))
  prcp.1.0 <- antecedent_precip(prcp, period=1, delay=0, fun=sum)
  expect_equal(prcp$Precip, prcp.1.0)
})

test_that("antecedent_precip() works with zoo", {
  prcp <- zoo::zoo(runif(n=48),
                   seq.POSIXt(lubridate::ymd_hm("2000-01-01 00:00"),
                              lubridate::ymd_hm("2000-01-02 23:00"),
                              by = "hour"))

  prcp.24.0 <- antecedent_precip(prcp, period=24, delay=0, fun=sum)
  expect_equal(length(prcp), length(prcp.24.0))
  expect_true(all(is.na(prcp.24.0[1:23])))
  expect_true(all(!is.na(prcp.24.0[24:length(prcp.24.0)])))
  expect_equal(sum(prcp[1:24]), prcp.24.0[24])

  prcp.24.1 <- antecedent_precip(prcp, period=24, delay=1, fun=sum)
  expect_equal(length(prcp), length(prcp.24.1))
  expect_true(all(is.na(prcp.24.1[1:24])))
  expect_true(all(!is.na(prcp.24.0[25:length(prcp.24.0)])))
  expect_equal(sum(prcp[1:24]), prcp.24.1[25])
})

test_that("load_precip_from_xls() loads excel file", {
  x <- load_precip_from_xls(system.file("extdata", "LoganPrecip.xlsx",
                                        package = "myrwaR"))
  expect_true(inherits(x, 'data.frame'))
  expect_identical(names(x), c("Datetime", "Precip"))
  expect_equal(lubridate::tz(x$Datetime), "EST")

  z <- load_precip_from_xls(system.file("extdata", "LoganPrecip.xlsx",
                                        package = "myrwaR"),
                            as.type = "zoo")
  expect_true(inherits(z, 'zoo'))
  expect_equal(nrow(x), length(z))
  expect_equal(x$Precip, zoo::coredata(z))
})

test_that("load_precip_from_xls() assigns correct timezone", {
  x <- load_precip_from_xls(system.file("extdata", "LoganPrecip.xlsx",
                                        package = "myrwaR"),
                            tz = "UTC")
  expect_equal(lubridate::tz(x$Datetime), "UTC")
})

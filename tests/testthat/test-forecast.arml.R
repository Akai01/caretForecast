# A unit test for forecast function
if(require(testthat)){
  test_that("tests for some arguments in forecast 1", {
    forecast(ARml(AirPassengers, caret_method = "lm", max_lag = 12), h = 2) -> fc
    values <- round(c(fc$mean))
    expect_that(values, equals(c(453, 435)))

  })
}

if(require(testthat)){
  test_that("tests for some arguments in forecast 2", {
    ARml(AirPassengers, caret_method = "lm", max_lag = 10,
         xreg = forecast::seasonaldummy(AirPassengers), seasonal = F) -> fit
    forecast(fit, h = 2, xreg = forecast::seasonaldummy(AirPassengers, h = 2)) -> fc
    values <- round(c(fc$mean))
    expect_that(values, equals(c(447, 430)))

  })
}

if(require(testthat)){
  test_that("tests for some arguments in forecast 3", {
    forecast(ARml(AirPassengers, caret_method = "lm", max_lag = 12,
                  seasonal = F), h = 2) -> fc
    values <- round(c(fc$mean))
    expect_that(values, equals(c(461, 433)))

  })
}

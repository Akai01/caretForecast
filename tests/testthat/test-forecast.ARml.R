# A unit test for forecast function
if(require(testthat)){

  test_that("tests for some arguments in forecast", {
    library(forecast)
    forecast(ARml(AirPassengers, caret_method = "lm", maxlag = 12), h = 12) -> fc
    class_fc <- class(fc)
    expect_that(class_fc, equals(c("forecast", "forecastARml")))

  })
}



if(require(testthat)){

  test_that("tests for some arguments in forecast", {
    library(forecast)
    ARml(AirPassengers, caret_method = "lm", maxlag = 10,
         xreg = forecast::seasonaldummy(AirPassengers), seasonal = F) -> fit
    forecast(fit, h = 12,
             xreg = forecast::seasonaldummy(AirPassengers, h = 12)) -> fc
    class_fc <- class(fc)
    expect_that(class_fc, equals(c("forecast", "forecastARml")))

  })
}

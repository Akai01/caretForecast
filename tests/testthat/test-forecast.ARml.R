# A unit test for forecast function
if(require(testthat)){

  test_that("tests for some arguments in forecast", {
    library(forecast)
    forecast(ARml(AirPassengers, caret_method = "lm", maxlag = 12), h = 12) -> fc
    class_fc <- class(fc)
    expect_that(class_fc, equals(c("forecast", "forecastARml")))

  })
}

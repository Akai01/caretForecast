
# A unit test for lag_maker_forecast function
if(require(testthat)){

  test_that("tests for some arguments in lag_maker_forecast", {
    result <- lag_maker_forecast(as.matrix(AirPassengers), maxlag = 1)

    expect_that(class(result), equals(c("matrix", "array" )))

  })
}

# A unit test for forecast function
if(require(testthat)){

  test_that("tests for some arguments in forecast 1", {

    forecast(ARml(AirPassengers, caret_method = "lm", max_lag = 12, K=5),
             h = 2) -> fc
    values <- as.numeric(ceiling(c(fc$mean)))
    expect_equal(values, c(459, 429), tolerance = 1)

  })
}



if(require(testthat)){

  test_that("tests for some arguments in forecast 2", {

    ARml(AirPassengers, caret_method = "lm", max_lag = 10,
         xreg = forecast::seasonaldummy(AirPassengers), seasonal = F) -> fit
    forecast(fit, h = 2, xreg = forecast::seasonaldummy(AirPassengers, h = 2)) -> fc
    values <- as.numeric(ceiling(c(fc$mean)))
    expect_equal(values, c(446, 445), tolerance = 1)

  })
}


if(require(testthat)){

  test_that("tests for some arguments in forecast 3", {

    forecast(ARml(AirPassengers, caret_method = "lm", max_lag = 12,
                  seasonal = F), h = 2) -> fc
    values <- as.numeric(ceiling(c(fc$mean)))
    expect_equal(values, c(465, 429), tolerance = 1)

  })
}

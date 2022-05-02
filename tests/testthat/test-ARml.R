# A unit test for ARml function
if(require(testthat)){
  test_that("tests for some arguments in ARml", {
    ARml(AirPassengers, caret_method = "lm", max_lag = 12,  K = 5) -> fit

    class_fit <- class(fit)

    expect_that(class_fit, equals("ARml"))

  })
}

if(require(testthat)){
  test_that("tests for some arguments in ARml", {
    ARml(AirPassengers, caret_method = "lm", max_lag = 12, lambda = NULL,
         K= 5) -> fit

    class_fit <- class(fit)

    expect_that(class_fit, equals("ARml"))

  })
}

if(require(testthat)){
  test_that("tests for some arguments in ARml", {
    ARml(AirPassengers, caret_method = "lm", max_lag = 12, lambda = "auto",
         BoxCox_method = "loglik", K = 5) -> fit

    class_fit <- class(fit)

    expect_that(class_fit, equals("ARml"))

  })
}


if(require(testthat)){
  test_that("tests for some arguments in ARml", {
    ARml(AirPassengers, caret_method = "lm", max_lag = 10,
         xreg = forecast::seasonaldummy(AirPassengers), seasonal = F) -> fit

    class_fit <- class(fit)

    expect_that(class_fit, equals("ARml"))

  })
}

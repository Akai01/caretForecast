# A unit test for ARml function
if(require(testthat)){
  test_that("tests for some arguments in ARml", {
    ARml(AirPassengers, caret_method = "lm", maxlag = 12) -> fit

    class_fit <- class(fit)

    expect_that(class_fit, equals("ARml"))

  })
}


if(require(testthat)){
  test_that("tests for some arguments in ARml", {
    ARml(AirPassengers, caret_method = "lm", maxlag = 12, lambda = NULL) -> fit

    class_fit <- class(fit)

    expect_that(class_fit, equals("ARml"))

  })
}

if(require(testthat)){
  test_that("tests for some arguments in ARml", {
    ARml(AirPassengers, caret_method = "lm", maxlag = 12, lambda = "auto",
         BoxCox_method = "loglik") -> fit

    class_fit <- class(fit)

    expect_that(class_fit, equals("ARml"))

  })
}




if(require(testthat)){
  test_that("tests for some arguments in ARml", {
    ARml(AirPassengers, caret_method = "lm", maxlag = 10,
         xreg = rnorm(length(AirPassengers)), K=3) -> fit

    class_fit <- class(fit)

    expect_that(class_fit, equals("ARml"))

  })
}

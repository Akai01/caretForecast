# A unit test for ARml function
if(require(testthat)){

  test_that("tests for some arguments in ARml", {
    ARml(AirPassengers, caret_method = "lm", maxlag = 12, trend_method = "none",
         pre_process = "center") -> fit

    class_fit <- class(fit)

    expect_that(class_fit, equals("ARml"))

  })
}






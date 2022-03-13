# A unit test for get_var_imp function
if(require(testthat)){
  test_that("tests for some arguments in get_var_imp", {
    library(caretForecast)
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 12,
                trend_method = "none",pre_process = "center")
    forecast(fit, h = 12) -> fc
    a <- get_var_imp(fc, plot = FALSE)
    class_a <- class(a)
    expect_that(class_a, equals("varImp.train"))

  })
}


get_var_imp# A unit test for get_var_imp function
if(require(testthat)){

  test_that("tests for some arguments in get_var_imp2", {
    library(caretForecast)
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 12,
                trend_method = "none",pre_process = "center")
    forecast(fit, h = 12) -> fc
    a <- get_var_imp(fc, plot = TRUE)
    class_a <- class(a)
    expect_that(class_a, equals("trellis"))

  })
}

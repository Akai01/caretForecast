# A unit test for get_var_imp function
if(require(testthat)){

  test_that("tests for some arguments in get_var_imp", {
    library(forecast)
    library(caret)
    forecast(ARml(AirPassengers, caret_method = "lm", maxlag = 12, trend_method = "none",
                  pre_process = "center"), h = 12) -> fc

    a <- get_var_imp(fc, plot = F)

    class_a <- class(a)

    expect_that(class_a, equals("varImp.train"))

  })
}


# A unit test for get_var_imp function
if(require(testthat)){

  test_that("tests for some arguments in get_var_imp", {
    forecast(ARml(AirPassengers, caret_method = "lm", maxlag = 12, trend_method = "none",
                  pre_process = "center"), h = 12) -> fc

    a <- get_var_imp(fc, plot = T)

    class_a <- class(a)

    expect_that(class_a, equals("trellis"))

  })
}

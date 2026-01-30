# A unit test for get_var_imp function
if(require(testthat)){
  test_that("tests for some arguments in get_var_imp", {
    library(caretForecast)
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 12,
                pre_process = "center")
    forecast(fit, h = 12) -> fc
    a <- get_var_imp(fc, plot = FALSE)
    class_a <- class(a)
    expect_that(class_a, equals("varImp.train"))

  })
}

# A unit test for get_var_imp function
if(require(testthat)){

  test_that("tests for some arguments in get_var_imp2", {
    library(caretForecast)
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 12,
                pre_process = "center")
    forecast(fit, h = 12) -> fc
    a <- get_var_imp(fc, plot = TRUE)
    class_a <- class(a)
    expect_that(class_a, equals("trellis"))

  })
}

# Test that get_var_imp accepts ARml objects directly (not just forecastARml)
if(require(testthat)){

  test_that("get_var_imp accepts ARml objects", {
    library(caretForecast)
    fit <- ARml(AirPassengers, caret_method = "lm", max_lag = 12,
                verbose = FALSE, calibrate = FALSE)

    # Should work with ARml object directly
    a <- get_var_imp(fit, plot = FALSE)
    expect_s3_class(a, "varImp.train")

    # Should also work with forecastARml object
    fc <- forecast(fit, h = 12)
    b <- get_var_imp(fc, plot = FALSE)
    expect_s3_class(b, "varImp.train")
  })
}

# Test that get_var_imp rejects invalid objects
if(require(testthat)){

  test_that("get_var_imp rejects invalid objects", {
    expect_error(
      get_var_imp(list(a = 1, b = 2)),
      "must be a forecastARml or ARml object"
    )

    expect_error(
      get_var_imp(data.frame(x = 1:10)),
      "must be a forecastARml or ARml object"
    )
  })
}

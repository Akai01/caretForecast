# A unit test for plot.forecast function
if(require(testthat)){
  test_that("tests for some arguments in plot.ts", {
    library(forecast)

    a <- ts_plot(AirPassengers)

    class_a <- class(a)

    expect_that(class_a, equals(c("gg", "ggplot")))

  })
}


# A unit test for plot.forecast function
if(require(testthat)){

  test_that("tests for some arguments in plot.forecast", {
    library(forecast)

    forecast(ARml(AirPassengers, caret_method = "lm", maxlag = 12, trend_method = "none",
                  pre_process = "center"), h = 12) -> fc

    aa <- fc_plot(fc)

    class_aa <- class(aa)

    expect_that(class_aa, equals(c("gg", "ggplot")))

  })
}

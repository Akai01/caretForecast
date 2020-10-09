
# A unit test for split_ts function
if(require(testthat)){

  test_that("tests for some arguments in split_ts", {
    result <- split_ts(retail_wide[,1], test_size = 2)

    result <- as.numeric(result$test)

    expect_that(result, equals(c(14549, 14579)))

  })
}

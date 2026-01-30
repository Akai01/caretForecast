# A unit test for split_ts function
if(require(testthat)){

  test_that("tests for some arguments in split_ts", {
    result <- split_ts(retail_wide[,1], test_size = 2)

    result <- as.numeric(result$test)

    expect_that(result, equals(c(409.4, 583.6)))

  })
}

# Test split_ts returns correct lengths
if(require(testthat)){
  test_that("split_ts returns correct train and test lengths", {
    y <- ts(1:100, frequency = 12)
    result <- split_ts(y, test_size = 20)

    expect_equal(length(result$train), 80)
    expect_equal(length(result$test), 20)
  })
}

# Test split_ts preserves time series attributes
if(require(testthat)){
  test_that("split_ts preserves time series attributes", {
    y <- ts(1:100, frequency = 12, start = c(2020, 1))
    result <- split_ts(y, test_size = 12)

    expect_true(is.ts(result$train))
    expect_true(is.ts(result$test))
    expect_equal(frequency(result$train), 12)
    expect_equal(frequency(result$test), 12)
    expect_equal(start(result$train), c(2020, 1))
  })
}

# Test split_ts error: non-ts input
if(require(testthat)){
  test_that("split_ts rejects non-ts input", {
    expect_error(
      split_ts(c(1, 2, 3, 4, 5), test_size = 2),
      "y must be a univariate time series"
    )
  })
}

# Test split_ts error: mts input
if(require(testthat)){
  test_that("split_ts rejects multivariate ts (mts)", {
    mts_data <- ts(matrix(1:20, ncol = 2), frequency = 1)
    expect_error(
      split_ts(mts_data, test_size = 2),
      "y must be a univariate time series"
    )
  })
}

# Test split_ts error: test_size <= 0
if(require(testthat)){
  test_that("split_ts rejects test_size <= 0", {
    y <- ts(1:20, frequency = 1)

    expect_error(
      split_ts(y, test_size = 0),
      "test_size must be greater than 0"
    )

    expect_error(
      split_ts(y, test_size = -5),
      "test_size must be greater than 0"
    )
  })
}

# Test split_ts error: test_size >= length(y)
if(require(testthat)){
  test_that("split_ts rejects test_size >= length(y)", {
    y <- ts(1:20, frequency = 1)

    expect_error(
      split_ts(y, test_size = 20),
      "test_size must be less than length"
    )

    expect_error(
      split_ts(y, test_size = 25),
      "test_size must be less than length"
    )
  })
}

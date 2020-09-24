# A unit test for suggested_methods function
if(require(testthat)){

  test_that("tests for some arguments in suggested_methods", {
    result <- suggested_methods()

    expect_that(result, equals(c("cubist", "svmLinear", "enet", "bridge", "glmboost",
                                    "ridge", "lasso", "relaxo", "M5Rules", "M5", "lm",
                                    "gaussprLinear", "glm", "glmnet", "pcr", "ppr", "foba",
                                    "gbm", "svmLinear2", "glm.nb", "gcvEarth", "lars2", "lars",
                                    "icr", "ctree2", "ctree", "bayesglm")))

  })
}

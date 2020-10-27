
<!-- README.md is generated from README.Rmd. Please edit that file -->

caretForecast
=============

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/Akai01/caretForecast.svg?branch=master)](https://travis-ci.com/Akai01/caretForecast)
[![codecov](https://codecov.io/gh/Akai01/caretForecast/branch/master/graph/badge.svg)](https://codecov.io/gh/Akai01/caretForecast)
[![CRAN
status](https://www.r-pkg.org/badges/version/caretForecast)](https://CRAN.R-project.org/package=caretForecast)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

The goal of caretForecast is to provide tools for forecasting time
series data using various machine learning algorithms. (More details
will follow…)

Installation
------------

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Akai01/caretForecast")
```

Example
-------

### Note: User can train any caret supported regression model.

These are basic examples which shows you how to solve common problems
with different ML models.

``` r
library(caretForecast)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
library(forecast)

# Forecasting Retail Data with glmboost
data(retail_wide, package = "caretForecast")

i <- 8

dtlist <- caretForecast::split_ts(retail_wide[,i], test_size = 12)

training_data <- dtlist$train

testing_data <- dtlist$test

fit <- ARml(training_data, max_lag = 12, caret_method = "glmboost", 
            verbose = FALSE)
#> Loading required package: lattice
#> Loading required package: ggplot2

forecast(fit, h = length(testing_data), level = c(95,80))-> fc

accuracy(fc, testing_data)
#>                     ME     RMSE      MAE        MPE     MAPE      MASE
#> Training set 0.8868074 16.39661 11.65025 -0.3620986 5.702257 0.7559694
#> Test set     8.3976171 20.15546 17.20306  3.0153042 5.572722 1.1162843
#>                   ACF1 Theil's U
#> Training set 0.5707204        NA
#> Test set     0.3971016 0.7547318


fc_plot(fc) + 
autolayer(testing_data, series = "testing_data")
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r


## NOTE : Promotions, holidays, and other external variables can be added in the model via xreg argument. Please look at the documentation of ARml.

# Forecasting Retail Data with cubist regression

i <- 9

data(retail_wide, package = "caretForecast")

dtlist <- caretForecast::split_ts(retail_wide[,i], test_size = 12)

training_data <- dtlist$train

testing_data <- dtlist$test

fit <- ARml(training_data, max_lag = 12, caret_method = "cubist", 
            verbose = FALSE)

forecast(fit, h = length(testing_data), level = c(95,80))-> fc

accuracy(fc, testing_data)
#>                     ME     RMSE      MAE         MPE     MAPE      MASE
#> Training set 0.3452345 16.39877 12.22406 -0.08475644 2.533889 0.4073634
#> Test set     2.5562312 14.21461 12.39887  0.24907619 1.592606 0.4131888
#>                    ACF1 Theil's U
#> Training set  0.2309758        NA
#> Test set     -0.1450719 0.1701567

fc_plot(fc) + 
autolayer(testing_data, series = "testing_data")
```

<img src="man/figures/README-example-2.png" width="100%" />

``` r


# Forecasting using Support Vector Machines with Linear Kernel

data(retail_wide, package = "caretForecast")

i <- 5

dtlist <- caretForecast::split_ts(retail_wide[,i], test_size = 12)

training_data <- dtlist$train

testing_data <- dtlist$test

fit <- ARml(training_data, max_lag = 12, caret_method = "svmLinear", 
            verbose = FALSE)

forecast(fit, h = length(testing_data), level = c(95,80))-> fc

accuracy(fc, testing_data)
#>                        ME     RMSE      MAE       MPE     MAPE      MASE
#> Training set -0.004996011 5.218181 3.980904 -0.333970 4.500042 0.5563544
#> Test set     -7.264572944 9.403773 7.652439 -8.319266 8.642448 1.0694725
#>                    ACF1 Theil's U
#> Training set 0.02901247        NA
#> Test set     0.22464379 0.7207839

fc_plot(fc) + 
autolayer(testing_data, series = "testing_data")
```

<img src="man/figures/README-example-3.png" width="100%" />

``` r
get_var_imp(fc)
```

<img src="man/figures/README-example-4.png" width="100%" />

``` r
get_var_imp(fc, plot = F)
#> loess r-squared variable importance
#> 
#>   only 20 most important variables shown (out of 22)
#> 
#>        Overall
#> lag12 100.0000
#> lag3   81.1655
#> lag1   79.2218
#> lag2   78.2771
#> lag5   75.4255
#> lag9   74.8507
#> lag10  74.3075
#> lag4   73.4707
#> lag7   71.2428
#> lag11  69.4001
#> lag8   69.3380
#> lag6   66.1550
#> S5-12   6.4068
#> S3-12   3.9841
#> S1-12   3.8337
#> C4-12   3.6437
#> C3-12   2.1007
#> S4-12   1.6039
#> C2-12   0.9426
#> C5-12   0.4801



# Forecasting using Ridge Regression
data(retail_wide, package = "caretForecast")

i <- 8

dtlist <- caretForecast::split_ts(retail_wide[,i], test_size = 12)

training_data <- dtlist$train

testing_data <- dtlist$test

fit <- ARml(training_data, max_lag = 12, caret_method = "ridge", 
            verbose = FALSE)

forecast(fit, h = length(testing_data), level = c(95,80))-> fc

accuracy(fc, testing_data)
#>                     ME     RMSE      MAE        MPE     MAPE      MASE
#> Training set 0.1414756  8.88367  6.44208 -0.0953757 3.126316 0.4180182
#> Test set     0.9965672 17.71459 13.30744  0.7019753 4.092068 0.8635023
#>                     ACF1 Theil's U
#> Training set 0.004518837        NA
#> Test set     0.389409945 0.6513039

fc_plot(fc) + 
autolayer(testing_data, series = "testing_data")
```

<img src="man/figures/README-example-5.png" width="100%" />

``` r
get_var_imp(fc)
```

<img src="man/figures/README-example-6.png" width="100%" />

``` r
get_var_imp(fc, plot = F)
#> loess r-squared variable importance
#> 
#>   only 20 most important variables shown (out of 22)
#> 
#>         Overall
#> lag12 100.00000
#> lag1   89.57697
#> lag11  86.89260
#> lag2   85.81967
#> lag3   84.07269
#> lag10  83.53358
#> lag9   82.52774
#> lag7   82.18917
#> lag4   81.93730
#> lag5   81.85290
#> lag8   81.22696
#> lag6   79.96364
#> S1-12   4.51371
#> S3-12   1.59063
#> C2-12   1.54661
#> S5-12   1.53004
#> C4-12   1.24044
#> C1-12   0.38784
#> S2-12   0.35446
#> S4-12   0.07244
```

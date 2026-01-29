# Fit a horizon-specific conformal regressor for time series forecasting.

This function creates a conformal regressor that accounts for increasing
uncertainty at longer forecast horizons. It uses separate nonconformity
score distributions for each horizon h=1,2,3,..., resulting in
prediction intervals that naturally widen as the forecast horizon
increases (trumpet-shaped intervals).

## Usage

``` r
conformalRegressorByHorizon(horizon_errors)
```

## Arguments

- horizon_errors:

  A named list where each element contains sorted absolute errors for
  that horizon. Names should be "h1", "h2", etc. This is typically
  produced by
  [`calibrate_horizon_scores()`](https://akai01.github.io/caretForecast/reference/calibrate_horizon_scores.md).

## Value

A conformalRegressorByHorizon object containing:

- alphas_by_horizon:

  List of sorted nonconformity scores for each horizon

- max_horizon:

  Maximum calibrated horizon

- n_samples:

  Number of calibration samples per horizon

## References

Boström, H., 2022. crepes: a Python Package for Generating Conformal
Regressors and Predictive Systems. In Conformal and Probabilistic
Prediction and Applications. PMLR, 179.

Stankeviciute, K., Alaa, A. M., & van der Schaar, M., 2021. Conformal
Time-series Forecasting. NeurIPS 2021.

## Author

Resul Akay

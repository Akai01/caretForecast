# Predict intervals from a horizon-specific conformal regressor

This function generates prediction intervals that account for increasing
uncertainty at longer forecast horizons. Each horizon h uses its own
calibrated nonconformity score distribution, resulting in trumpet-shaped
prediction intervals.

## Usage

``` r
# S3 method for class 'conformalRegressorByHorizon'
predict(
  object,
  y_hat = NULL,
  confidence = 0.95,
  y_min = -Inf,
  y_max = Inf,
  ...
)
```

## Arguments

- object:

  A conformalRegressorByHorizon object

- y_hat:

  Predicted values (one per horizon)

- confidence:

  Confidence level(s) between 0 and 1 (e.g., 0.95 for 95 percent)

- y_min:

  The minimum value to include in prediction intervals

- y_max:

  The maximum value to include in prediction intervals

- ...:

  Ignored

## Value

A data frame with lower and upper bounds for each confidence level

## Author

Resul Akay

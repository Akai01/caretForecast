# Predict a conformalRegressor

Predict a conformalRegressor

## Usage

``` r
# S3 method for class 'conformalRegressor'
predict(
  object,
  y_hat = NULL,
  sigmas = NULL,
  confidence = 0.95,
  y_min = -Inf,
  y_max = Inf,
  ...
)
```

## Arguments

- object:

  A conformalRegressor object

- y_hat:

  Predicted values

- sigmas:

  Difficulty estimates

- confidence:

  Confidence level

- y_min:

  The minimum value to include in prediction intervals

- y_max:

  The maximum value to include in prediction intervals

- ...:

  Ignored

## Value

Prediction intervals

## Author

Resul Akay

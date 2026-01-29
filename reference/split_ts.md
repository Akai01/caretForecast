# Split a time series into training and testing sets

Split a time series into training and testing sets

## Usage

``` r
split_ts(y, test_size = 10)
```

## Arguments

- y:

  A univariate time series

- test_size:

  The number of observations to keep in the test set

## Value

A list with train and test elements

## Author

Resul Akay

## Examples

``` r
dlist <- split_ts(retail_wide[,1], test_size = 12)
```

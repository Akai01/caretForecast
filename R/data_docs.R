#' Sales data from an Australian Retailer in time series format
#'
#' A dataset containing 42 products' sales
#'
#' @format
#' An object of class mts (inherits from ts, matrix)
#' with 333 rows and 43 columns.
#' \describe{
#'   This data set is the wide format of \code{\link{retail}} data.
#' }
#' @source \url{https://robjhyndman.com/data/ausretail.csv}
"retail_wide"

#' Grouped sales data from an Australian Retailer
#'
#' A dataset containing 42 products' sales
#'
#' @format A data class of "tbl_df", "tbl", "data.frame" with 13986 rows and 3 columns:
#' \describe{
#'   \item{date}{date}
#'   \item{item}{products}
#'   \item{value}{sales}
#' }
#' @source \url{https://robjhyndman.com/data/ausretail.csv}
"retail"

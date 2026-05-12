#' Convert Demand to Cumulative Values
#'
#' Converts a demand vector to cumulative values, using the first cumulative value
#' as the starting point.
#'
#' @param demand Numeric vector of demand values
#' @param cumulative Numeric vector of cumulative values (only first element is used)
#'
#' @return Numeric vector of cumulative sum
#'
#' @details
#' The function sets the first demand value to match the first cumulative value,
#' then computes the cumulative sum of the demand vector.
#'
#' @examples
#' \dontrun{
#' demand <- c(5, 10, 15, 20)
#' cumulative <- c(100, 0, 0, 0) # Only first value matters
#' make_cumulative(demand, cumulative)
#' # Returns: 100, 110, 125, 145
#' }
#'
#' @export
make_cumulative <- function(demand, cumulative) {
  demand[1] <- cumulative[1]
  cumsum(demand)
}

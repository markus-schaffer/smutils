#' Impute Missing Values with Moving Average and Scaling
#'
#' Imputes missing demand values using a C++ moving average (`ma()`) and scales the result to match
#' cumulative totals. Designed for time series with both demand and cumulative measurements.
#'
#' @param demand Numeric vector of demand values (may contain NA)
#' @param cumulative Numeric vector of cumulative values corresponding to demand
#'
#' @return Numeric vector of imputed demand values, scaled to match cumulative totals.
#'   Returns original demand if no imputation is possible or needed.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Returns original values if no NAs or fewer than 2 non-NA values exist
#'   \item Uses linear weighted moving average (k=48) for initial imputation
#'   \item Scales imputed values using [scale_accum()] to match cumulative totals
#'   \item Returns NA vector if scaling fails
#' }
#'
#' @examples
#' \dontrun{
#' demand <- c(10, NA, NA, 15, 20)
#' cumulative <- c(10, 25, 40, 55, 75)
#' impute_ma(demand, cumulative)
#' }
#'
#' @seealso [scale_accum()]
#'
#'
#' @export
impute_ma <- function(demand, cumulative) {
  na_vec <- is.na(demand)

  if (!any(na_vec)) {
    return(demand)
  } else if (sum(!na_vec) < 2) {
    return(rep(NA_real_, length(demand)))
  } else {
    imp_result <- ma(demand, k = 48, weighting = "linear")
    if (max(which(na_vec)) == 1 || isTRUE(all.equal(which(na_vec), c(1, length(na_vec)), check.class = FALSE))) {
      return(imp_result)
    } else {
      tryCatch(
        {
          scale_accum(
            cumulative = cumulative,
            demand = demand,
            imp_result = imp_result
          )
        },
        error = function(e) {
          rep(NA, length(demand))
        },
        warning = function(w) {
          rep(NA, length(demand))
        }
      )
    }
  }
}

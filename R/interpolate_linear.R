#' Interpolate Time Series Data
#'
#' Performs extrapolated interpolation on time series data,
#' returning results as a data.table with unique time points.
#'
#' @param x Numeric or POSIXct vector of input time points
#' @param y Numeric vector of values to interpolate (may contain NA)
#' @param xout Numeric or POSIXct vector of output time points for interpolation
#' @param qname Character string specifying the column name for the interpolated values
#'
#' @return A data.table with columns:
#'   \itemize{
#'     \item `time_rounded`: Time points (from xout)
#'     \item Column named as `qname`: Interpolated values
#'   }
#'
#' @details
#' If fewer than 2 non-NA values exist in y, the function returns NA for all xout values.
#' Otherwise, it uses `Hmisc::approxExtrap` for interpolation with extrapolation.
#' Results are deduplicated before returning.
#'
#' @examples
#' \dontrun{
#' x <- 1:10
#' y <- c(1, 2, 3.5, 4.5, 5, 6, 7.5, 8.5, 9, 10)
#' xout <- 1:10
#' result <- interpolate_linear(x, y, xout, "value")
#' }
#'
#' @export
interpolate_linear <- function(x, y, xout, qname) {
  log_vec <- !is.na(y)
  if (sum(log_vec) < 2) {
    res <- data.table::data.table(
      time_rounded = xout,
      V1 = rep(NA_real_, length(xout))
    )
    res <- unique(res)
  } else {
    res <- data.table::data.table(
      time_rounded = xout[log_vec],
      V1 = Hmisc::approxExtrap(
        x = as.numeric(x)[log_vec],
        y = y[log_vec],
        xout = as.numeric(xout)[log_vec]
      )$y
    )
  }

  data.table::setnames(res, "V1", qname)
  res <- unique(res)
  return(res)
}

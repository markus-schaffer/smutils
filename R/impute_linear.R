#' Impute Missing Values
#'
#' Performs imputation with extrapolation on vectors with missing values.
#'
#' @param x Numeric vector with potential missing values
#' @param time Numeric vector of time points. Default is 1:length(x)
#'
#' @return Numeric vector with missing values interpolated
#'
#' @details
#' Returns original vector if there are fewer than 2 non-missing values or no missing
#' values. Otherwise uses `Hmisc::approxExtrap` for interpolation/imputation with extrapolation.
#'
#' @examples
#' \dontrun{
#' x <- c(1, 2, NA, NA, 5, 6)
#' impute_linear(x)
#' }
#'
#' @note Original function name: `na_int_imp`
#'
#' @export
impute_linear <- function(x, time = 1:length(x)) {
  missindx <- which(is.na(x))

  if (sum(!is.na(x)) < 2 || length(missindx) == 0) {
    return(x)
  }
  n <- length(x)
  indx <- which(!is.na(x))
  x[missindx] <- Hmisc::approxExtrap(x = time[indx], y = x[indx], xout = time[missindx])$y

  return(x)
}

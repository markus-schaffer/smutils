#' Scale Imputed Values to Match Cumulative Totals
#'
#' Scales imputation results for demand values to match the actual differences
#' in cumulative values across gaps.
#'
#' @param cumulative Numeric vector of cumulative values
#' @param demand Numeric vector of demand values (with missing values)
#' @param imp_result Numeric vector of imputed demand values
#'
#' @return Numeric vector of scaled imputation results
#'
#' @details
#' This function is used when cumulative values are available but demand values
#' have gaps. It scales the imputed demand values so that their sum over each
#' gap matches the actual change in cumulative values.
#'
#' The scaling process:
#' \enumerate{
#'   \item Identifies gaps in the demand data
#'   \item Calculates the real difference in cumulative values across each gap
#'   \item Scales imputed values proportionally to match the real difference
#'   \item Handles edge cases (last value missing, zero sums)
#' }
#'
#' @examples
#' \dontrun{
#' demand <- c(10, NA, NA, 15, 20)
#' cumulative <- c(10, 25, 40, 55, 75)
#' imp_result <- c(10, 12, 13, 15, 20) # Initial imputation
#' scale_accum(cumulative, demand, imp_result)
#' }
#'
#' @export
scale_accum <- function(cumulative, demand, imp_result) {
  lag_bool_real <- is.na(demand)

  if (all(!lag_bool_real)) {
    return(imp_result)
  }

  # Handle the case that the last demand value is missing - this happens if the
  # second to last cumulative value is missing
  if (lag_bool_real[length(lag_bool_real)] == TRUE) {
    last_value_missing <- TRUE # last demand value is missing
    lag_bool_real[length(lag_bool_real)] <- FALSE # ignore last demand value for scaling
  } else {
    last_value_missing <- FALSE
  }

  lag_bool_real[1] <- FALSE

  # Get the length of gaps and non gaps as well as their sequence
  len <- rle(lag_bool_real)

  # Calculate the pattern for the cumulative values This is needed as by
  # calculating the demand as lagged difference the number and pattern of gaps
  # can changed i.e. if only one value exists between two gaps the two gaps
  # become one
  len_temp <- len
  len_temp$lengths[2:length(len_temp$lengths)] <- len_temp$lengths[2:length(len_temp$lengths)] - c(1, -1)

  # Handle the case that the last demand value is missing - so the gap pattern
  # must be corrected
  if (last_value_missing == FALSE) {
    bool_real <- rep(len_temp$values, len_temp$lengths)
  } else if (last_value_missing == TRUE) {
    last_two_values <- length(len_temp$lengths):(length(len_temp$lengths) - 1)
    len_temp$lengths[last_two_values] <- len_temp$lengths[last_two_values] - c(1, -1)
    bool_real <- rep(len_temp$values, len_temp$lengths)
  }

  # get the real difference of the value before and after the gaps i.e. the
  # amount of quantity that should be interpolated per gap
  difference <- (cumulative[!bool_real] - data.table::shift(cumulative[!bool_real]))[utils::head(cumsum(len$lengths[!len$values] + 1), -1)]

  # Scale imputation result to fit the gap
  main <- data.table::data.table() # Initialize the data.table
  main[, result := imp_result[lag_bool_real]] # Get imputed positions
  main[result < 0, result := 0] # Ensure positive results
  n_sum <- len$lengths[len$values] # number of missing values per gaps
  main[, group := rep(1:length(n_sum), n_sum)] # make one group per gap
  main[, sum := sum(result), by = "group"] # sum the imputation results per gap
  main[, real_sum := rep(difference, n_sum)] # real sum for each gap in each group
  main[, scaled_result := result * (real_sum / sum)] # get the scaled result
  main[is.na(scaled_result), scaled_result := result + (real_sum - sum) / n_sum[group]] # needed if sum of imputed == 0
  imp_result[lag_bool_real] <- main$scaled_result

  return(imp_result)
}

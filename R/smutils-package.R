#' smutils: Utility Functions for Smart Meter Data Processing
#'
#' A collection of utility functions for processing smart meter data, including
#' time series interpolation, missing value imputation, cumulative meter reset
#' correction, demand disaggregation, and efficient C++ helpers for string
#' matching and run-length encoding.
#'
#' @section Time Series and Interpolation:
#' \itemize{
#'   \item [interpolate_linear()] - Interpolate missing values with extrapolation
#'   \item [impute_ma()] - Impute missing demand using a moving average scaled to cumulative totals
#'   \item [impute_linear()] - Impute missing values using linear extrapolation
#' }
#'
#' @section Data Processing:
#' \itemize{
#'   \item [make_cumulative()] - Convert demand values to cumulative format
#'   \item [scale_accum()] - Scale imputed values to match cumulative totals
#'   \item [reset_meter()] - Reconstruct a cumulative counter after resets
#'   \item [reset_meter_binary()] - Detect whether a meter has been reset
#'   \item [spms()] - Smooth and scale demand using the SPMS method
#'   \item [disaggregate_heat()] - Disaggregate total heat into space heating and DHW
#' }
#'
#' @section C++ Optimised Functions:
#' \itemize{
#'   \item [ma()] - Moving average imputation (simple, linear, or exponential weighting)
#'   \item [fast_round_time()] - Fast rounding of POSIXct time values
#'   \item [check_completeness_list()] - Check string list completeness against a reference
#'   \item [get_match_values()] - Get matching values from character vectors
#'   \item [get_match_values_list()] - Get matching values from a list of character vectors
#'   \item [rle2()] - Run-length encoding with length per element
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rcpp evalCpp
#' @useDynLib smutils, .registration = TRUE
## usethis namespace: end

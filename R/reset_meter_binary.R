#' Check if Meter Has Been Reset
#'
#' Checks whether a cumulative meter vector contains any resets (decreases in value).
#'
#' @param vec Numeric vector representing cumulative meter readings
#'
#' @return Logical value: TRUE if meter has been reset, FALSE otherwise
#'
#' @examples
#' meter1 <- c(10, 20, 30, 40)
#' reset_meter_binary(meter1) # Returns FALSE
#'
#' meter2 <- c(10, 20, 30, 5, 10)
#' reset_meter_binary(meter2) # Returns TRUE
#'
#' @export
reset_meter_binary <- function(vec) {
  if (!is.numeric(vec)) stop("`vec` must be numeric.", call. = FALSE)
  any(diff(vec) < 0)
}

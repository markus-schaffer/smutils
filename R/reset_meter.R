#' Reconstruct a cumulative counter with multiple resets
#'
#' @description
#' Many sensors produce cumulative counts that occasionally reset to zero
#' (e.g., device restart, overflow, power loss). This function reconstructs
#' a monotonically non-decreasing cumulative series by detecting each reset
#' (any negative step in the time series) and adding cumulative offsets.
#'
#' @param vec A numeric vector representing cumulative meter readings.
#'
#' @details
#' A reset is detected whenever `diff(vec) < 0`. Each reset introduces a
#' new segment, and the function adds the maximum value of the previous segment
#' to all following values, accumulating offsets across consecutive resets.
#'
#' This version handles:
#' \itemize{
#'   \item multiple consecutive resets
#'   \item NA values in the pre-reset segment
#'   \item strictly increasing reconstruction
#' }
#'
#' @return A numeric vector of the same length as `vec`, adjusted so
#' that the cumulative series is monotonically non-decreasing.
#'
#' @examples
#' # No resets
#' reset_meter(c(0, 1, 2, 3))
#'
#' # Single reset
#' reset_meter(c(1, 2, 3, 0, 1, 2))
#'
#' # Multiple consecutive resets
#' reset_meter(c(1, 2, 3, 4, 2, 1, 2, 3))
#'
#' @export
reset_meter <- function(vec) {
  # Validate input
  if (!is.numeric(vec)) {
    stop("`vec` must be numeric.", call. = FALSE)
  }

  n <- length(vec)
  if (n <= 1) {
    return(vec)
  }

  # Where the vector decreases → reset points
  idx_resets <- which(diff(vec) < 0) + 1
  if (length(idx_resets) == 0) {
    return(vec)
  }

  # Segment boundaries
  starts_pre <- c(1, idx_resets)
  ends_pre <- idx_resets - 1
  starts_post <- idx_resets
  ends_post <- c(idx_resets[-1] - 1, n)

  out <- vec
  offset <- 0

  for (k in seq_along(idx_resets)) {
    # Max of pre-reset segment; if all NA, treat as 0
    pre_max <- suppressWarnings(max(vec[starts_pre[k]:ends_pre[k]], na.rm = TRUE))
    if (!is.finite(pre_max)) {
      pre_max <- 0
    }
    offset <- offset + pre_max
    out[starts_post[k]:ends_post[k]] <- vec[starts_post[k]:ends_post[k]] + offset
  }

  out
}

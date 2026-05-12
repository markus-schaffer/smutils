#' Generate weights for moving averages
#'
#' Computes weights based on window size, type, and alignment.
#' Supported types: "simple", "linear", "exponential".
#' Supported alignments: "left", "center", "right" (internal helper)
#'
#' @param window Integer, size of the window (>= 1).
#' @param type Character, one of "simple", "linear", "exponential".
#' @param align Character, one of "left", "center", "right".
#'
#' @return A numeric vector of weights.
#'
#' @examples
#' # Simple weights for a window of 5
#' get_weights(window = 5, type = "simple", align = "left")
#'
#' # Linear weights, center alignment, odd window
#' get_weights(window = 5, type = "linear", align = "center")
#'
#' # Exponential weights, right alignment
#' get_weights(window = 4, type = "exponential", align = "right")
#'
#' @keywords internal
#' @noRd
get_weights <- function(window = 5, type = "simple", align = "left") {
  # Validate inputs
  if (!is.numeric(window) || length(window) != 1 || window < 1) {
    stop("'window' must be a single integer >= 1.")
  }
  if (!type %in% c("simple", "linear", "exponential")) {
    stop("'type' must be one of: 'simple', 'linear', 'exponential'.")
  }
  if (!align %in% c("left", "center", "right")) {
    stop("'align' must be one of: 'left', 'center', 'right'.")
  }

  # Compute weights
  if (type == "simple") {
    return(rep(1, window))
  }

  if (type == "linear") {
    if (align == "left") {
      return(seq(window, 1))
    } else if (align == "right") {
      return(seq(1, window))
    } else if (align == "center") {
      if (window %% 2 == 0) {
        half <- window %/% 2
        return(c(seq(1, half), seq(half, 1)))
      } else {
        half <- ceiling(window / 2)
        return(c(seq(1, half), seq(half - 1, 1)))
      }
    }
  }

  if (type == "exponential") {
    if (align == "left") {
      return(2 / ((1:window) + 1))
    } else if (align == "right") {
      return(2 / ((window:1) + 1))
    } else if (align == "center") {
      if (window %% 2 == 0) {
        half <- window %/% 2
        return(c(2 / ((half:1) + 1), 2 / ((1:half) + 1)))
      } else {
        half <- ceiling(window / 2)
        return(c(2 / ((half:1) + 1), 2 / ((2:half) + 1)))
      }
    }
  }

  stop("No proper weighting arguments found. Check 'window', 'type', and 'align'.")
}

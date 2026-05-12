#' Pad a vector with NA values (internal helper)
#'
#' This function is used internally for padding vectors when applying moving averages.
#' It is not intended for direct use by package users.
#'
#' @param vec A numeric vector.
#' @param window An integer specifying the window size.
#' @param align Alignment: "left", "right", or "center".
#'
#' @return A padded numeric vector.
#'
#' @examples
#' pad(c(1, 2, 3), window = 3, align = "left")
#' pad(c(1, 2, 3), window = 3, align = "right")
#' pad(c(1, 2, 3), window = 4, align = "center")
#'
#' @keywords internal
#' @noRd
pad <- function(vec, window = 3, align = "left") {
  # Validate inputs
  if (!is.numeric(vec)) stop("'vec' must be a numeric vector.")
  if (!is.numeric(window) || length(window) != 1 || window < 1) {
    stop("'window' must be a single integer >= 1.")
  }
  if (!align %in% c("left", "right", "center")) {
    stop("'align' must be one of: 'left', 'center', 'right'.")
  }

  # Compute padding
  if (align == "left") {
    return(c(vec, rep(NA, window - 1)))
  } else if (align == "right") {
    return(c(rep(NA, window - 1), vec))
  } else if (align == "center") {
    if ((window %% 2) == 0) { # even
      left_pad <- (window %/% 2) - 1
      right_pad <- window %/% 2
    } else { # odd
      left_pad <- window %/% 2
      right_pad <- window %/% 2
    }
    return(c(rep(NA, left_pad), vec, rep(NA, right_pad)))
  }
}

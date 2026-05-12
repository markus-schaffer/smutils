#' Smooth and scale demand using SPMS method
#'
#' SPMS: Smooth – Pointwise Move – Scale was developed to mitigate the effect of
#' low transmission resolution of SHMs that create level-like patterns.
#'
#' @param data A `data.table` containing the demand and grouping columns.
#' @param demand_col Character, name of the column with demand values.
#' @param group_col Character, name of the column used for grouping (e.g., "day").
#' @param window Integer, size of the smoothing window (default = 5).
#' @param align Character, alignment for moving average: "left", "center", or "right" (default = "center").
#' @param offset Numeric, offset for scaling (default = 0.4).
#'
#' @return A `data.table` with an additional column `demand_spms`.
#'
#' @examples
#' library(data.table)
#' dt <- data.table(day = rep(1:2, each = 5), demand = c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28))
#' spms(dt, demand_col = "demand", group_col = "day", window = 3, align = "center", offset = 0.4)
#'
#' @references
#' Schaffer, M., Leiria, D., Vera-Valdés, J. E., & Marszal-Pomianowska, A. (2023).
#' *Increasing the accuracy of low-resolution commercial smart heat meter data and analysing its error.*
#' Proceedings of the European Conference on Computing in Construction.
#' DOI: \doi{10.35490/EC3.2023.208}
#' URL: <https://ec-3.org/publications/conference/paper/?id=EC32023_208>
#'
#' @export
spms <- function(data, demand_col, group_col, window = 5, align = "center", offset = 0.4) {
  # Validate inputs
  if (!inherits(data, "data.table")) stop("'data' must be a data.table.")
  if (!demand_col %in% names(data)) stop(paste("Column", demand_col, "not found in data."))
  if (!group_col %in% names(data)) stop(paste("Column", group_col, "not found in data."))

  # Compute weights
  weights <- get_weights(window = window, type = "linear", align = align)

  # Smooth using frollapply
  data[, demand_spms := collapse::na_rm(data.table::frollapply(
    pad(get(demand_col), window = window, align = align),
    n = window,
    FUN = function(x) stats::weighted.mean(x, w = weights, na.rm = TRUE),
    align = align
  ))]

  # Scale results by group - commonly day
  data[, demand_spms := scale_spms(smd = demand_spms, demand = get(demand_col), offset = offset), by = group_col]

  return(data)
}

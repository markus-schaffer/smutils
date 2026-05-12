#' Scale Supply to Match Demand with Offset Constraints
#'
#' Adjusts a numeric vector `smd` so that its sum matches the sum of `demand`,
#' subject to per-element bounds defined by `offset`. If exact matching is not
#' possible within tolerance, returns `NA_real_` for all positions.
#'
#' @param smd Numeric vector of initial values.
#' @param demand Numeric vector of target demand per position.
#' @param offset Numeric vector of symmetric offsets around `demand`.
#'
#' @return A numeric vector of adjusted values or `NA_real_` if infeasible.
#'
#' @keywords internal
#' @noRd
scale_spms <- function(smd, demand, offset, max_iter = 1000L) {
  n <- length(smd)

  lower_bound <- pmax(0, demand - (offset))
  upper_bound <- demand + (offset)

  smd_off <- pmax(pmin(smd, upper_bound), lower_bound)
  smd_off[smd_off < 0] <- 0

  dif <- sum(demand) - sum(smd_off)

  for (i in seq_len(max_iter)) {
    if (isTRUE(all.equal(abs(dif), 0))) break

    if (dif > 0) {
      adjustable <- smd_off < upper_bound
      n_adj <- sum(adjustable)
      if (n_adj == 0L) break

      headroom <- upper_bound[adjustable] - smd_off[adjustable]
      adjustment <- pmin(dif / n_adj, headroom)
      smd_off[adjustable] <- smd_off[adjustable] + adjustment
    } else {
      adjustable <- smd_off > lower_bound
      n_adj <- sum(adjustable)
      if (n_adj == 0L) break

      headroom <- smd_off[adjustable] - lower_bound[adjustable]
      adjustment <- pmin(-dif / n_adj, headroom)
      smd_off[adjustable] <- smd_off[adjustable] - adjustment
    }

    dif <- sum(demand) - sum(smd_off)
  }

  if (!isTRUE(all.equal(abs(dif), 0))) {
    return(rep(NA_real_, n))
  }

  return(smd_off)
}

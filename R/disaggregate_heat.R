#' Disaggregate Heat Energy into Space Heating and Domestic Hot Water
#'
#' @description Disaggregates total heat energy consumption into space heating (SH)
#' and domestic hot water (DHW) components using a Random Forest regression model.
#'
#' @param dt A \code{data.table} containing the energy data and features. Modified by
#'   reference.
#' @param features Character vector of feature column names to use for prediction
#'   (e.g., outdoor temperature, time variables).
#' @param group Character string. Name of the column indicating the grouping variable
#'   where 1 = training data and 0 = prediction data.
#' @param id Character string. Name of the column containing the unique identifier
#'   for the building/meter.
#' @param energy Character string. Name of the column containing total energy
#'   consumption.
#' @param water_use_m3 Character string or NULL. Name of the column containing water
#'   usage in cubic meters. If provided, used for thermodynamic plausibility checks
#'   on DHW. Default is NULL.
#' @param t_cold Numeric. Cold water temperature in Celsius. Default is 10.
#' @param t_hot Numeric. Hot water temperature in Celsius. Default is 55.
#' @param save Logical. If TRUE, saves results to files instead of returning them.
#'   Default is FALSE.
#' @param path Character string or NULL. Directory path for saving results when
#'   \code{save = TRUE}. Required if \code{save = TRUE}.
#' @param seed Integer or NULL. Random seed for reproducibility. If NULL, no seed is
#'   set. Default is NULL.
#'
#' @return If \code{save = FALSE}, returns a list with two elements:
#' \describe{
#'   \item{data}{A \code{data.table} with the original data plus columns
#'     \code{sh_calculated} and \code{dhw_calculated}.}
#'   \item{performance}{A \code{data.table} with cross-validation performance metrics
#'     including CV(RMSE), bias, RMSE, MAE, and computation time.}
#' }
#'   If \code{save = TRUE}, returns NULL invisibly and writes:
#' \itemize{
#'   \item \code{data_<id>.fst} - The disaggregated data
#'   \item \code{per_<id>.csv} - The performance metrics
#' }
#'
#' @details The disaggregation approach assumes that during non-heating periods
#' (typically summer), all heat energy is used for DHW. A Random Forest model is
#' trained on space heating only data to predict the space heating component based on features like
#' outdoor temperature. The DHW is then calculated as total energy minus predicted
#' space heating.
#'
#' Plausibility checks ensure:
#' \itemize{
#'   \item DHW is non-negative
#'   \item DHW does not exceed total energy consumption
#'   \item If water usage data is provided, DHW does not exceed the thermodynamic
#'     maximum based on heating water from \code{t_cold} to \code{t_hot}
#' }
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' # Create sample data
#' dt <- data.table(
#'   building_id = rep("B001", 365),
#'   date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 365),
#'   temp_outdoor = rnorm(365, mean = 10, sd = 8),
#'   total_heat_kwh = runif(365, 50, 200),
#'   heating_season = c(rep(1, 120), rep(0, 125), rep(1, 120))
#' )
#'
#' # Disaggregate heat
#' result <- disaggregate_heat(
#'   dt = dt,
#'   features = "temp_outdoor",
#'   group = "heating_season",
#'   id = "building_id",
#'   energy = "total_heat_kwh"
#' )
#'
#' # Access results
#' head(result$data)
#' result$performance
#' }
#'
#' @export
disaggregate_heat <- function(dt,
                              features,
                              group,
                              id,
                              energy,
                              water_use_m3 = NULL,
                              t_cold = 10,
                              t_hot = 55,
                              save = FALSE,
                              path = NULL,
                              seed = NULL) {
  # ---- Input validation ----

  # Check dt is a data.table
  if (!inherits(dt, "data.table")) {
    stop("'dt' must be a data.table", call. = FALSE)
  }

  # Check required columns exist
  required_cols <- c(features, group, id, energy)
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(
      "The following columns are missing from 'dt': ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Check water_use_m3 column if provided
  if (!is.null(water_use_m3) && !water_use_m3 %in% names(dt)) {
    stop("Column '", water_use_m3, "' not found in 'dt'", call. = FALSE)
  }

  # Validate save/path arguments
  if (save) {
    if (is.null(path)) {
      stop("'path' must be provided when save = TRUE", call. = FALSE)
    }
    if (!dir.exists(path)) {
      stop("Directory '", path, "' does not exist", call. = FALSE)
    }
  }

  # Validate temperature parameters
  if (!is.numeric(t_cold) || !is.numeric(t_hot)) {
    stop("'t_cold' and 't_hot' must be numeric", call. = FALSE)
  }
  if (t_hot <= t_cold) {
    stop("'t_hot' must be greater than 't_cold'", call. = FALSE)
  }

  # ---- Validate data subsets ----

  # Check group values
  train_idx <- dt[[group]] == 1
  predict_idx <- dt[[group]] == 0

  n_train <- sum(train_idx, na.rm = TRUE)
  n_predict <- sum(predict_idx, na.rm = TRUE)

  if (n_train == 0) {
    stop("No training data found (group == 1)", call. = FALSE)
  }
  if (n_predict == 0) {
    stop("No prediction data found (group == 0)", call. = FALSE)
  }
  if (n_train < 10) {
    stop(
      "Insufficient training data for 10-fold CV. ",
      "Found ", n_train, " rows, need at least 10.",
      call. = FALSE
    )
  }
  # Validate seed parameter
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1) {
      stop("'seed' must be a single integer value", call. = FALSE)
    }
    set.seed(seed)
  }

  # ---- Register CVRMSE measure ----
  cvrmse_mlr3()


  # ---- Define training and prediction subsets ----
  train_dt_subset <- dt[train_idx, c(features, energy), with = FALSE]
  predict_dt_subset <- dt[predict_idx, c(features, energy), with = FALSE]

  # Create mlr3 task
  task <- mlr3::as_task_regr(train_dt_subset, target = energy, id = id)

  # Setup cross-validation
  resampling <- mlr3::rsmp("cv", folds = 10)

  # Configure Random Forest learner
  requireNamespace("mlr3learners", quietly = TRUE)
  rf_untuned <- mlr3::lrn("regr.ranger")
  rf_untuned$param_set$set_values(num.trees = 1000, seed = seed)
  rf_untuned$id <- "rf_untuned"

  # ---- Perform cross-validation ----
  cv <- mlr3::resample(
    task = task,
    learner = rf_untuned,
    resampling = resampling,
    store_backends = FALSE,
    store_models = FALSE
  )

  cv_result <- cv$aggregate(c(
    mlr3::msr("reg.cvrmse"),
    mlr3::msr("regr.bias"),
    mlr3::msr("regr.rmse"),
    mlr3::msr("regr.mae"),
    mlr3::msr("time_both")
  ))

  regression_result <- data.table::as.data.table(as.list(cv_result))
  regression_result[, (id) := dt[1, get(id)]]

  # ---- Train final model and predict ----
  rf_untuned$train(task)
  prediction_result <- rf_untuned$predict_newdata(predict_dt_subset)

  # ---- Calculate SH and DHW ----
  dt[predict_idx, sh_calculated := prediction_result$response]
  dt[, dhw_calculated := data.table::fifelse(
    is.na(sh_calculated),
    yes = 0,
    no = get(energy) - sh_calculated
  )]

  # ---- Apply plausibility checks ----
  if (is.null(water_use_m3)) {
    # Simple bounds check
    dt[, dhw_calculated := data.table::fcase(
      dhw_calculated > get(energy), get(energy),
      dhw_calculated < 0, 0,
      dhw_calculated <= get(energy) & dhw_calculated >= 0, dhw_calculated
    )]
  } else {
    # Thermodynamic plausibility check using water usage
    t_dif <- t_hot - t_cold

    TabvT <- IAPWS95::satTabvT(273.15 + t_cold, 273.15 + t_hot, t_dif) |>
      data.table::as.data.table()

    # Calculate maximum allowed energy in kWh
    dt[, dhw_max := (get(water_use_m3) / TabvT[1, vf]) *
      (TabvT[2, hf] - TabvT[1, hf]) / 3600]
    dt[, dhw_max := data.table::fifelse(
      dhw_max > get(energy),
      yes = get(energy),
      no = dhw_max
    )]

    # Apply thermodynamic bounds
    dt[, dhw_calculated := data.table::fcase(
      dhw_calculated > dhw_max, dhw_max,
      dhw_calculated < 0, 0,
      dhw_calculated <= dhw_max & dhw_calculated >= 0, dhw_calculated
    )]

    # Clean up temporary columns
    dt[, (water_use_m3) := NULL]
    dt[, dhw_max := NULL]
  }

  # Recalculate SH based on bounded DHW
  dt[, sh_calculated := get(energy) - dhw_calculated]

  # Remove working columns
  dt[, (features) := NULL]
  dt[, (energy) := NULL]
  dt[, (group) := NULL]

  # ---- Return or save results ----
  if (save) {
    fst::write_fst(
      dt,
      file.path(path, paste0("data_", dt[1, get(id)], ".fst")),
      compress = 100
    )
    data.table::fwrite(
      regression_result,
      file.path(path, paste0("per_", dt[1, get(id)], ".csv"))
    )
    return(invisible(NULL))
  } else {
    return(list(data = dt, performance = regression_result))
  }
}

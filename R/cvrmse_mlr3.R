#' CVRMSE Measure for mlr3
#'
#' @description
#' Internal function that creates and registers a CV(RMSE) (Coefficient of Variation
#' of the Root Mean Square Error) measure for use with mlr3 regression tasks.
#'
#' @details
#' CV(RMSE) is calculated as: RMSE / mean(truth)
#'
#' This measure is commonly used in building energy modeling to assess model
#' performance relative to the magnitude of the target variable.
#'
#' @return Invisibly returns NULL. The measure is registered in mlr3::mlr_measures
#'   with id "reg.cvrmse".
#'
#' @keywords internal
#' @noRd
cvrmse_mlr3 <- function() {
  # Only register if not already present

  if (!"reg.cvrmse" %in% names(mlr3::mlr_measures)) {
    reg_cvrmse <- R6::R6Class("reg.cvrmse",
      inherit = mlr3::MeasureRegr,
      public = list(
        initialize = function() {
          super$initialize(
            id = "reg.cvrmse",
            packages = character(),
            properties = character(),
            predict_type = "response",
            range = c(0, Inf),
            minimize = TRUE
          )
        }
      ),
      private = list(
        .score = function(prediction, ...) {
          cvrmse <- function(truth, response) {
            sqrt(mean((truth - response)^2)) / mean(truth)
          }
          cvrmse(prediction$truth, prediction$response)
        }
      )
    )

    mlr3::mlr_measures$add("reg.cvrmse", reg_cvrmse)
  }

  invisible(NULL)
}

#include <Rcpp.h>
#include <cmath> // for std::round, std::floor, std::ceil

using namespace Rcpp;

//' Round Time Values with Precision
//'
//' Rounds POSIXct or numeric time values to a specified precision using
//' various rounding methods. It does *not* work for rounding days because of timezone issues
//'
//' @param x Numeric or POSIXct vector of time values
//' @param precision Numeric value specifying rounding precision. Default is 1.0
//' @param method Character string specifying rounding method. Options are:
//'   \itemize{
//'     \item "round": Round to nearest (default)
//'     \item "floor": Round down
//'     \item "ceiling" or "ceil": Round up
//'   }
//'
//' @return Numeric vector of rounded values, preserving POSIXct class and
//'   timezone attributes if input was POSIXct
//'
//' @details
//' For time values, precision is typically in seconds. For example:
//' \itemize{
//'   \item precision = 1: Round to nearest second
//'   \item precision = 60: Round to nearest minute
//'   \item precision = 3600: Round to nearest hour
//' }
//'
//' @examples
//' \dontrun{
//' times <- as.POSIXct(c("2024-01-01 12:34:56", "2024-01-01 12:35:23"))
//' # Round to nearest minute
//' fast_round_time(times, precision = 60, method = "round")
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector fast_round_time(Rcpp::NumericVector x, double precision = 1.0, std::string method = "round") {
  Rcpp::NumericVector result(x.size());
  
  auto round_fn = [precision, &method](double val) -> double {
    if (!std::isfinite(val)) return val;
    double scaled = val / precision;
    if (method == "round") {
      return std::round(scaled) * precision;
    } else if (method == "floor") {
      return std::floor(scaled) * precision;
    } else if (method == "ceiling" || method == "ceil") {
      return std::ceil(scaled) * precision;
    } else {
      Rcpp::stop("Unsupported rounding method: " + method);
    }
  };
  
  std::transform(x.begin(), x.end(), result.begin(), round_fn);
  
  // Copy over POSIXct class and attributes
  result.attr("class") = x.attr("class");
  if (x.hasAttribute("tzone")) {
    result.attr("tzone") = x.attr("tzone");
  }
  
  return result;
}

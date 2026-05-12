#include <Rcpp.h>
using namespace Rcpp;

//' Run Length Encoding with Length per Element
//'
//' Computes run-length encoding but returns the length of each run for every
//' element in the input vector (instead of compressed run lengths).
//'
//' @param x Numeric vector to compute run lengths for
//'
//' @return Integer vector of same length as x, where each element contains
//'   the length of the run it belongs to
//'
//' @details
//' Unlike base R's \code{rle()}, this function returns a vector of the same
//' length as the input, with each position containing the length of its
//' corresponding run. Handles NA and NaN values correctly (consecutive NAs
//' are considered part of the same run).
//'
//' @examples
//' \dontrun{
//' x <- c(1, 1, 1, 2, 2, 3, 3, 3, 3)
//' rle2(x)
//' # Returns: c(3, 3, 3, 2, 2, 4, 4, 4, 4)
//' }
//'
//' @export
// [[Rcpp::export]]
IntegerVector rle2(NumericVector x) {
  int n = x.size();
  IntegerVector out(n);
  if (n == 0) return out;
  
  int run_start = 0;
  for (int i = 1; i <= n; ++i) {
    bool is_break = false;
    
    if (i == n) {
      is_break = true;
    } else {
      double curr = x[i];
      double prev = x[i - 1];
      
      // Handle NA/NaN comparison like R does
      if (R_IsNA(curr) && R_IsNA(prev)) {
        is_break = false; // consecutive NAs = same
      } else if (R_IsNaN(curr) && R_IsNaN(prev)) {
        is_break = false; // consecutive NaNs = same
      } else {
        is_break = (curr != prev);
      }
    }
    
    if (is_break) {
      int run_length = i - run_start;
      for (int j = run_start; j < i; ++j) {
        out[j] = run_length;
      }
      run_start = i;
    }
  }
  
  return out;
}

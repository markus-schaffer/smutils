#include <Rcpp.h>
using namespace Rcpp;


// Helper: pow wrapper for std::transform
struct pow_wrapper {
public:
  double operator()(double a, double b) {
    return ::pow(a, b);
  }
};

// Helper: element wise power for integer base and numeric exponent
NumericVector vecpow(const IntegerVector base, const NumericVector exp) {
  NumericVector out(base.size());
  std::transform(base.cbegin(), base.cend(), exp.cbegin(), out.begin(), pow_wrapper());
  return out;
}


//' @title Moving-average imputation with simple, linear, or exponential weighting
//' @description
//' Imputes `NA` values in a numeric vector using a local moving-average
//' based on a window of size `2*k + 1` centered around each missing position.
//' Three weighting schemes are supported:
//' \itemize{
//'   \item \code{"simple"}: unweighted mean of non-missing neighbors,
//'   \item \code{"linear"}: weights \eqn{1 / (|i - j| + 1)},
//'   \item \code{"exponential"}: weights \eqn{1 / 2^{|i - j|}}.
//' }
//'
//' For each \code{NA} entry, the method expands the window symmetrically by
//' increasing \code{k} as needed until at least two non-missing values are found,
//' then computes the (weighted) average of the available neighbors to impute.
//' Non-NA elements are left unchanged.
//'
//' @param x Numeric vector with possible \code{NA}s to impute.
//' @param k Integer, the initial half-window size (the window is \code{2*k + 1}).
//' Must be \code{\link[base]{>=} 0}.
//' @param weighting Character string, one of \code{"simple"}, \code{"linear"},
//' or \code{"exponential"}.
//'
//' @details
//' The algorithm considers indices \code{i - k, ..., i + k} clipped to \code{[0, length(x)-1]}
//' for each missing \code{x[i]}. If fewer than two non-missing elements are present
//' in this window, \code{k} is increased by one and the process is retried,
//' until the requirement is satisfied. The final imputed value is the mean of
//' the available neighbors under the chosen weighting scheme. Elements of \code{x}
//' that are not \code{NA} are returned unchanged.
//'
//' Linear weights are normalized by their sum over the non-missing neighbors.
//' Exponential weights use base 2 and are similarly normalized. When the window
//' contains \code{NA}s, their corresponding weight contributions are set to zero.
//'
//' @return A numeric vector of the same length as \code{x} where missing values
//' have been imputed according to the chosen method.
//'
//' @examples
//' # Simple weighting
//' ma(c(1, NA, 3, NA, 5), k = 1, weighting = "simple")
//'
//' # Linear weighting
//' ma(c(10, NA, NA, 40, 50), k = 1, weighting = "linear")
//'
//' # Exponential weighting
//' set.seed(1)
//' x <- c(rnorm(5), NA, rnorm(5))
//' ma(x, k = 2, weighting = "exponential")
//'
//' # No change to non-missing values
//' ma(c(1, 2, 3), k = 1, weighting = "simple")
//'
//' @export
// [[Rcpp::export]]
 Rcpp::NumericVector ma(NumericVector x, int k, String weighting) {
   Rcpp::NumericVector tempdata = clone(x);
   Rcpp::NumericVector out = clone(x);
   
   int n = tempdata.size();
   
   for (int i = 0; i < n; i++ ) {
     // check for interrupt every 1024 iterations
     if (i % 1024 == 0) {Rcpp::checkUserInterrupt();}
     
     // If Value is NA -> impute it based on selected method
     if (ISNAN(tempdata[i])) {
       int ktemp = k;
       IntegerVector usedIndices = seq(i - ktemp, i + ktemp);
       usedIndices = usedIndices[usedIndices >= 0];
       usedIndices = usedIndices[usedIndices < n];
       NumericVector t = tempdata[usedIndices];
       
       // Search for at least 2 non-NA values
       while (sum(!is_na(t)) < 2) {
         ktemp = ktemp + 1;
         usedIndices = seq(i - ktemp, i + ktemp);
         usedIndices = usedIndices[usedIndices >= 0];
         usedIndices = usedIndices[usedIndices < n];
         t = tempdata[usedIndices];
       }
       
       if (weighting =="simple") {
         // Calculate mean value
         NumericVector noNAs = wrap(na_omit(t));
         out[i] = mean(noNAs);
       } 
       else if(weighting == "linear") {
         // Calculate weights based on indices 1/(distance from current index+1)
         // Set weights where data is NA to 0
         // Sum up all weights (needed later) to norm it
         // Create weighted data (weights*data)
         // Sum up
         NumericVector weightsData = 1 / (abs(usedIndices - i) + 1);
         LogicalVector naCheck = !is_na(t);
         weightsData = weightsData * as<NumericVector>(naCheck);
         double sumWeights = sum(weightsData);
         NumericVector weightedData = (t * weightsData) / sumWeights;
         NumericVector noNAs = wrap(na_omit(weightedData));
         out[i] = sum(noNAs);
       } 
       else if (weighting == "exponential") {
         // Calculate weights based on indices 1/ 2 ^ (distance from current index)
         // Set weights where data is NA to 0
         // Sum up all weights (needed later) to norm it
         // Create weighted data (weights*data)
         // Sum up
         NumericVector expo = abs(usedIndices - i);
         IntegerVector base = Rcpp::rep(2, expo.size());
         NumericVector weightsData = 1 / (vecpow(base, expo));
         LogicalVector naCheck = !is_na(t);
         weightsData = weightsData * as<NumericVector>(naCheck);
         double sumWeights = sum(weightsData);
         NumericVector weightedData = (t * weightsData) / sumWeights;
         NumericVector noNAs = wrap(na_omit(weightedData));
         out[i] = sum(noNAs);
       } else {
         stop("Wrong input for parameter weighting. Has to be \"simple\",\"linear\" or \"exponential\"." );
       }
     }
   }
   
   return out;
 }
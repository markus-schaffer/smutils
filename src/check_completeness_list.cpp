#include <Rcpp.h>
#include <unordered_map>
using namespace Rcpp;

//' Check Completeness of String Lists Against Reference Vector
//'
//' Checks whether each character vector in a list contains only strings that
//' exist in a reference character vector, with optional uniqueness constraint.
//'
//' @param strg1_list List of character vectors to check
//' @param strg2 Character vector containing valid reference strings
//' @param require_unique Logical indicating whether strings in strg2 must be
//'   unique (appear exactly once). Default is TRUE.
//'
//' @return Logical vector of same length as strg1_list. Each element is:
//'   \itemize{
//'     \item TRUE if all strings in that list element exist in strg2 and meet
//'       uniqueness requirement
//'     \item FALSE if any string is not found or violates uniqueness constraint
//'     \item NA if the list element is NULL
//'   }
//'
//' @examples
//' \dontrun{
//' ref_strings <- c("a", "b", "c", "d")
//' test_list <- list(c("a", "b"), c("a", "e"), c("c"), NULL)
//' check_completeness_list(test_list, ref_strings)
//' }
//'
//' @export
// [[Rcpp::export]]
LogicalVector check_completeness_list(List strg1_list, CharacterVector strg2, bool require_unique = true) {
  // Create a frequency map for elements of strg2
  // Key: string, Value: number of times it appears in strg2
  std::unordered_map<std::string, int> freq_map;
  freq_map.reserve(strg2.size());  // Reserve space to reduce rehashing
  
  // Fill frequency map with elements from strg2
  for (R_xlen_t i = 0; i < strg2.size(); ++i) {
    freq_map[std::string(CHAR(strg2[i]))]++;
  }
  
  R_xlen_t n = strg1_list.size();  // Number of elements in the input list
  LogicalVector result(n);         // Output logical vector (same length as list)
  
  // Iterate over each element of strg1_list
  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP el = strg1_list[i];  // Extract the current list element
    
    // Handle NULL case explicitly
    if (Rf_isNull(el)) {
      result[i] = NA_LOGICAL;  // Mark as NA (could also use TRUE/FALSE)
      continue;
    }
    
    // Convert list element to a character vector
    CharacterVector query(el);
    bool valid = true;  // Assume valid until proven otherwise
    
    // Check each string in the query against the frequency map
    for (R_xlen_t j = 0; j < query.size(); ++j) {
      const char* s = CHAR(query[j]);  // Convert to C string
      auto it = freq_map.find(s);      // Look up in frequency map
      
      // Condition for invalidity:
      // If string not found → invalid
      if (it == freq_map.end()) {
        valid = false;
        break;
      }
      
      // If require_unique = true, also check for exactly one occurrence
      if (require_unique && it->second != 1) {
        valid = false;
        break;
      }
    }
    
    result[i] = valid;  
  }
  
  return result;
}

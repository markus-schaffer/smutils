#include <Rcpp.h>
#include <unordered_map>
#include <vector>
using namespace Rcpp;

//' Get Matching Values from Character Vectors
//'
//' For each character vector in a list, finds matching positions in a reference
//' character vector and returns corresponding values.
//'
//' @param strg1_list List of character vectors to search for
//' @param strg2 Character vector to search in (keys)
//' @param values Character vector of values corresponding to strg2
//'
//' @return List of same length as strg1_list. Each element contains:
//'   \itemize{
//'     \item Character vector of values from 'values' corresponding to matches in strg2
//'     \item NULL if the input list element was NULL
//'     \item Empty character vector if no matches found
//'   }
//'
//' @details
//' Creates a lookup map from strg2 to indices, then for each query string in
//' strg1_list elements, retrieves all corresponding values. If a string appears
//' multiple times in strg2, all corresponding values are returned.
//'
//' @examples
//' \dontrun{
//' keys <- c("a", "b", "c", "a")
//' vals <- c("1", "2", "3", "4")
//' queries <- list(c("a", "c"), c("b"), c("d"))
//' get_match_values(queries, keys, vals)
//' # Returns: list(c("1", "4", "3"), "2", character(0))
//' }
//'
//' @export
// [[Rcpp::export]]
List get_match_values(List strg1_list, CharacterVector strg2, CharacterVector values) {
  // Build a lookup map: string -> vector of all 1-based indices in strg2
  std::unordered_map<std::string, std::vector<int>> lookup;
  for (R_xlen_t i = 0; i < strg2.size(); ++i) {
    std::string s = as<std::string>(strg2[i]);
    lookup[s].push_back(i); // store 0-based for direct indexing into values
  }
  
  R_xlen_t n = strg1_list.size();
  List result(n);
  
  for (R_xlen_t i = 0; i < n; ++i) {
    if (Rf_isNull(strg1_list[i])) {
      result[i] = R_NilValue;  // Keep NULL
      continue;
    }
    
    CharacterVector query = strg1_list[i];
    std::vector<std::string> matched_values;
    
    for (R_xlen_t j = 0; j < query.size(); ++j) {
      std::string s = as<std::string>(query[j]);
      auto it = lookup.find(s);
      if (it != lookup.end()) {
        // For each matching index, push corresponding value
        for (auto idx : it->second) {
          matched_values.push_back(as<std::string>(values[idx]));
        }
      }
    }
    
    result[i] = wrap(matched_values); // convert std::vector<std::string> to R character vector
  }
  
  return result;
}

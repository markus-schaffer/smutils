#include <Rcpp.h>
#include <unordered_map>
#include <vector>
using namespace Rcpp;

//' Get Matching Values from List of Character Vectors
//'
//' Similar to \code{get_match_values} but searches within a list of character
//' vectors instead of a single character vector.
//'
//' @param strg1_list List of character vectors to search for (queries)
//' @param strg2 List of character vectors to search in (keys)
//' @param values Character vector of values corresponding to list elements in strg2
//'
//' @return List of same length as strg1_list. Each element contains:
//'   \itemize{
//'     \item Character vector of values corresponding to matches
//'     \item NULL if the input list element was NULL
//'     \item Empty character vector if no matches found
//'   }
//'
//' @details
//' Creates a lookup map from all strings in strg2 list elements to their list
//' indices. For each query, retrieves values corresponding to the list positions
//' where matches were found.
//'
//' @examples
//' \dontrun{
//' key_list <- list(c("a", "b"), c("c", "d"), NULL, c("e"))
//' vals <- c("group1", "group2", "group3", "group4")
//' queries <- list(c("a", "c"), c("e"), c("f"))
//' get_match_values_list(queries, key_list, vals)
//' # Returns: list(c("group1", "group2"), "group4", character(0))
//' }
//'
//' @export
// [[Rcpp::export]]
List get_match_values_list(List strg1_list, List strg2, CharacterVector values) {
  // Build a lookup map: string -> vector of list indices
  std::unordered_map<std::string, std::vector<int>> lookup;
  
  for (R_xlen_t i = 0; i < strg2.size(); ++i) {
    if (Rf_isNull(strg2[i])) continue;  // skip NULL
    CharacterVector group = strg2[i];
    for (R_xlen_t j = 0; j < group.size(); ++j) {
      std::string s = as<std::string>(group[j]);
      lookup[s].push_back(i); // store list index (0-based for values lookup)
    }
  }
  
  R_xlen_t n = strg1_list.size();
  List result(n);
  
  for (R_xlen_t i = 0; i < n; ++i) {
    if (Rf_isNull(strg1_list[i])) {
      result[i] = R_NilValue;
      continue;
    }
    
    CharacterVector query = strg1_list[i];
    std::vector<std::string> matched_values;
    
    for (R_xlen_t j = 0; j < query.size(); ++j) {
      std::string s = as<std::string>(query[j]);
      auto it = lookup.find(s);
      if (it != lookup.end()) {
        for (auto idx : it->second) {
          matched_values.push_back(as<std::string>(values[idx]));
        }
      }
    }
    
    result[i] = wrap(matched_values);
  }
  
  return result;
}

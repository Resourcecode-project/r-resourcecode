#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector ww_calc_cpp(NumericVector times, double winlen_hours, bool allow_overlap, double tstep_secs) {
  int n = times.size();
  if (n < 2) return NumericVector(0);

  std::vector<double> detected;

  int current_index = 0;   // was k
  int next_start_index = 0;         // for non-overlapping windows
  double winlen_secs = winlen_hours * 3600.0;

  while (current_index < n - 1) {
    double start_time = times[current_index];
    double current_time = start_time;

    // Assign next_time safely
    double next_time = times[current_index + 1];

    while ((next_time - current_time) <= tstep_secs && current_index < n - 1) {
      current_index += 1;

      if ((next_time - start_time) >= winlen_secs) {
        detected.push_back(start_time);
        break;
      }

      current_time = times[current_index];

      // update next_time safely
      if (current_index + 1 < n) next_time = times[current_index + 1];
      else break;
    }

    next_start_index += 1;
    current_index = allow_overlap ? current_index + 1 : next_start_index;
  }

  return wrap(detected);
}

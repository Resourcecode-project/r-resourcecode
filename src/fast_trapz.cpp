#include <RcppArmadillo.h>
using namespace Rcpp;

//' Fast implementation of pracma::trapz from the Armadillo C++ library
//'
//' Compute the area of a multivariate function with (matrix) values Y at the points x.
//'
//' @param x x-coordinates of points on the x-axis (vector)
//' @param y y-coordinates of function values (matrix)
//' @param dim an integer giving the subscripts which the function will be applied over. 1 indicates rows, 2 indicates columns
//' @export
// [[Rcpp::export]]
arma::vec fastTrapz( arma::vec X, arma::mat Y, int dim ) {
  arma::mat res   = trapz(X,Y,dim+1);
  return res;
}

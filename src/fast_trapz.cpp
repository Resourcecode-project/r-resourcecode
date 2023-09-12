#include <RcppArmadillo.h>
using namespace Rcpp;

//' Fast implementation of pracma::trapz from the Armadillo C++ library
//'
//' Compute the area of a multivariate function with (matrix) values Y at the points x.
//'
//' @param x x-coordinates of points on the x-axis (vector)
//' @param Y y-coordinates of function values (matrix)
//' @param dim an integer giving the subscripts which the function will be applied over. 1 indicates rows, 2 indicates columns
//' @export
//' @examples
//' x = 1:10
//' Y = sin(pi/10*matrix(1:10,ncol=10,nrow=10))
//' fastTrapz(x*pi/10,Y,2)
// [[Rcpp::export]]
arma::vec fastTrapz( arma::vec x, arma::mat Y, int dim ) {
  arma::mat res   = trapz(x,Y,dim-1);
  return res;
}

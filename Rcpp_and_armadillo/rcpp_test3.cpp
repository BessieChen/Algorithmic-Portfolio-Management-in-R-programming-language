// #include <Rcpp.h>
#include <RcppArmadillo.h>
#include <vector>
using namespace std;
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

////////////////////////////
// RcppArmadillo functions for test #3
////////////////////////////


// The function get_eigen() calculates the eigen_values 
// of the matrix cov_mat.
//' @export
// [[Rcpp::export]]
arma::vec get_eigen(const arma::mat& cov_mat) {
  arma::vec eig_vals = arma::eig_sym(cov_mat);
  return eig_vals;
}  // end get_eigen


// The function get_min_eigens() performs a loop over the 
// end_points, subsets the re_turns matrix, and calculates 
// the smallest eigenvalues as a function of the number of 
// time periods (rows) in the re_turns matrix.
//' @export
// [[Rcpp::export]]
arma::vec get_min_eigens(const arma::mat& re_turns, const IntegerVector& end_points) {
  arma::vec eig_vals(end_points.size());
  arma::mat sub_returns;
  // perform a loop over the end_points
  for (int i = 0; i < end_points.size(); i++) {
    // subset the returns
    arma::mat sub_returns = re_turns.rows(0, end_points[i]);
    // calculate the covariance matrix and its smallest eigenvalue
    eig_vals[i] = min(arma::eig_sym(cov(sub_returns)));
  }  // end for
  return eig_vals;
}  // end get_min_eigens



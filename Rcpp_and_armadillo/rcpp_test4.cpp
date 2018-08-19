// #include <Rcpp.h>
#include <RcppArmadillo.h>
#include <vector>
using namespace std;
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

////////////////////////////
// RcppArmadillo functions for test #4
////////////////////////////


// The function get_pca() calculates the PCA 
// of the matrix re_turns.
//' @export
// [[Rcpp::export]]
List get_pca(const arma::mat& re_turns) {
  arma::mat co_eff;
  arma::mat sco_re;
  arma::vec la_tent;
  arma::vec t_squared;
  
  arma::princomp(co_eff, sco_re, la_tent, t_squared, re_turns);
  
  return List::create(Named("coefficients") = co_eff,
                      Named("score")  = sco_re,
                      Named("latent")  = la_tent,
                      Named("tsquared")  = t_squared);
  
}  // end get_pca


// The function get_pca_var() performs a loop over the 
// end_points, subsets the re_turns matrix, and calculates 
// the PCA variances using eigen decomposition.
//' @export
// [[Rcpp::export]]
arma::mat get_pca_var(const arma::mat& re_turns, const IntegerVector& end_points) {
  arma::mat variance_s(re_turns.n_cols, end_points.size()-1);

  // perform a loop over the end_points
  for (arma::uword i = 1; i < end_points.size(); i++) {
    // subset the returns
    arma::mat sub_returns = re_turns.rows(end_points[i-1]+1, end_points[i]);
    // perform PCA using eigen decomposition
    variance_s.col(i-1) = arma::eig_sym(cor(sub_returns));
  }  // end for
  // return the variances
  return variance_s;
}  // end get_pca_var


// The function get_pca_varr() performs a loop over the 
// end_points, subsets the re_turns matrix, centers and 
// scales it, and calculates the PCA variances using 
// arma::princomp()
// The function get_pca_varr() produces the same output as 
// get_pca_var() but it's about 30 times slower!
//' @export
// [[Rcpp::export]]
arma::mat get_pca_varr(const arma::mat& re_turns, const IntegerVector& end_points) {
  arma::mat variance_s(re_turns.n_cols, end_points.size()-1);
  arma::mat co_eff;
  arma::mat sco_re;
  arma::vec la_tent;
  arma::vec t_squared;
  
  // perform a loop over the end_points
  for (arma::uword i = 1; i < end_points.size(); i++) {
    // subset the returns
    arma::mat sub_returns = re_turns.rows(end_points[i-1]+1, end_points[i]);
    // de-mean (center) and scale sub_returns
    for (arma::uword j = 0; j < sub_returns.n_cols; j++) {
      sub_returns.col(j) -= arma::mean(sub_returns.col(j));
      sub_returns.col(j) /= arma::stddev(sub_returns.col(j));
    }  // end for
    // perform PCA
    arma::princomp(co_eff, sco_re, la_tent, t_squared, sub_returns);
    variance_s.col(i-1) = la_tent;
  }  // end for
  // return the variances
  return variance_s;
}  // end get_pca_varr


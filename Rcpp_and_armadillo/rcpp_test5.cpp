// #include <Rcpp.h>
#include <RcppArmadillo.h>
#include <vector>
using namespace std;
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

////////////////////////////
// RcppArmadillo functions for test #5
////////////////////////////


// The function sharpe_weights() calculates the maximum 
// Sharpe ratio portfolio weights for the matrix re_turns.
//' @export
// [[Rcpp::export]]
arma::vec sharpe_weights(const arma::mat& re_turns) {
  arma::mat in_verse = arma::inv_sympd(cov(re_turns));
  arma::vec weight_s = arma::trans(arma::mean(re_turns, 0));

  weight_s = in_verse*weight_s;
  return weight_s/sum(weight_s);
}  // end sharpe_weights



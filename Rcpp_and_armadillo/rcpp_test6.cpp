// #include <Rcpp.h>
#include <RcppArmadillo.h>
#include <vector>
using namespace std;
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

////////////////////////////
// RcppArmadillo functions for test #6
////////////////////////////



// The function inv_reg() calculates the regularized inverse 
// of the covariance matrix, by truncating the number of 
// eigen-vectors to max_eigen.
//' @export
// [[Rcpp::export]]
arma::mat inv_reg(const arma::mat& re_turns, const arma::uword& max_eigen) {
  arma::mat eigen_vec;
  arma::vec eigen_val;
  
  arma::eig_sym(eigen_val, eigen_vec, cov(re_turns));
  eigen_vec = eigen_vec.cols(eigen_vec.n_cols-max_eigen, eigen_vec.n_cols-1);
  eigen_val = 1/eigen_val.subvec(eigen_val.n_elem-max_eigen, eigen_val.n_elem-1);
  // arma::mat eigen_valmat = diagmat(eigen_val);
  
  return eigen_vec*diagmat(eigen_val)*eigen_vec.t();
  
}  // end inv_reg


// The function sharpe_weights_reg() calculates the maximum 
// Sharpe ratio portfolio weights for the matrix re_turns.
// It uses the regularized inverse of the covariance matrix.
//' @export
// [[Rcpp::export]]
arma::vec sharpe_weights_reg(const arma::mat& re_turns, 
                             const double& al_pha, 
                             const arma::uword& max_eigen) {
  arma::mat in_verse = inv_reg(re_turns, max_eigen);
  arma::vec weight_s = arma::trans(arma::mean(re_turns, 0));
  arma::vec mean_s(weight_s.n_elem);
  mean_s.fill(arma::mean(weight_s));
  arma::vec alpha_s(weight_s.n_elem);
  alpha_s.fill(al_pha);
  arma::vec alphas_b(weight_s.n_elem);
  alphas_b.fill(1-al_pha);
  
  // shrink weight_s to the mean of weight_s
  weight_s = (alphas_b % weight_s + alpha_s % mean_s);
  // apply regularized inverse
  weight_s = in_verse*weight_s;
  return weight_s/sqrt(sum(square(weight_s)));
}  // end sharpe_weights_reg



// The function roll_portf() performs a loop over the 
// end_points, subsets the re_turns matrix, and calculates 
// the PCA variances using eigen decomposition.
//' @export
// [[Rcpp::export]]
arma::mat roll_portf(const arma::mat& ex_cess, // portfolio returns
                     const arma::mat& re_turns, // portfolio returns
                     const arma::uvec& start_points, 
                     const arma::uvec& end_points, 
                     const double& al_pha, 
                     const arma::uword& max_eigen) {
  arma::vec sre_turns = zeros(re_turns.n_rows);
  arma::vec weight_s(re_turns.n_cols);
  
  // sre_turns.subvec(1, 11) = ones<vec>(11);
  
  // perform a loop over the end_points
  for (arma::uword i = 1; i < end_points.size(); i++) {
    // subset the returns
    arma::mat sub_returns = ex_cess.rows(start_points[i-1], end_points[i-1]);
    // calculate portfolio weights
    weight_s = sharpe_weights_reg(sub_returns, al_pha, max_eigen);
    // sub_returns = re_turns.rows(end_points[i-1]+1, end_points[i]);
    sre_turns.subvec(end_points[i-1]+1, end_points[i]) = re_turns.rows(end_points[i-1]+1, end_points[i])*weight_s;
    // arma::mat foo = re_turns.rows(end_points[i-1]+1, end_points[i])*weight_s;
  }  // end for
  // return the strategy returns
  return sre_turns;
}  // end roll_portf



// The function garch_proc() simulates a GARCH model
//' @export
// [[Rcpp::export]]
NumericMatrix garch_proc(int len_gth, 
                         double om_ega, 
                         double al_pha, 
                         double be_ta, 
                         NumericVector r_norm) {
  NumericVector vari_ance(len_gth);
  NumericVector re_turns(len_gth);
  vari_ance[0] = om_ega/(1-al_pha-be_ta);
  re_turns[0] = sqrt(vari_ance[0])*r_norm[0];
  
  for (int i = 1; i < len_gth; i++) {
    re_turns[i] = sqrt(vari_ance[i-1])*r_norm[i];
    vari_ance[i] = om_ega + al_pha*pow(re_turns[i], 2) + be_ta*vari_ance[i-1];
  }
  return cbind(re_turns, vari_ance);
}  // end garch_proc


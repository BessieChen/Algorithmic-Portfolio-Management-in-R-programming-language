########################
### RcppArmadillo scripts
########################

#########
## Scripts for calling RcppArmadillo functions for manipulating vectors and matrices

# Compile Rcpp functions
Rcpp::sourceCpp(file="C:/Develop/R/scripts/armadillo_functions.cpp")


### sum_na() sum_if() conditional sums Rcpp functions

# Create synthetic data
vec_tor <- 1:100
vec_tor[sample(1:100, 5)] <- NA

sum(is.na(vec_tor))
sum_na(vec_tor)
sum_na_stl(vec_tor)

# Benchmark Rcpp sum_na() function
library(microbenchmark)
summary(microbenchmark(
  sum_na=sum_na(vec_tor),
  sum_na_stl=sum_na_stl(vec_tor),
  sum_is_na=sum(is.na(vec_tor)),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# microbenchmark shows: 
# sum(is.na()) is 5 times faster than Rcpp
#         expr    mean median
# 1     sum_na 3778.50   3910
# 2 sum_na_stl 3592.92   3422
# 3  sum_is_na  728.87    490

sum_if(vec_tor, 5)
sum_if_cpp(vec_tor, 5)
sum_if_stl(vec_tor, 5)
sum(vec_tor < 5)

summary(microbenchmark(
  sum_if_cpp=sum_if_cpp(vec_tor, 5),
  sum_if=sum_if(vec_tor, 5),
  sum_if_stl=sum_if_stl(vec_tor, 5),
  r_code=sum(vec_tor < 5),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# microbenchmark shows: 
# sum(vec_tor < 5) is over 2 times faster than Rcpp
#         expr    mean median
# 1 sum_if_cpp 2424.76   2444
# 2     sum_if 2419.90   2444
# 3 sum_if_stl 2185.26   1956
# 4     r_code 1056.44    978


### which() Rcpp functions

# Create synthetic data
vec_tor <- round(runif(16), 2)
mat_rix <- matrix(round(runif(16), 2), nc=4)
bool_ean <- sample(c(TRUE, rep(FALSE, 9)), size=1e3, replace=TRUE)

# whi_ch3(bool_ean)
all.equal(whi_ch3(bool_ean), whi_ch4(bool_ean))

# Benchmark Rcpp which functions
library(microbenchmark)
summary(microbenchmark(
  whi_ch32=whi_ch32(bool_ean),
  whi_ch33=whi_ch33(bool_ean),
  whi_ch34=whi_ch34(bool_ean),
  whi_ch=whi_ch(bool_ean),
  whi_ch2=whi_ch2(bool_ean),
  whi_ch4=whi_ch4(bool_ean),
  whi_ch3=whi_ch3(bool_ean),
  which=which(bool_ean),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# microbenchmark shows: which() is fastest followed by whi_ch3():
#      expr     mean median
# 1 whi_ch5 59.05335 57.181
# 2  whi_ch  7.16539  6.843
# 3 whi_ch2  8.05976  7.332
# 4 whi_ch4  4.17935  3.911
# 5 whi_ch3  3.32402  2.934
# 6   which  2.28303  2.444


### select elements and assign values

# sub-matrix Rcpp functions
sub_mat(mat_rix=mat_rix, row_num=c(1, 3), col_num=1:2)
sub_mat(mat_rix=mat_rix, row_num=1:2, col_num=1:2)
sub_mat_cast(mat_rix=mat_rix, row_num=1:2, col_num=1:2)


select_sub_mat(mat_rix=mat_rix, 0.4, 0)
find_sub_mat(mat_rix=mat_rix, 0.4, 0)

library(microbenchmark)
summary(microbenchmark(
  select_sub_mat=select_sub_mat(mat_rix=mat_rix, 0.4, 0),
  find_sub_mat=find_sub_mat(mat_rix=mat_rix, 0.4, 0),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# microbenchmark shows: both about the same
#             expr    mean median
# 1 select_sub_mat 3.21168  2.933
# 2   find_sub_mat 2.79620  2.445


# function to assign values to selected vector elements
sub_assign(vec_tor=vec_tor, in_dex=c(2, 4, 6), da_ta=c(3, 5, 7))
# function to find selected vector elements and to assign values
find_assign_vec(vec_tor=vec_tor, fi_nd=0.5, da_ta=0.1)
# function to find selected vector elements and to assign values
find_assign_vec_point(vec_tor=vec_tor, fi_nd=0.5, da_ta=0.1)
# Rcpp function to assign values to selected matrix elements
find_assign_mat(mat_rix=mat_rix, fi_nd=0.5, da_ta=1)
# Rcpp function to assign values to selected matrix elements
find_extract_mat(mat_rix=mat_rix, fi_nd=0.8)

library(microbenchmark)
summary(microbenchmark(
  in_place=find_assign_vec_point(vec_tor=vec_tor, fi_nd=0.5, da_ta=1),
  find_assign=find_assign_vec(vec_tor=vec_tor, fi_nd=0.5, da_ta=1),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# microbenchmark shows: 
# find_assign_vec_point() is slightly faster than find_assign_vec()
#          expr     mean  median
# 1    in_place 540.4299 543.465
# 2 find_assign 940.3258 815.197


# column compare Rcpp functions
compare_col(mat_rix=mat_rix, 0.5, 1)
compare_col_arma(mat_rix=mat_rix, 0.5)
compare_col_arma(mat_rix=mat_rix, 0.5, 1)
compare_col_armaa(mat_rix=mat_rix, 0.5, 1)

library(microbenchmark)
summary(microbenchmark(
  compare_col=compare_col(mat_rix=mat_rix, 0.5, 1),
  compare_col_arma=compare_col_arma(mat_rix=mat_rix, 0.5, 1),
  compare_col_armaa=compare_col_armaa(mat_rix=mat_rix, 0.5, 1),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# microbenchmark shows: compare_col() is fastest
#                expr    mean median
# 1       compare_col 1525.52   1466
# 2  compare_col_arma 2366.22   2444
# 3 compare_col_armaa 2024.09   1956

# which column Rcpp function
which_col(mat_rix=mat_rix, 0.5, 2)
which_col(mat_rix=mat_rix, 0.5)


#########
## Scripts for calling RcppArmadillo functions for matrix algebra

## de-mean the columns of a matrix

summary(microbenchmark(
  demean_mat=demean_mat(mat_rix),
  demean_arma=demean_arma(mat_rix),
  apply=(apply(mat_rix, 2, mean)),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# microbenchmark shows: 
# demean_mat() is over 5 times faster than demean_arma()
# and over 20 times faster than apply()
#           expr      mean    median
# 1   demean_mat  1.206325  1.188584
# 2  demean_arma  9.909479  5.964911
# 3        apply 44.555462  25.05482


## bind the columns of two matrices

mat_rix1 <- matrix(runif(1e6), nc=1e3)
mat_rix2 <- matrix(runif(1e6), nc=1e3)
# cbind(mat_rix1, mat_rix2)
all.equal(cbind_rcpp(mat_rix1, mat_rix2), cbind_arma(mat_rix1, mat_rix2))

summary(microbenchmark(
  cbind_arma=cbind_arma(mat_rix1, mat_rix2),
  cbind_rcpp=cbind_rcpp(mat_rix1, mat_rix2),
  cbind=cbind(mat_rix1, mat_rix2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# microbenchmark shows: 
# cbind_rcpp() is as fast as cbind(), and more that 2 times faster than
# cbind_arma().
#         expr      mean    median
# 1 cbind_arma 12.893332 12.414150
# 2 cbind_rcpp  5.943275  4.813715
# 3      cbind  5.829133  4.906573


## calculate the inner (dot) product of two vectors.

vec1 <- runif(1e5)
vec2 <- runif(1e5)

vec_in(vec1, vec2)
vec1 %*% vec2

summary(microbenchmark(
  vec_in=vec_in(vec1, vec2),
  r_code=(vec1 %*% vec2),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# microbenchmark shows: 
# vec_in() is several times faster than %*%, especially for longer vectors.
#       expr     mean   median
# 1   vec_in 110.7067 110.4530
# 2   r_code 585.5127 591.3575


## calculate the product of a matrix times a vector.

mat_rix <- matrix(runif(1e7), nc=1e5)
vec1 <- runif(1e5)
all.equal(mat_rix %*% vec1, mat_vec_in(vec_tor=vec1, mat_rix=mat_rix))

summary(microbenchmark(
  mat_vec_in=mat_vec_in(vec_tor=vec1, mat_rix=mat_rix),
  r_code=(mat_rix %*% vec1),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# microbenchmark shows: 
# mat_vec_in() is 3 times faster than %*%, for matrix with 100,000 columns.
#        expr      mean    median
# 1 mat_vec_in  7.299448  7.180375
# 2    r_code 21.133891 21.048730


vec2 <- runif(1e2)
all.equal(drop(vec2 %*% (mat_rix %*% vec1)), mat_2vec_in(vec2, mat_rix, vec1))

summary(microbenchmark(
  mat_2vec_in=mat_2vec_in(vec2, mat_rix, vec1),
  r_code=(vec2 %*% (mat_rix %*% vec1)),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# microbenchmark shows: 
# mat_2vec_in() is 3 times faster than %*%, for matrix with 100,000 columns.
#            expr      mean    median
# 1   mat_2vec_in  7.138696  7.071877
# 2        r_code 20.826379 20.678520


## calculate product of matrix and vectors
# multiply the matrix elements *by* the vector elements

vec1 <- runif(NROW(mat_rix))
prod_uct <- vec1 * mat_rix
mat_vec_by(vec_tor=vec1, mat_rix=mat_rix)
all.equal(prod_uct, mat_rix)

summary(microbenchmark(
  mat_vec_by=mat_vec_by(vec_tor=vec1, mat_rix=mat_rix),
  r_code=(vec1 * mat_rix),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# microbenchmark shows: 
# mat_vec_by() is slightly slower than %*%, for matrix with 100,000 columns.
#        expr     mean   median
# 1 mat_vec_by 44.90964 44.69138
# 2    r_code 32.89803 25.79500


# multiply the matrix elements *by* the elements of two vectors

mat_rix <- matrix(runif(1e7), nc=1e5)
vec1 <- runif(NCOL(mat_rix))
vec2 <- runif(NROW(mat_rix))
prod_uct <- t(t(vec2*mat_rix)*vec1)
mat_2vec_by(vec2, mat_rix, vec1)
all.equal(mat_rix, prod_uct)

summary(microbenchmark(
  mat_2vec_by=mat_2vec_by(vec2, mat_rix, vec1),
  mat_2vec_rcpp_by=mat_2vec_rcpp_by(vec2, mat_rix, vec1),
  mat_2vec_rcpp_by2=mat_2vec_rcpp_by2(vec2, mat_rix, vec1),
  r_code=(t(t(vec2*mat_rix)*vec1)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# microbenchmark shows: 
# mat_2vec_by() is over 2 times faster than %*% and t(), for matrix
# with 100,000 columns.
#                expr      mean    median
# 1       mat_2vec_by  73.65367  73.50842
# 2  mat_2vec_rcpp_by 101.39165 100.44875
# 3 mat_2vec_rcpp_by2 612.48159 612.98899
# 4            r_code 182.74140 174.80584



## matrix inversion

# create random positive semi-definite matrix
mat_rix <- matrix(runif(25), nc=5)
mat_rix <- t(mat_rix) %*% mat_rix

matrix_inv <- solve(mat_rix)
matrix_inv <- invspd_arma(mat_rix)
matrix_inv <- invspd_rcpp(mat_rix)

library(microbenchmark)
summary(microbenchmark(
  inv_mat=inv_mat(mat_rix),
  invspd_arma=invspd_arma(mat_rix),
  invspd_rcpp=invspd_rcpp(mat_rix),
  solve=solve(mat_rix),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# microbenchmark shows: 
# inv_mat() is over 10 times faster than solve()
# invspd_arma() is over 7 times faster than solve()
#                expr     mean median
# 1           inv_mat  3.42669  2.933
# 2       invspd_arma  4.68759  3.911
# 3       invspd_rcpp  4.74625  3.911
# 4             solve 32.00254 31.280


## linear regression
# define explanatory variable
len_gth <- 100
n_var <- 5
ex_plain <- matrix(rnorm(n_var*len_gth), nc=n_var)
# calculate de-meaned ex_plain matrix
explan_zm <- t(t(ex_plain) - colSums(ex_plain)/len_gth)
noise <- rnorm(len_gth, sd=0.5)
# response equals linear form plus error terms
weight_s <- rnorm(n_var)
res_ponse <- -3 + ex_plain %*% weight_s + noise
# calculate de-meaned response vector
response_zm <- res_ponse - mean(res_ponse)

# multivariate regression using lm()
reg_model <- lm(res_ponse ~ ex_plain)
coef(reg_model)

# multivariate regression using lm_arma()
reg_model <- lm_arma(res_ponse=res_ponse, ex_plain=ex_plain)
reg_model_sum <- summary(reg_model)
reg_model_sum$coefficients

# library(MASS)
# multivariate regression using MASS::ginv() in lm_r()
lm_r <- function(res_ponse, ex_plain) {
  # solve for regression betas
  ex_plain <- cbind(rep(1, NROW(ex_plain)), ex_plain)
  beta_s <- MASS::ginv(ex_plain) %*% res_ponse
  fit_ted <- drop(ex_plain %*% beta_s)
  # calculate residuals
  resid_uals <- drop(res_ponse - fit_ted)
  # variance of residuals
  deg_free <- len_gth-NCOL(ex_plain)
  resid_var <- sum(resid_uals^2)/deg_free
  # explanatory matrix squared
  explain_squared <- crossprod(ex_plain)
  # calculate covariance matrix of betas
  beta_covar <- resid_var*MASS::ginv(explain_squared)
  beta_sd <- sqrt(diag(beta_covar))
  # calculate t-values of betas
  beta_tvals <- drop(beta_s)/beta_sd
  # calculate two-sided p-values of betas
  beta_pvals <- 2*pt(-abs(beta_tvals), df=deg_free)
  cbind(beta_s, beta_sd, beta_tvals, beta_pvals)
}  # end lm_r
lm_r(res_ponse, ex_plain)


library(microbenchmark)
summary(microbenchmark(
  lm_arma=lm_arma(res_ponse, ex_plain),
  lm_r=lm_r(res_ponse, ex_plain),
  lm=lm(res_ponse ~ ex_plain),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# microbenchmark shows: 
# lm_arma() is over 10 times faster than lm() and over 3 times faster than
# lm_r().
#      expr       mean    median
# 1 lm_arma   99.31485   98.4795
# 2    lm_r  328.17102  324.5155
# 3      lm 1070.44432 1036.8345



## split-apply-combine procedure

# Create synthetic data
vec_tor <- sample(1:5, 1e3, replace=TRUE)
fac_tor <- sample(1:5, 1e3, replace=TRUE)
mat_rix <- matrix(runif(2e3), nc=2)
mat_rix <- cbind(vec_tor, mat_rix)

# The function tapply_arma() performs aggregations over a vector using a factor.
# It produces the same result as the R code: 
#   tapply(X=vec_tor, INDEX=fac_tor, FUN=NROW)

tapply_arma(vec_tor, fac_tor)
tapply(X=vec_tor, INDEX=fac_tor, FUN=NROW)
all.equal(drop(tapply_arma(vec_tor, fac_tor)), as.numeric(tapply(X=vec_tor, INDEX=fac_tor, FUN=NROW)))

summary(microbenchmark(
  tapply_arma=tapply_arma(vec_tor, fac_tor),
  tapply=tapply(X=vec_tor, INDEX=fac_tor, FUN=NROW),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# microbenchmark shows: 
# tapply_arma() is almost 4 times faster than tapply(), for vector with 1,000
# elements, but it loses its advantage for longer vectors.
#          expr      mean   median
# 1 tapply_arma  45.19329  44.4750
# 2      tapply 178.35619 173.0095



# The function apply_agg() performs aggregations over a matrix using its
# first column as a factor.
# It produces the same result as the R code: 
#     sapply(X=unique(mat_rix[, 1]), FUN=function(mat_rix[, -1]))

tapply(X=mat_rix[, 2], INDEX=mat_rix[, 1], FUN=mean)

all.equal(sort(apply_agg(mat_rix)), 
          sort(sapply(X=unique(mat_rix[, 1]), FUN=function(x) {
            foo <- mat_rix[which(mat_rix[, 1] == x), -1,  drop=FALSE]
            sum(apply(foo, 1, prod))
            # sum(foo)
          })))

summary(microbenchmark(
  apply_agg=apply_agg(mat_rix),
  sapply=sapply(X=unique(mat_rix[, 1]), FUN=function(x) {
    foo <- mat_rix[which(mat_rix[, 1] == x), -1,  drop=FALSE]
    sum(apply(foo, 1, prod))
    # sum(foo)
  }),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary

# microbenchmark shows: 
# apply_agg() is over 40 times faster than sapply(), for matrix with 1,000 
# rows.
#        expr       mean   median
# 1 apply_agg   66.95125   54.494
# 2    sapply 2433.73195 2351.748


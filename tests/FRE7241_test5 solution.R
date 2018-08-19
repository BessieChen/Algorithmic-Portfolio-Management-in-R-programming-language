#################################
### FRE7241 Test #5 Solutions March 6, 2018
#################################
# Max score 190pts

# The below solutions are examples,
# Slightly different solutions are also possible.

############## Part I
# Summary: Calculate the minimum variance portfolio weights
# using matrix algebra and quadratic programming. 

# 1. (20pts) 
# Create a vector of strings called sym_bols, with all the 
# symbols in rutils::env_etf$re_turns except for "VXX".
# You can't simply type the symbol strings, but you must 
# use R code.
# You can use the function colnames() and the "!" operator.

sym_bols <- colnames(rutils::env_etf$re_turns)
sym_bols <- sym_bols[!(sym_bols=="VXX")]
n_weights <- NROW(sym_bols)

# You should get the following output:
# > sym_bols
#  [1] "VTI" "VEU" "IEF" "VNQ" "DBC" "XLY" "XLP" "XLE" "XLF" "XLV"
# [11] "XLI" "XLB" "XLK" "XLU" "VYM" "IVW" "IWB" "IWD" "IWF"


# Extract the columns of rutils::env_etf$re_turns for 
# the sym_bols, into an xts called re_turns.
# Remove NA values from re_turns by first carrying forward 
# non-NA re_turns, and then removing any remaining NAs.
# You can use the functions zoo::na.locf() and na.omit().

re_turns <- rutils::env_etf$re_turns[, sym_bols]
re_turns <- zoo::na.locf(re_turns)
re_turns <- na.omit(re_turns)

# You should get the following output:
# > dim(re_turns)
# [1] 2741   19
# 
# > round(re_turns[1:5, 1:5], 3)
#               VTI    VEU    IEF    VNQ    DBC
# 2007-03-09  0.000  0.007 -0.006  0.013 -0.013
# 2007-03-12  0.003  0.005  0.003  0.005 -0.009
# 2007-03-13 -0.018 -0.026  0.004 -0.024 -0.008
# 2007-03-14  0.005  0.001 -0.001  0.005  0.002
# 2007-03-15  0.004  0.008 -0.001  0.009 -0.006


# Calculate the covariance matrix of re_turns and call 
# it co_var, and its inverse and call it in_verse.
# You can use the functions cov() and solve().

co_var <- cov(re_turns)
in_verse <- solve(a=co_var)

# Calculate the minimum variance weights using matrix 
# algebra, with the constraint that the sum of weights  
# is equal 1.

u_nit <- rep(1, NCOL(co_var))
weight_s <- in_verse %*% u_nit
weight_s <- weight_s / drop(t(u_nit) %*% weight_s)


# You should get the following output:
# > round(drop(weight_s), 3)
#    VTI    VEU    IEF    VNQ    DBC    XLY    XLP    XLE    XLF    XLV 
# -0.104 -0.057  0.688 -0.051  0.100 -0.015  0.088 -0.061 -0.019  0.006 
#   XLI    XLB    XLK    XLU    VYM    IVW    IWB    IWD    IWF 
# 0.025 -0.012 -0.008 -0.035  0.141 -0.026  0.188  0.042  0.109

# Calculate the variance of the minimum variance portfolio.

t(weight_s) %*% co_var %*% weight_s
# or
1/(t(u_nit) %*% in_verse %*% u_nit)

# You should get the following output:
#             [,1]
# [1,] 8.717695e-06


# 2. (20pts) 
# Calculate the minimum variance portfolio weights
# using the function quadprog::solve.QP(), to 
# demonstrate that they are the same as from p.1.

op_tim <- quadprog::solve.QP(Dmat=2*co_var, 
                             dvec=numeric(n_weights), 
                             Amat=matrix(1, nr=n_weights, nc=1), 
                             bvec=1)

# You should get the following output:
# > all.equal(unname(drop(weight_s)), op_tim$solution)
# [1] TRUE


# 3. (20pts) 
# Repeat the calculations of p.1 and p.2, and 
# calculate the minimum variance portfolio weights
# using the function quadprog::solve.QP(), but with 
# the additional constraints that the weights are 
# constrained between -1 and +1, and that the sum 
# of weights for a sub-portfolio are equal to 0.8.
# hint: You can adapt code from the slide "Portfolio 
# Optimization Using Package quadprog".

# Define the sub-portfolio symbols:

symbols_sub <- c("XLF", "XLE", "XLB")

# Calculate a Boolean vector called portfolio_sub
# which is TRUE if sym_bols is in symbols_sub.
# You can use the %in% operator.

portfolio_sub <- sym_bols %in% symbols_sub

# You should get the following output:
# > portfolio_sub
#  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE TRUE TRUE FALSE FALSE
# [12] TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

# Add the constraint that the sum of the symbols_sub weights 
# is equal to 0.8.

a_mat <- cbind(matrix(1, nr=n_weights, nc=1), 
               portfolio_sub*matrix(1, nr=n_weights, nc=1), 
               diag(n_weights), -diag(n_weights))
b_vec <- c(1, 0.8, rep(-1, n_weights), rep(-1, n_weights))
op_tim <- quadprog::solve.QP(Dmat=2*co_var, 
                             dvec=numeric(n_weights), 
                             Amat=a_mat, 
                             bvec=b_vec, 
                             meq=2)

weight_s <- op_tim$solution

# You should get the following output:
# > round(weight_s, 3)
#  [1] -0.499 -0.155  0.779 -0.083  0.022  0.117  0.314  0.219  0.368  0.185
# [11]  0.077  0.213  0.190  0.069 -0.221 -0.105 -0.189 -0.471  0.171
# 
# > sum(portfolio_sub*weight_s)
# [1] 0.8
# 
# > sum(weight_s)
# [1] 1

# Calculate the variance of the minimum variance portfolio.
t(weight_s) %*% co_var %*% weight_s
# or
1/(t(u_nit) %*% in_verse %*% u_nit)

# You should get the following output:
#             [,1]
# [1,] 1.84783e-05



############## Part II
# Summary: Calculate the maximum Sharpe ratio portfolio 
# weights using matrix algebra and RcppArmadillo.
# You can use the re_turns and in_verse matrices from 
# Part I above.

# 1. (20pts) Calculate the xts series of re_turns and 
# the in_verse of the covariance matrix of re_turns, 
# if you haven't done it already.

# Calculate the covariance matrix of re_turns and call 
# it co_var, and its inverse and call it in_verse.
# You can use the functions cov() and solve().

co_var <- cov(re_turns)
in_verse <- solve(a=co_var)


# Calculate the vector of average daily excess returns 
# called ex_cess.
# risk_free is the daily risk-free rate.

risk_free <- 0.03/260
ex_cess <- re_turns - risk_free
ex_cess <- colMeans(ex_cess)

# Calculate the maximum Sharpe ratio portfolio weights.

weight_s <- in_verse %*% ex_cess
weight_s <- drop(weight_s/sum(weight_s))

# You should get the following output:
# > round(weight_s, 3)
#    VTI    VEU    IEF    VNQ    DBC    XLY    XLP    XLE    XLF    XLV 
# -0.011 -0.340  0.596 -0.038 -0.116  0.137  0.268  0.196  0.069  0.197 
#   XLI    XLB    XLK    XLU    VYM    IVW    IWB    IWD    IWF 
# 0.201  0.024  0.229 -0.065  0.367  0.099 -0.424 -0.814  0.426


# Calculate the Sharpe ratio of the weighted portfolio returns, 
# assuming 260 business days in a year.

sqrt(260)*sum(weight_s * ex_cess) / 
  sqrt(drop(weight_s %*% co_var %*% weight_s))

# You should get the following output:
# [1] 1.246343


# 2. (20pts) Create an objective function equal to minus 
# the Sharpe ratio.
# The objective function should accept the arguments:
#  weights: the portfolio weights,
#  ex_cess: a vector of average excess returns,
#  co_var: the covariance matrix,

object_ive <- function(weight_s, ex_cess, co_var) {
  -sum(weight_s * ex_cess) / sqrt(drop(weight_s %*% co_var %*% weight_s))
}  # end object_ive

# Perform portfolio optimization using the function optim(), 
# and calculate the maximum Sharpe ratio portfolio weights
# to demonstrate that they are close to the weights obtained 
# from matrix inversion. 

op_tim <- optim(par=rep(1.0, n_weights),
                fn=object_ive,
                method="L-BFGS-B",
                upper=rep(10, n_weights),
                lower=rep(-10, n_weights),
                ex_cess=ex_cess,
                co_var=co_var)

weight_s <- op_tim$par/sum(op_tim$par)

# You should get output similar to the following:
# > round(weight_s, 3)
#  [1] -0.017 -0.339  0.596 -0.038 -0.116  0.136  0.267  0.196  0.068  0.197
# [11]  0.201  0.024  0.228 -0.066  0.366  0.096 -0.416 -0.812  0.428


# 3. (30pts) 
# Create an RcppArmadillo function called sharpe_weights(), 
# which calculates the maximum Sharpe ratio portfolio weights, 
# and produces the same result as p.1 above.
# The function sharpe_weights() should accept a single argument, 
# an xts time series of returns called re_turns.
# sharpe_weights() should calculate the covariance matrix of 
# re_turns and its inverse.
# sharpe_weights() should calculate and return a vector of the 
# maximum Sharpe ratio portfolio weights, using matrix algebra 
# in RcppArmadillo.
# Save the Rcpp code in a file called rcpp_test5.cpp,
# and compile it as follows:

Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/rcpp_test5.cpp")

# Calculates the weights using sharpe_weights() and call them
# weights_arma.

weights_arma <- sharpe_weights(re_turns-risk_free)

# You should get the following result:
# > all.equal(as.vector(weights_arma), unname(weight_s))
# [1] TRUE



############## Part III
# Summary: Perform a rolling portfolio optimization 
# over annual intervals, calculate optimized portfolio 
# weights in each year, and apply them to out-of-sample 
# returns in the following year.

# 1. (20pts) Calculate the xts series of re_turns (if 
# you haven't done it already).

# Calculate a vector of annual end points from
# the index of re_turns, and call it end_points.
# Use function rutils::calc_endpoints().

end_points <- rutils::calc_endpoints(re_turns, inter_val="years")

# Define starting points as the one-year (single period) 
# lag of end_points.

len_gth <- NROW(end_points)
start_points <- c(1, end_points[1:(len_gth-1)])

# You should get the following output:
# > end_points
# [1] 206  459  711  963 1215 1465 1717 1969 2221 2473 2724 2741
# 
# > start_points
# [1] 1  206  459  711  963 1215 1465 1717 1969 2221 2473 2724


# 2. (20pts) Define the daily risk-free rate as risk_free:

risk_free <- 0.03/260

# Calculate the xts series of excess daily returns called 
# ex_cess, by subtracting risk_free from the re_turns.

ex_cess <- re_turns - risk_free


# Perform an sapply() loop over the range 1:(NROW(end_points)-1), 
# and inside the loop, first subset the ex_cess returns 
# from start_points[i] to end_points[i], and then
# calculate the maximum Sharpe ratio portfolio weights 
# using matrix algebra.
# Call the matrix returned by sapply() weight_s.
# You can use functions sapply(), solve(), cov(), 
# colMeans(), drop(), sum(), and an anonymous function,

weight_s <- sapply(1:(NROW(end_points)-1),
                   function(i) {
                     # subset the ex_cess returns
                     ex_cess <- ex_cess[start_points[i]:end_points[i], ]
                     in_verse <- solve(cov(ex_cess))
                     # calculate the maximum Sharpe ratio portfolio weights.
                     weight_s <- in_verse %*% colMeans(ex_cess)
                     weight_s <- drop(weight_s/sum(weight_s))
                   }  # end anonymous function
)  # end sapply

# You should get the following output:
# > round(weight_s, 2)
#      [,1]   [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10] [,11]
# VTI  0.05  -4.61 -1.51  0.72 -0.09 -1.72  2.34 -1.43  3.66 -1.11 -3.60
# VEU  0.06  -5.85 -1.11 -0.33 -0.29  0.19 -1.16 -0.95 -0.05 -1.54  0.32
# IEF  0.53 -21.09  2.84  0.67  0.55  0.73  0.21  1.59  0.37  0.10  0.08
# VNQ -0.01  -8.44 -0.05  0.02  0.06  0.01 -0.76  0.53 -0.22  0.34 -0.01
# DBC  0.01  10.18 -0.57  0.05  0.04  0.03 -0.62 -1.60  0.82  0.50  0.22
# XLY -0.57  -8.88 -1.00  0.33  0.26  0.47  1.15 -0.49 -1.94  0.83  0.07
# XLP  0.40  -4.82 -0.56  0.25  0.10 -0.01  0.06 -0.04 -0.70  0.17  0.01
# XLE  0.18 -17.31  1.27  0.21  0.31 -0.25 -0.01  0.61 -0.70  0.42 -0.04
# XLF -0.21  -2.04 -1.68 -0.14 -0.01  0.19 -1.07  0.30 -0.45  2.28  0.52
# XLV  0.11  -2.67 -2.66 -0.51  0.24  0.35  0.63  0.20 -0.86  0.68  0.01
# XLI  0.40   6.36  0.81  0.23  0.15 -0.08  0.60 -0.23  0.66  1.73  0.08
# XLB -0.03   4.10 -1.81 -0.21 -0.05 -0.13 -0.14 -0.13 -0.34 -0.13  0.07
# XLK  0.04   5.38 -3.47 -0.58  0.35  0.09  0.88  0.69 -1.52  5.97 -0.68
# XLU -0.03   3.60 -0.76 -0.39  0.19 -0.32 -0.06  0.03  0.28  1.03  0.11
# VYM  0.15   8.60  6.74  1.01  0.53 -0.35  0.97 -0.63 -2.58  1.40  1.10
# IVW -0.21   6.35  2.78  0.25  0.85  0.61 -1.07  1.26  2.57 -8.06 -0.98
# IWB  0.21  22.91 -0.77 -0.60 -0.56  1.27 -1.03  0.36  0.74 -1.24  2.79
# IWD -0.37  -5.29  4.08 -0.44 -0.27  0.85  1.85  2.30  2.77 -2.29 -1.81
# IWF  0.31  14.53 -1.58  0.45 -1.38 -0.94 -1.77 -1.38 -1.51 -0.08  2.76


# 3. (20pts) Perform an lapply() loop over the range 
# 2:NROW(end_points), and inside the loop apply the 
# lagged weight_s to the columns of the subset re_turns, 
# to calculate the out-of-sample portfolio returns.
# hint: subset the re_turns by the interval 
# (end_points[i-1]+1):end_points[i]
# Inside the loop coerce the portfolio returns to an 
# xts series and return it, to produce a list of xts 
# called portf_rets.
# You can use functions lapply(), xts(), index(),
# and an anonymous function,

portf_rets <- lapply(2:NROW(end_points),
                     function(i) {
                       # subset the ex_cess returns
                       re_turns <- re_turns[(end_points[i-1]+1):end_points[i], ]
                       # calculate the out-of-sample portfolio returns
                       xts(re_turns %*% weight_s[, i-1], index(re_turns))
                     }  # end anonymous function
)  # end lapply

# Flatten portf_rets into a single xts series.
# You can use functions rutils::do_call() and rbind(),

portf_rets <- rutils::do_call(rbind, portf_rets)
colnames(portf_rets) <- "portf_rets"

# You should get the following output:
# dim(portf_rets)
# [1] 2535    1
# 
# > tail(portf_rets, 11)
#               portf_rets
# 2018-01-10  0.0006433005
# 2018-01-11  0.0006631701
# 2018-01-12  0.0097569820
# 2018-01-16 -0.0014937710
# 2018-01-17  0.0064484704
# 2018-01-18 -0.0001588550
# 2018-01-19  0.0047600417
# 2018-01-22  0.0011479045
# 2018-01-23  0.0071458638
# 2018-01-24  0.0064285156
# 2018-01-26  0.0134055880

# The portf_rets are percentage returns, because re_turns
# are percentage returns.

# Calculate the Sharpe ratio of the weighted portfolio returns, 
# assuming 260 business days in a year.

sqrt(260)*(mean(portf_rets)-risk_free) / sd(portf_rets)

# You should get the following output:
# [1] 0.5204891

# Compound portf_rets into a single xts series of compounded 
# cumulative returns.
# Use functions cumsum() and exp(),

portf_rets <- exp(cumsum(portf_rets))

# You should get the following output:
# > tail(portf_rets, 11)
#            portf_rets
# 2018-01-10   4359.759
# 2018-01-11   4362.651
# 2018-01-12   4405.425
# 2018-01-16   4398.850
# 2018-01-17   4427.307
# 2018-01-18   4426.604
# 2018-01-19   4447.725
# 2018-01-22   4452.833
# 2018-01-23   4484.767
# 2018-01-24   4513.690
# 2018-01-26   4574.606


# Plot portf_rets using quantmod::chart_Series().

quantmod::chart_Series(portf_rets, 
  name="Cumulative Returns of Max Sharpe Portfolio Strategy")

# Your plot should be similar to backtest_sharpe.png




#################################
### FRE7241 Test #6 Solutions March 24, 2018
#################################
# Max score 310pts

# The below solutions are examples,
# Slightly different solutions are also possible.


############## Part I
# Summary: Calculate efficient portfolios with the 
# lowest variance given a fixed target return.
# Plot the efficient frontier and the capital 
# market lines for different values of the fixed 
# target return.

# 1. (20pts) Create a vector of symbol names called 
# sym_bols. 
# Create a named vector of initial portfolio weights 
# for sym_bols, all equal to 1/3, and call it weight_s. 
# The names of weight_s should be equal to sym_bols. 
# You can use functions rep() and names(). 

library(HighFreq)
sym_bols <- c("VTI", "IEF", "DBC")
n_weights <- NROW(sym_bols)
weight_s <- rep(1/n_weights, n_weights)
names(weight_s) <- sym_bols

# You should get the following output:
# > weight_s
#       VTI       IEF       DBC 
# 0.3333333 0.3333333 0.3333333

# Extract the columns of rutils::env_etf$re_turns for 
# the sym_bols, into an xts called re_turns.
# Remove NA values from re_turns by first carrying forward 
# non-NA re_turns, and then removing any remaining NAs.
# You can use the functions zoo::na.locf() and na.omit().

re_turns <- rutils::env_etf$re_turns[, sym_bols]
re_turns <- zoo::na.locf(re_turns)
re_turns <- na.omit(re_turns)

# You should get the following output:
# > round(tail(re_turns), 3)
#               VTI    IEF    DBC
# 2018-01-18 -0.002 -0.003  0.000
# 2018-01-19  0.005 -0.003 -0.002
# 2018-01-22  0.008  0.000  0.002
# 2018-01-23  0.002  0.003  0.007
# 2018-01-24 -0.001 -0.002  0.011
# 2018-01-26  0.011 -0.001  0.002


# Create an objective function called object_ive(), 
# equal to the portfolio variance, plus two penalty 
# terms for weight constraints.
# The first constraint is that the sum of weights 
# is equal to 1. 
# The second constraint is that the mean weighted 
# portfolio return is equal to the target return.
# 
# The function object_ive() should accept three arguments:
# - weight_s: a named numeric vector of portfolio weights
# - re_turn: the target return
# - re_turns: the time series of asset returns
# 
# hint: Scale (multiply) the penalty terms by some 
# large numbers and then square them.
# 
# You can use functions mean(), sum(), var(), and the 
# %*% operator. 

object_ive <- function(weight_s, re_turn, re_turns) {
  portf_rets <- drop(re_turns %*% weight_s)
  var(portf_rets) + 
    (1e6*(1-sum(weight_s)))^2 + # sum penalty
    (1e6*(re_turn-sum(portf_rets)/NROW(portf_rets)))^2 # returns penalty
}  # end object_ive


# 2. (20pts) Calculate the weights of the efficient 
# portfolio, which has the smallest portfolio variance, 
# under the constraint that the mean portfolio return 
# is equal to the target return.
# hint: You can perform a portfolio optimization to 
# find the weights that minimize object_ive().
# You must use the function DEoptim::DEoptim(), with 
# the "upper" and "lower" parameters equal to 10 and 
# -10, respectively.
# hint: You must use the dots "..." argument of function 
# DEoptim::DEoptim() to pass the arguments re_turn and 
# re_turns.

# Specify the target return called re_turn:
re_turn <- 1.5*mean(re_turns)

# perform portfolio optimization
op_tim <- DEoptim::DEoptim(object_ive, 
                           upper=rep(10, n_weights),
                           lower=rep(-10, n_weights), 
                           re_turn=re_turn,
                           re_turns=re_turns, 
                           control=list(trace=FALSE, itermax=500))

# extract optimal parameters into weight_s vector
weight_s <- op_tim$optim$bestmem
names(weight_s) <- sym_bols
object_ive(weight_s, re_turn, re_turns)
summary(op_tim)
plot(op_tim)

# You should get the following output:
# > weight_s
#       VTI        IEF        DBC 
# 0.3548016  0.7849600 -0.1397615


# 3. (30pts) Calculate the same efficient portfolio 
# weights as in p.2 above, but using matrix algebra.
# hint: You can generalize the formulas from the slide 
# Minimum Variance Portfolio Weights, by adding 
# another constraint with a Lagrange multiplier.
# hint: You need to solve a set of two linear equations.
# hint: You need to calculate a matrix of products of 
# the inverse covariance matrix with the mean returns 
# and the unit vector.

# You can follow these steps:
# calculate covariance matrix of returns and its inverse
cov_mat <- cov(re_turns)
cov_inv <- solve(a=cov_mat)
# calculate vector of mean returns and the unit vector
mean_rets <- colMeans(re_turns)
u_nit <- rep(1, NCOL(cov_mat))
# calculate matrix of products of inverse with mean returns and unit vector
f_mat <- matrix(c(t(u_nit) %*% cov_inv %*% u_nit, 
                  t(u_nit) %*% cov_inv %*% mean_rets, 
                  t(mean_rets) %*% cov_inv %*% u_nit, 
                  t(mean_rets) %*% cov_inv %*% mean_rets), nc=2)

# solve for the Lagrange multipliers
multipli_ers <- solve(a=f_mat, b=c(2, 2*re_turn))

# calculate weights
weight_s <- 0.5*cov_inv %*% cbind(u_nit, mean_rets) %*% multipli_ers


# 4. (20pts) Create a named vector of target returns 
# called target_s:

factor_s <- seq(from=-0.5, to=2.5, by=0.2)
target_s <- mean(re_turns)*factor_s
names(target_s) <- paste0("targ=", factor_s)

# Calculate a matrix of the efficient portfolio weights 
# called weights_optim, for the vector of target returns, 
# by performing an sapply() loop over target_s and using 
# matrix algebra.

mat_rix <- 0.5*cov_inv %*% cbind(u_nit, mean_rets)

weights_optim <- sapply(target_s, function(re_turn) {
  mat_rix %*% solve(a=f_mat, b=c(2, 2*re_turn))
})  # end sapply
rownames(weights_optim) <- sym_bols

# You should get the following output:
# > round(t(weights_optim), 3)
#              VTI   IEF    DBC
# targ=-0.5 -0.567 0.786  0.780
# targ=-0.3 -0.475 0.786  0.688
# targ=-0.1 -0.382 0.786  0.596
# targ=0.1  -0.290 0.786  0.504
# targ=0.3  -0.198 0.786  0.412
# targ=0.5  -0.106 0.786  0.320
# targ=0.7  -0.014 0.786  0.228
# targ=0.9   0.078 0.785  0.136
# targ=1.1   0.170 0.785  0.044
# targ=1.3   0.263 0.785 -0.048
# targ=1.5   0.355 0.785 -0.140
# targ=1.7   0.447 0.785 -0.232
# targ=1.9   0.539 0.785 -0.324
# targ=2.1   0.631 0.785 -0.416
# targ=2.3   0.723 0.784 -0.508
# targ=2.5   0.816 0.784 -0.600


# Plot the weights_optim.
# You can use functions plot(), lines(), and legend().

x11(width=6, height=4)
col_ors <- c("red", "blue", "green")
plot(x=factor_s, y=weights_optim[1, ], 
     ylim=range(weights_optim), col=col_ors[1], lwd=2, t="l",
     main="Efficient Portfolio Weights 
     as Function of Target Returns", 
     xlab="target returns", ylab="portfolio weights")
lines(x=factor_s, y=weights_optim[2, ], col=col_ors[2], lwd=2)
lines(x=factor_s, y=weights_optim[3, ], col=col_ors[3], lwd=2)
legend("right", legend=rownames(weights_optim), 
       inset=0.02, bg="white", cex=1.0, lwd=6, 
       col=col_ors, bty="n")

# Your plot should be similar to eff_weights.png


# 5. (20pts) Calculate a matrix of the mean returns 
# and the standard deviations of the efficient 
# portfolios, by performing an apply() loop over 
# weights_optim.

eff_front <- apply(weights_optim, MARGIN=2, function(weight_s) {
  portf_rets <- drop(re_turns %*% weight_s)
  c(return=mean(portf_rets), sd=sd(portf_rets))
})  # end sapply

# You should get the following output:
# > 260*round(t(eff_front), 4)
#           return    sd
# targ=-0.5 -0.026 2.600
# targ=-0.3 -0.026 2.314
# targ=-0.1  0.000 2.028
# targ=0.1   0.000 1.742
# targ=0.3   0.026 1.482
# targ=0.5   0.026 1.222
# targ=0.7   0.026 1.014
# targ=0.9   0.052 0.858
# targ=1.1   0.052 0.832
# targ=1.3   0.078 0.910
# targ=1.5   0.078 1.066
# targ=1.7   0.078 1.300
# targ=1.9   0.104 1.534
# targ=2.1   0.104 1.820
# targ=2.3   0.130 2.106
# targ=2.5   0.130 2.392


# Plot the efficient frontier with the points in eff_front.
# You can use functions plot() and points().

x11(width=6, height=5)
plot(x=eff_front["sd", ], 
     y=eff_front["return", ], t="l", 
     xlim=c(0.0, max(eff_front["sd", ])), col="blue", lwd=2,
     # ylim=c(0.0*min_ret, 2.0*min_ret),
     main="Efficient Frontier and Capital Market Line", 
     xlab="standard deviation", ylab="return")
points(x=eff_front["sd", ], y=eff_front["return", ], 
       col="blue", lwd=3)


# extra
# 6. (20pts) Calculate the tangent line which touches 
# the efficient frontier at the efficient portfolio 
# from p.3 above.
# hint: Calculate the weights, the mean returns, and 
# the standard deviations for two efficient portfolios
# with very close target returns, say 2*re_turn and 
# 2.01*re_turn.

# solve for the Lagrange multipliers
multipli_ers <- solve(a=f_mat, b=c(2, 2.01*re_turn))
# calculate weights
weights_p <- 0.5*cov_inv %*% cbind(u_nit, mean_rets) %*% multipli_ers

tan_gent <- apply(cbind(weight_s, weights_p), MARGIN=2, function(weight_s) {
  portf_rets <- drop(re_turns %*% weight_s)
  c(return=mean(portf_rets), sd=sd(portf_rets))
})  # end sapply
colnames(tan_gent) <- c("base", "bump")

# Calculate the slope and the intercept of the tangent 
# line.  The intercept is the risk-free rate.

slop_e <- unname(diff(tan_gent["return", ])/diff(tan_gent["sd", ]))
risk_free_rate <- unname(tan_gent["return", 1] - slop_e*tan_gent["sd", 1])

# You should get the following output:
# > slop_e
# [1] 0.05307091
# > risk_free_rate
# [1] 8.968558e-05


# Add to the efficient frontier plot the tangent line 
# and points for the efficient portfolio from p.3 and 
# the corresponding risk-free rate
# You can use functions plot(), points(), text(), and abline().

points(x=0, y=risk_free_rate, col="red", lwd=3)
text(x=0, y=risk_free_rate, labels="risk-free rate", pos=4, cex=0.8)
points(x=tan_gent["sd", 1], y=tan_gent["return", 1], col="red", lwd=3)
text(x=tan_gent["sd", 1], y=tan_gent["return", 1], labels="tangent", pos=4, cex=0.8)
abline(a=risk_free_rate, b=slop_e, lwd=2, col="green")

# Your plot should be similar to eff_front_tangent.png

# extra end



############## Part II
# Summary: Create an Rcpp function which simulates 
# the GARCH model using Rcpp.

# Define the GARCH parameters
om_ega <- 0.01
al_pha <- 0.5
be_ta <- 0.2
len_gth <- 10000
set.seed(1121)
r_norm <- rnorm(len_gth)

# Simulate the GARCH model in R as follows
# (run the code):

# Allocate memory and initialize variables
re_turns <- numeric(len_gth)
vari_ance <- numeric(len_gth)
vari_ance[1] <- om_ega/(1-al_pha-be_ta)
re_turns[1] <- r_norm[1]*sqrt(vari_ance[1])

# Simulate GARCH model using for() loop in R
for (i in 2:len_gth) {
  re_turns[i] <- r_norm[i]*sqrt(vari_ance[i-1])
  vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 + be_ta*vari_ance[i-1]
}  # end for


# 1. (30pts) Create an Rcpp function called garch_proc(), 
# which calculates the re_turns and vari_ance as above.
# 
# The function garch_proc() should accept five arguments: 
# len_gth, om_ega, al_pha, be_ta, and r_norm.
# garch_proc() should return a matrix with two columns: 
# re_turns and vari_ance
# 
# hint: You don't need to use Armadillo code, just Rcpp.
# hint: You can adapt code from rcpp_ou.cpp.
# 
# Save the Rcpp function garch_proc() in the file 
# called rcpp_test6.cpp, and compile it as follows:

Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/rcpp_test6.cpp")

# Simulate the GARCH model using Rcpp as follows:

garch_rcpp <- garch_proc(len_gth=len_gth, 
                         om_ega=om_ega, 
                         al_pha=al_pha, 
                         be_ta=be_ta, 
                         r_norm=r_norm)

# You should get TRUE from the following output:

all.equal(garch_rcpp, unname(cbind(re_turns, vari_ance)))

# Compare the speed of Rcpp and R using microbenchmark()

library(microbenchmark)
summary(microbenchmark(
  pure_r={for (i in 2:len_gth) {
    re_turns[i] <- r_norm[i]*sqrt(vari_ance[i-1])
    vari_ance[i] <- om_ega + al_pha*re_turns[i]^2 + be_ta*vari_ance[i-1]
  }},
  r_cpp=garch_proc(len_gth=len_gth, om_ega=om_ega, al_pha=al_pha, be_ta=be_ta, r_norm=r_norm),
  times=10))[, c(1, 4, 5)]

# You should get output similar to the following:
# (If you run microbenchmark the second time you will 
# see even greater improvement because of R caching.)
# 
#     expr       mean     median
# 1 pure_r 14550.0590 14495.3220
# 2  r_cpp   677.9107   558.3685


# 2. (30pts) Run function garch_proc() for a matrix 
# of alpha and beta parameters, and calculate the 
# kurtosis and the Jarque-Bera test statistic for 
# each pair of alpha and beta parameters.
# If alpha plus beta are greater than 1, then set
# the outputs to zero.
# Save the outputs into two matrices called 
# kur_tosis and jb_stat.

# Define vectors of GARCH parameters:
alpha_s <- seq(0.3, 0.8, by=0.1)
beta_s <- seq(0.1, 0.5, by=0.1)

# Allocate memory for kur_tosis and jb_stat
# You can use functions matrix(), dimnames(), 
# rownames(), colnames(), list(), and paste0().

kur_tosis <- matrix(nrow=NROW(alpha_s), ncol=NROW(beta_s))
dimnames(kur_tosis) <- list(paste0("alpha=", alpha_s),
                            paste0("beta=", beta_s))
jb_stat <- matrix(nrow=NROW(alpha_s), ncol=NROW(beta_s))
dimnames(jb_stat) <- list(paste0("alpha=", alpha_s),
                          paste0("beta=", beta_s))


# Perform two for() loops in R, over the vectors 
# alpha_s and beta_s.
# You must use the vector r_norm inside the loop, 
# instead of calling rnorm().
# You can use functions for(), if(), seq_along(),
# moments::moment(), and tseries::jarque.bera.test().

for (i in seq_along(alpha_s))
  for (j in seq_along(beta_s)) {
    if ((alpha_s[i] + beta_s[j]) < 1.0) {
      garch_rcpp <- garch_proc(len_gth=len_gth, om_ega=om_ega, al_pha=alpha_s[i], be_ta=beta_s[j], r_norm=r_norm)
      re_turns <- garch_rcpp[, 1]
      # calculate kurtosis of GARCH returns
      kur_tosis[i, j] <- moments::moment(re_turns, order=4) / moments::moment(re_turns, order=2)^2
      # perform Jarque-Bera test of normality
      jb_stat[i, j] <- tseries::jarque.bera.test(re_turns)$statistic
    }
    else
    {
      kur_tosis[i, j] <- 0.0
      jb_stat[i, j] <- 0.0
    }  # end if
  }  # end for

# You should get output similar to the following:
# (Note the proper row and column names)

# > round(kur_tosis, 2)
#           beta=0.1 beta=0.2 beta=0.3 beta=0.4 beta=0.5
# alpha=0.3     3.60     3.64     3.70     3.82     4.08
# alpha=0.4     4.20     4.31     4.52     4.99     6.23
# alpha=0.5     5.18     5.53     6.30     8.28     0.00
# alpha=0.6     6.93     8.13    11.20     0.00     0.00
# alpha=0.7    10.44    14.81     0.00     0.00     0.00
# alpha=0.8    18.31     0.00     0.00     0.00     0.00

# > round(jb_stat)
#           beta=0.1 beta=0.2 beta=0.3 beta=0.4 beta=0.5
# alpha=0.3      154      173      209      285      493
# alpha=0.4      608      727      979     1674     4405
# alpha=0.5     2016     2704     4632    11785        0
# alpha=0.6     6549    11140    28504        0        0
# alpha=0.7    23413    59063        0        0        0
# alpha=0.8    99111        0        0        0        0



############## Part III
# Summary: Simulate a rolling portfolio optimization strategy 
# using the regularized inverse of the covariance matrix.

# Run all the code below, to set up the data.
# Calculate the xts series of re_turns as follows
# (run the code):

library(HighFreq)

sym_bols <- colnames(rutils::env_etf$re_turns)
sym_bols <- sym_bols[!(sym_bols=="VXX")]
n_weights <- NROW(sym_bols)
re_turns <- rutils::env_etf$re_turns[, sym_bols]
re_turns <- zoo::na.locf(re_turns)
re_turns <- na.omit(re_turns)

# Calculate the vector of average daily excess returns.
# risk_free is the daily risk-free rate.
# (run the code):

risk_free <- 0.03/260
ex_cess <- re_turns - risk_free

# Calculate the covariance matrix of ex_cess and 
# perform its eigen decomposition (run the code):

cov_mat <- cov(ex_cess)
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors

# Define max_eigen as the number of eigen-vectors used 
# in the regularized inverse.

max_eigen <- 2

# Calculate the regularized inverse of the covariance 
# matrix from its eigen decomposition (run the code):

cov_inv <- eigen_vec[, 1:max_eigen] %*% 
  (t(eigen_vec[, 1:max_eigen]) / ei_gen$values[1:max_eigen])


# Define the shrinkage intensity:

al_pha <- 0.01

# Calculate the weighted vector of mean excess returns using 
# the shrinkage intensity (this is a form of shrinkage):

excess_mean <- colMeans(ex_cess)
excess_mean <- ((1-al_pha)*excess_mean + al_pha*mean(excess_mean))

# Calculate the maximum Sharpe ratio portfolio weights
# (run the code):

weight_s <- cov_inv %*% excess_mean
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
names(weight_s) <- colnames(re_turns)


# 1. (30pts) Create an RcppArmadillo function called 
# sharpe_weights_reg(), which calculates the weight_s
# as above.
# The function sharpe_weights_reg() should accept three 
# arguments: re_turns, al_pha, and max_eigen.
# sharpe_weights_reg() should return a vector of weight_s.
# hint: Adapt sharpe_weights() from test #5
# hint: To calculate the weighted vector of mean excess 
# returns you can define vectors with al_pha and 1-al_pha.
# 
# Save the Rcpp code in a file called rcpp_test6.cpp,
# and compile it as follows:

Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/rcpp_test6.cpp")

# Calculate the weights using sharpe_weights_reg() and 
# call them weights_arma:

weights_arma <- drop(sharpe_weights_reg(ex_cess, al_pha, max_eigen))

# You should get TRUE from the following output:

all.equal(unname(weight_s), weights_arma)



# 2. (30pts) Simulate a monthly rolling portfolio optimization 
# strategy:

# Run all the code below, to set up the data.
# Define end_points
end_points <- rutils::calc_endpoints(re_turns, inter_val="months")
# Remove initial end_points for warmpup
end_points <- end_points[end_points>50]
len_gth <- NROW(end_points)
# Define look_back interval and start_points over sliding window:
look_back <- 12
start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])

# Simulate the strategy in lapply() loop:

strat_rets <- lapply(2:NROW(end_points),
                     function(i) {
                       # subset the ex_cess returns
                       ex_cess <- ex_cess[start_points[i-1]:end_points[i-1], ]
                       cov_mat <- cov(ex_cess)
                       # perform eigen decomposition and calculate eigenvectors and eigenvalues
                       ei_gen <- eigen(cov_mat)
                       eigen_vec <- ei_gen$vectors
                       # calculate regularized inverse
                       cov_inv <- eigen_vec[, 1:max_eigen] %*% (t(eigen_vec[, 1:max_eigen]) / ei_gen$values[1:max_eigen])
                       # weight_s are proportional to the mean of ex_cess
                       excess_mean <- colMeans(ex_cess)
                       # shrink excess_mean vector to the mean of excess_mean
                       excess_mean <- ((1-al_pha)*excess_mean + al_pha*mean(excess_mean))
                       # apply regularized inverse to mean of ex_cess
                       weight_s <- cov_inv %*% excess_mean
                       weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
                       # subset the re_turns to out-of-sample returns
                       re_turns <- re_turns[(end_points[i-1]+1):end_points[i], ]
                       # calculate the out-of-sample portfolio returns
                       xts(re_turns %*% weight_s, index(re_turns))
                     }  # end anonymous function
)  # end lapply

# Flatten the list of xts into a single xts series
strat_rets <- rutils::do_call(rbind, strat_rets)
colnames(strat_rets) <- "strat_rets"
# Add warmup period so that index(strat_rets) equals index(re_turns)
in_dex <- index(re_turns)[index(re_turns) < start(strat_rets)]
strat_rets <- rbind(xts(numeric(NROW(in_dex)), in_dex), strat_rets)
# Cumulate strat_rets
strat_rets <- cumsum(strat_rets)
# Plot dygraph
dygraphs::dygraph(strat_rets, 
                  main="Cumulative Returns of Max Sharpe Portfolio Strategy")


# Create an R function called roll_portf_r(), which 
# calculates the weight_s as above.
# The function roll_portf_r() should accept six arguments: 
# ex_cess, re_turns, start_points, end_points, al_pha, 
# and max_eigen.
# roll_portf_r() should return an xts series of cumulative 
# strategy returns.
# Save the code of the function roll_portf_r() in the file 
# roll_portf.R

# Source the function roll_portf_r() from the file 
# roll_portf.R, using function source().

source("C:/Develop/R/lecture_slides/scripts/roll_portf.R")


# You should get TRUE from the following output:

all.equal(strat_rets, 
          roll_portf_r(ex_cess,
                       re_turns,
                       start_points, 
                       end_points, 
                       al_pha, 
                       max_eigen))


# 3. (30pts) Create a shiny app that simulates the rolling 
# portfolio optimization strategies, and produces an 
# interactive dygraphs plot.
# The shiny app should run the function roll_portf_r().
# The shiny app should also run an equal weight, static 
# portfolio with all the assets weighted equally.
# Save the shiny app in a file called app_roll_portf.R
# hint: Adapt code from app_dygraphs2.R on NYU Classes.

# The shiny app output should be similar to roll_portf_shiny.png
# on NYU Classes.



# 4. (30pts) Create an RcppArmadillo function called 
# roll_portf(), which calculates the returns of the rolling 
# portfolio optimization strategy as above.
# The function roll_portf() should accept six arguments: 
# ex_cess, re_turns, start_points, end_points, al_pha, 
# and max_eigen.
# roll_portf() should return a vector of non-cumulated 
# strategy returns.
# hint: Adapt get_pca_var() from test #4.
# hint: You can use the function sharpe_weights_reg()
# hint: You can use the RcppArmadillo functions .subvec()
# and .fill()
# hint: You can use the RcppArmadillo %	operator to 
# perform element-wise multiplication of two vectors.
# 
# Save the Rcpp code in a file called rcpp_test6.cpp,
# and compile it as follows:

Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/rcpp_test6.cpp")

# Calculate the strategy returns using roll_portf() and 
# call them strat_rets_arma:

strat_rets_arma <- roll_portf(ex_cess, 
                              re_turns, 
                              start_points-1, 
                              end_points-1, 
                              al_pha=al_pha, 
                              max_eigen=max_eigen)

# You should get TRUE from the following output:

all.equal(as.numeric(strat_rets), cumsum(drop(strat_rets_arma)))




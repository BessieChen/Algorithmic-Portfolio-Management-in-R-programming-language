#################################
### FRE7241 Test #4 Solutions February 27, 2018
#################################
# Max score 110pts

# The below solutions are examples,
# Slightly different solutions are also possible.


############## Part I
# Summary: Calculate performance of random sub-portfolios
# of S&P500 constituent stocks.

# 1. (20pts) Load the file sp500_prices.RData containing 
# an xts series called price_s, with the daily closing 
# prices of the S&P500 stock index constituents.  

load("C:/Develop/R/lecture_slides/data/sp500_prices.RData")


# Create a vector of dates called date_s, equal to the 
# index of price_s.
# Use function index(),

date_s <- index(price_s)

# Normalize the columns of price_s, so that the first 
# row for all columns is equal to 1.
# The columns of price_s represent the growth of one 
# dollar invested in that stock.
# You can use functions as.numeric() and t().

price_s <- t(t(price_s) / as.numeric(price_s[1, ]))

# You should get the following output:
# round(price_s[1:6, 1:6], 3)
#              RCL   BBY    HP   DVN   CME  MSFT
# 2007-01-03 1.000 1.000 1.000 1.000 1.000 1.000
# 2007-01-04 1.007 1.016 0.997 0.982 1.007 0.998
# 2007-01-05 0.997 1.019 1.000 1.007 1.012 0.993
# 2007-01-08 1.007 1.007 1.005 1.012 1.023 1.002
# 2007-01-09 1.034 1.000 1.011 1.012 1.029 1.003
# 2007-01-10 1.021 1.007 1.003 0.997 1.058 0.993
# 
# round(price_s[(NROW(price_s)-5):NROW(price_s), 1:6], 3)
#              RCL   BBY    HP   DVN   CME  MSFT
# 2018-01-19 3.490 2.054 3.830 0.753 2.216 3.919
# 2018-01-22 3.483 2.086 3.829 0.788 2.233 3.989
# 2018-01-23 3.443 2.072 3.853 0.782 2.216 4.002
# 2018-01-24 3.586 2.052 3.843 0.782 2.226 3.998
# 2018-01-25 3.570 2.038 3.968 0.772 2.214 4.020
# 2018-01-26 3.653 2.081 3.989 0.771 2.229 4.096


# Calculate a vector equal to the equal dollar-weighted 
# prices of the index components, i.e. the average of 
# the rows of price_s, and call it in_dex.
# You can use functions NCOL() and rowSums(). 

n_col <- NCOL(price_s)
in_dex <- rowSums(price_s)/n_col

# You should get the following output:
# > tail(in_dex)
# 2018-01-19 2018-01-22 2018-01-23 2018-01-24 2018-01-25 2018-01-26 
#   3.831064   3.855518   3.882164   3.886024   3.890283   3.929215 


# 2. (30pts) Select twenty equally dollar-weighted, 
# random sub-portfolios from the columns of price_s,
# with each sub-portfolio being the average of five
# randomly selected columns (stocks) of price_s.
# Bind the sub-portfolio prices into a single xts 
# series called sub_portfolios.  
# You can use the vector of dates called date_s.
# You can use function sample.int() with "replace=FALSE". 
# The initial instructions had a typo with "replace=TRUE". 
# You will get full credit in either case.
# You can use functions sapply(), xts::xts(), 
# rowSums(), and colnames(). 

n_portf <- 20
n_stocks <- 5
set.seed(1121)
sub_portfolios <- sapply(1:n_portf, function(x) {
  price_s <- price_s[, sample.int(n=n_col, size=n_stocks, replace=FALSE)]
  rowSums(price_s)/n_stocks
})  # end sapply

sub_portfolios <- xts(sub_portfolios, order.by=date_s)
colnames(sub_portfolios) <- paste0("portf", 1:n_portf)

# You should get the following output:
# 
# round(sub_portfolios[1:4, 1:4], 3)
#            portf1 portf2 portf3 portf4
# 2007-01-03  1.000  1.000  1.000  1.000
# 2007-01-04  1.010  1.000  1.013  1.000
# 2007-01-05  1.013  0.989  1.004  1.001
# 2007-01-08  1.022  0.990  0.999  1.004
# 
# round(sub_portfolios[(NROW(price_s)-3):NROW(price_s), 1:4], 3)
#            portf1 portf2 portf3 portf4
# 2018-01-23  5.582  1.878  2.535  5.055
# 2018-01-24  5.545  1.870  2.522  5.066
# 2018-01-25  5.468  1.859  2.528  5.089
# 2018-01-26  5.510  1.884  2.569  5.169


# Plot the sub_portfolios from worst to best (based 
# on final price) using a color ramp from red to blue.
# 
# Create a color ramp, using functions colorRampPalette() 
# and order(). 

col_ors <- colorRampPalette(c("red", "blue"))(n_portf)
col_ors <- col_ors[order(order(sub_portfolios[NROW(sub_portfolios), ]))]

# Create a plot of the sub_portfolios with the custom 
# color ramp, using either function zoo::plot.zoo(), 
# or functions chart_theme() and chart_Series(). 

# plot using chart_theme() and chart_Series()
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(sub_portfolios, theme=plot_theme, 
                       name="Random S&P500 stock sub-portfolios (normalized)")

# OR plot using zoo::plot.zoo()
zoo::plot.zoo(sub_portfolios, plot.type="single", 
              col=col_ors, xlab="", ylab="",
              main="Random S&P500 stock sub-portfolios (normalized)")


# Your plot should be similar to sp500_sub_portfolios.png


# Calculate an xts series called above_index, with the 
# percentage of sub-portfolios whose prices at the end 
# of each year are above the index price in_dex.
# You can use functions endpoints(), rowSums(), 
# chart_theme(), and xts(). 

end_points <- xts::endpoints(date_s, on="years")
above_index <- (sub_portfolios[end_points, ] > in_dex[end_points])
above_index <- rowSums(above_index)/n_portf
above_index <- xts::xts(above_index, order.by=date_s[end_points])
colnames(above_index) <- "percentage"

# You should get output similar to the following:
# > above_index
#            percentage
# 2007-12-31       0.70
# 2008-12-31       0.55
# 2009-12-31       0.60
# 2010-12-31       0.65
# 2011-12-30       0.60
# 2012-12-31       0.60
# 2013-12-31       0.40
# 2014-12-31       0.40
# 2015-12-31       0.45
# 2016-12-30       0.35
# 2017-12-29       0.40
# 2018-01-26       0.40

# Create a plot of above_index using function 
# zoo::plot.zoo(). 

zoo::plot.zoo(above_index, col="blue", lwd=2, xlab="", ylab="",
              main="Percentage of Random Sub-portfolios 
              Above the Index")

# Your plot should be similar to sp500_sub_portfolios_above.png



############## Part II
# Summary: Perform PCA of returns over annual intervals 
# using RcppArmadillo.

# 1. (20pts) Load the file sp500_prices.RData containing 
# an xts series called price_s, with the daily closing 
# prices of the S&P500 stock index constituents.  

library(HighFreq)
load("C:/Develop/R/lecture_slides/data/sp500_prices.RData")

# Calculate the simple returns (differences not percentages) 
# of the first six columns of price_s and call it re_turns.
# You can use function rutils::diff_it().

re_turns <- rutils::diff_it(price_s[, 1:6])

# De-mean (center) and scale the returns.
# You can use functions t(), colMeans(), colSums(), 
# and NROW().

re_turns <- t(t(re_turns) - colMeans(re_turns))
re_turns <- t(t(re_turns) / sqrt(colSums(re_turns^2)/(NROW(re_turns)-1)))
date_s <- index(price_s)
re_turns <- xts(re_turns, date_s)

# Perform PCA as follows:

pc_a <- prcomp(re_turns, center=TRUE, scale=TRUE)

# Create an Rcpp function called get_pca(), which 
# calculates a list containing the PCA objects 
# (sdev, rotation, etc.) from the matrix re_turns.
# Hint: You can use the RcppArmadillo function 
# arma::princomp().
# The function get_pca() requires just a few lines 
# of Rcpp code.
# Save the Rcpp code in a file called rcpp_test4.cpp,
# and compile it as follows:

Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/rcpp_test4.cpp")

# Calculate the PCA using the Rcpp function get_pca() 
# and save the output to a list called pca_arma.
# Verify that the elements of pc_a and pca_arma contain 
# the same numbers by using function all.equal().
# You can also use functions zoo::coredata(), unname(), 
# and drop().

pca_arma <- get_pca(re_turns)

all.equal(unname(pc_a$rotation), pca_arma$coefficients)
all.equal(unname(pc_a$sdev)^2, drop(pca_arma$latent))
all.equal(unname(pc_a$x), pca_arma$score)


# 2. (20pts) Create a vector of annual end points 
# from the index of price_s, and call it end_points.
# Use function endpoints() from package xts.

end_points <- xts::endpoints(price_s, on="years")

# You should get the following output:
# > end_points
# [1] 0 251 504 756 1008 1260 1510 1762 2014 2266 2518 2769 2787

# Perform an sapply() loop over the length of end_points.
# Inside the loop subset (select) the annual returns from 
# re_turns, then de-mean (center) and scale them, perform 
# PCA on the annual returns, and extract the vector pc_a$sdev.
# The sapply() loop should produce a matrix with six rows and
# (NROW(end_points)-1) columns.

s_dev <- sapply(2:NROW(end_points), function(i) {
  re_turns <- re_turns[(end_points[i-1] + 1):end_points[i], ]
  pc_a <- prcomp(re_turns, center=TRUE, scale=TRUE)
  pc_a$sdev
})  # end sapply

# You should get the following output:
# > round(s_dev, 3)
#       [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10] [,11] [,12]
# [1,] 1.662 1.754 1.828 1.860 1.904 1.633 1.535 1.569 1.545 1.508 1.301 1.437
# [2,] 0.977 1.184 0.878 0.863 0.880 0.951 0.981 1.025 1.193 1.073 1.211 1.066
# [3,] 0.861 0.700 0.792 0.779 0.726 0.870 0.930 0.908 0.898 0.949 1.003 0.990
# [4,] 0.829 0.691 0.731 0.726 0.685 0.817 0.875 0.857 0.802 0.919 0.928 0.881
# [5,] 0.733 0.622 0.685 0.619 0.600 0.782 0.784 0.783 0.689 0.753 0.797 0.833
# [6,] 0.563 0.411 0.507 0.529 0.492 0.627 0.659 0.562 0.517 0.510 0.582 0.592


# 3. (30pts) Create an Rcpp function called get_pca_var(), 
# which reproduces the above sapply loop and returns a 
# matrix of PCA variances equal to s_dev^2.
# Hint: To perform matrix subsetting, you can use the 
# RcppArmadillo function arma::re_turns.rows().
# The function get_pca_var() requires at most seven 
# lines of Rcpp code.
# Save the Rcpp code in a file called rcpp_test4.cpp,
# and compile it as follows:
# Verify that get_pca_var() returns a matrix of PCA 
# variances equal to s_dev^2, using function all.equal().

pca_var <- get_pca_var(re_turns, end_points-1)

all.equal(s_dev^2, pca_var[NROW(pca_var):1, ])


# Benchmark the function get_pca_var() compared 
# to the sapply loop, using the function microbenchmark().
# You should obtain a speedup of almost 3 times.
# The function get_pca_varr() is another possible solution,
# but it's about 30 times slower than get_pca_var()!

library(microbenchmark)
summary(microbenchmark(
  get_pca_var=get_pca_var(zoo::coredata(re_turns), end_points-1),
  get_pca_varr=get_pca_varr(zoo::coredata(re_turns), end_points-1),
  s_apply=sapply(2:NROW(end_points), function(i) {
    re_turns <- re_turns[(end_points[i-1] + 1):end_points[i], ]
    pc_a <- prcomp(re_turns, center=TRUE, scale=TRUE)
    pc_a$sdev
  }),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary



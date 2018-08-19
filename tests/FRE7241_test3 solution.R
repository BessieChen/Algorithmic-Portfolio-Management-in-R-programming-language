#################################
### FRE7241 Test #3 Solutions February 20, 2018
#################################
# Max score 110pts

# The below solutions are examples,
# Slightly different solutions are also possible.

############## Part I
# Summary: Calculate the maximum drawdown of a time series.

# 1. (20pts) Extract the adjusted close prices from 
# rutils::env_etf$VTI into a variable called price_s.
# You can use function Ad() from package quantmod.

library(quantmod)
price_s <- Ad(rutils::env_etf$VTI)

# The cumulative maximum of a price series is the maximum
# price in the past, reached up to that point in time.
# Calculate the cumulative maximum of price_s using
# function cummax().
# Plot the cumulative maximum of price_s using function
# chart_Series().

chart_Series(x=cummax(price_s),
             name="Cumulative maximum prices")

# A drawdown is a drop in price from its previous maximum.
# Calculate the xts time series of drawdowns of price_s,
# as the difference between price_s minus the cumulative
# maximum of price_s, and call it draw_down.

draw_down <- (price_s - cummax(price_s))

# plot draw_down using function chart_Series().

plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
chart_Series(x=draw_down, name="VTI Drawdowns", theme=plot_theme)

# Find the minimum value of draw_down (call it 
# max_drawdown) and the date when it reaches its 
# minimum (call it date_trough). 
# You can use functions index() and which.min().

in_dex <- index(price_s)
date_trough <- in_dex[which.min(draw_down)]
max_drawdown <- draw_down[date_trough]

# You should get the following output:
# > date_trough
# [1] "2009-03-09"
# > max_drawdown
# VTI.Adjusted
# 2009-03-09     -35.0769


# Add a vertical red line to the draw_down plot,
# at the date date_trough.
# hint: use function match() and index() to first
# calculate the index of date_trough.
# You can use functions match(), index(), and abline(),

abline(v=match(date_trough, index(draw_down)), 
       lwd=2, col="red")

# Your plot should be similar to drawdown_plot.png


# 2. (20pts) Divide draw_down into two time series at
# the date date_trough.
# First subset draw_down to dates before date_trough,
# and call it pre_drawdown,

pre_drawdown <- draw_down[in_dex<date_trough]

# Next subset draw_down to dates after date_trough,
# and call it post_drawdown,

post_drawdown <- draw_down[in_dex>date_trough]

# Now find the date when the drawdown period starts.
# The drawdown starts when draw_down is first zero
# and then starts decreasing to some price below zero.
# Find the latest date when pre_drawdown was still
# equal to zero, and call it date_from.
# date_from is when the drawdown started.
# You can use functions index() and max().

date_from <- max((index(pre_drawdown))[pre_drawdown==0])

# Now find the date when the drawdown period ends.
# When the current price exceeds the previous maximum
# price, then draw_down returns back to zero, and the
# drawdown period is over.
# A drawdown ends when draw_down first returns back
# to zero after date_trough.
# Find the first date when post_drawdown returns back
# to zero, and call it date_to.
# date_to is when the drawdown has ended.
# You can use functions index() and min(),

date_to <- min((index(post_drawdown))[post_drawdown==0])

# You should get the following output:
# > date_from
# [1] "2007-10-09"
# > date_to
# [1] "2012-03-13"


# 3. (20pts) Combine the three dates: date_from,
# date_trough, and date_to into a named vector with
# names "from", "trough", and "to", and call it
# drawdown_dates,

drawdown_dates <- c(from=date_from, trough=date_trough, to=date_to)

# You should get the following output:
# drawdown_dates
#         from       trough           to
# "2007-10-09" "2009-03-09" "2012-03-13"

# Plot price_s using function chart_Series().

plot_theme <- chart_theme()
plot_theme$col$line.col <- c("blue")
chart_Series(x=price_s, name="VTI drawdown dates", 
             theme=plot_theme)

# Add vertical green, red, and orange lines for the
# three dates: date_from, date_trough, date_to.
# Add text at the vertical lines equal to
# names(drawdown_dates).
# hint: use function match() and index() to first
# calculate the index of drawdown_dates.
# You can use functions match(), index(), abline(),
# and text().

abline(v=match(drawdown_dates, index(draw_down)),
       lwd=2, col=c("green", "red", "orange"))
text(x=match(drawdown_dates, index(draw_down)),
     y=as.vector(price_s[drawdown_dates]),
     labels=names(drawdown_dates), pos=3, cex=0.8)

# Your plot should be similar to drawdown_vti.png



############## Part II
# Summary: Calculate the eigenvectors and eigenvalues
# of the covariance matrix of returns as a function of 
# the number of time periods, using RcppArmadillo.

# First perform the calculations in R below (run the code).
# Create a matrix called re_turns, of random returns 
# with the number of columns equal to n_assets, and 
# the number of rows equal to n_assets^2.
n_assets <- 10
set.seed(1121)
re_turns <- matrix(rnorm(2*n_assets^2), nc=n_assets)
# Calculate the eigenvalues
cov_mat <- cov(re_turns)
eigen_values <- eigen(cov_mat)$values

# 1. (20pts) 
# Create an Rcpp function called get_eigen(), which 
# calculates the eigen_values of the matrix cov_mat.
# Hint: You can use the RcppArmadillo function arma::eig_sym().
# The function get_eigen() requires at most two lines 
# of Rcpp code.
# Save the Rcpp code in a file called rcpp_test3.cpp,
# and compile it as follows:

Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/rcpp_test3.cpp")

# Calculate the eigenvalues using the Rcpp function 
# get_eigen() and save the output to rcpp_eigen_values.
# Hint: if get_eigen() returns a matrix then you may 
# need to coerce it into a vector, and you may also 
# need to sort it.
# Hint: You can use function sort().

rcpp_eigen_values <- get_eigen(cov_mat)
rcpp_eigen_values <- sort(rcpp_eigen_values, decreasing=TRUE)

# Verify that eigen_values and rcpp_eigen_values are
# the same using function all.equal():

all.equal(eigen_values, rcpp_eigen_values)


# 2. (30pts) 
# This part of the exercise demonstrates that if the 
# number of time periods of returns (rows) is less than 
# the number of portfolio assets (columns), then the 
# returns are collinear, so the covariance matrix 
# is singular, and some of its eigenvalues are zero,

# Calculate the smallest eigenvalues as a function 
# of the number of time periods (rows) (run the code).

n_rows <- (n_assets/2):(2*n_assets)
eigen_values <- sapply(n_rows, function(x) {
  # subset the returns
  re_turns <- re_turns[1:x, ]
  # calculate the covariance matrix
  cov_mat <- cov(re_turns)
  # return the smallest eigenvalue
  min(eigen(cov_mat)$values)
})  # end sapply


# Create an Rcpp function called get_min_eigens(), which 
# reproduces the above sapply loop and returns the same 
# values as in eigen_values.
# Hint: To perform matrix subsetting, you can use the 
# RcppArmadillo function arma::re_turns.rows().
# The function get_min_eigens() requires at most seven 
# lines of Rcpp code.
# Save the Rcpp code in a file called rcpp_test3.cpp,
# and compile it as follows:

Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/rcpp_test3.cpp")

# Calculate the eigenvalues using the Rcpp function 
# get_min_eigens() and save the output to rcpp_eigen_values.
# Hint: if get_min_eigens() returns a matrix then you may 
# need to coerce it into a vector.
# Hint: You can use function as.vector().

rcpp_eigen_values <- get_min_eigens(re_turns, n_rows-1)
rcpp_eigen_values <- as.vector(rcpp_eigen_values)

# Verify that eigen_values and rcpp_eigen_values are
# the same using function all.equal():

all.equal(eigen_values, rcpp_eigen_values)


# Benchmark the function get_min_eigens() compared 
# to the sapply loop, using the function microbenchmark().
# You should obtain a speedup on the order of 50 times!

library(microbenchmark)
summary(microbenchmark(
  r_cpp=get_min_eigens(re_turns, n_rows-1),
  s_apply=sapply(n_rows, function(x) {
    re_turns <- re_turns[1:x, ]
    re_turns <- apply(re_turns, MARGIN=2, function(y) (y-mean(y)))
    cov_mat <- crossprod(re_turns) / (x-1)
    min(eigen(cov_mat)$values)
  }),
  times=100))[, c(1, 4, 5)]  # end microbenchmark summary



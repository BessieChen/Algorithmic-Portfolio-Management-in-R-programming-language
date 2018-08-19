#################################
### FRE7241 Test #2 Solutions February 6, 2018
#################################
# Max score 130pts

# The below solutions are examples,
# Slightly different solutions are also possible.

############## Part I
# Summary: Perform interval aggregations on an OHLC time 
# series, to obtain an OHLC series with lower periodicity, 
# with the same output as function to.period() from package 
# xts, or function to_period() from package rutils.

# 1. (20pts) Define an OHLC time series called oh_lc 
# as follows:

oh_lc <- rutils::env_etf$VTI

# Define an aggregation function for OHLC data, 
# called agg_ohlc(). 
# agg_ohlc() should return a named vector of Open, 
# High, Low, and Close prices, and Volume.
# The Open price should be the first Open price.
# The High price should be the max of the High prices.
# The Low price should be the min of the Low prices.
# The Close price should be the last Close price.
# The Volume should be the sum of Volumes.
# You can use functions c(), NROW(), and coredata(). 

agg_ohlc <- function(oh_lc)
  c(Open=coredata(oh_lc[1, 1]), 
    High=max(oh_lc[, 2]), 
    Low=min(oh_lc[, 3]), 
    Close=coredata(oh_lc[NROW(oh_lc), 4]), 
    Volume=sum(oh_lc[, 5]))

# You should get the following output:
# > round(agg_ohlc(oh_lc))
# Open   High   Low   Close       Volume 
#   42    147     0     147   7113840736


# 2. (10pts) Calculate equally spaced end points 
# over oh_lc, with a stub interval at the beginning, 
# and call them end_points.

# define look-back interval
look_back <- 252

n_row <- NROW(oh_lc)
num_agg <- n_row %/% look_back
end_points <- n_row-look_back*num_agg + look_back*(0:num_agg)
len_gth <- NROW(end_points)

# Calculate starting points as the lag of end_points, 
# and call them start_points.
# The start_points and end_points should together 
# form non-overlapping intervals over oh_lc. 

start_points <- c(1, end_points[1:(len_gth-1)]+1)

# You should get the following output:
# > end_points
# [1]  158  410  662  914 1166 1418 1670 1922 2174 2426 2678
# [12] 2930 3182 3434 3686 3938 4190
# 
# > start_points
#  [1]    1  159  411  663  915 1167 1419 1671 1923 2175 2427 2679 2931
# [14] 3183 3435 3687 3939


# 3. (20pts) Perform an sapply() loop over the length of 
# end_points, applying agg_ohlc() to the intervals 
# defined by the start_points and end_points along 
# the way.  Call the output agg_s.
# You can use functions sapply(), xts(), index(), 
# is.vector(), and t(). 

agg_s <- sapply(1:len_gth, 
  function(it_er) {
    agg_ohlc(oh_lc[start_points[it_er]:end_points[it_er]])
  })  # end sapply

# Coerce the output of the sapply() loop into an xts series.

if (is.vector(agg_s))
  agg_s <- t(agg_s)
agg_s <- t(agg_s)
# coerce agg_s into xts series
agg_s <- xts(agg_s, order.by=index(oh_lc[end_points]))

# You should get the following output:
# > round(tail(agg_s), 3)
#               Open    High     Low   Close    Volume
# 2013-01-24  59.718  70.360  57.988  70.033 488161341
# 2014-01-24  70.306  89.261  69.661  86.445 598609684
# 2015-01-26  86.445 101.608  83.731 100.179 657858412
# 2016-01-26  99.405 105.120  88.322  93.017 801628646
# 2017-01-25  92.757 116.412  88.120 116.373 655389957
# 2018-01-26 116.412 146.870 114.566 146.860 547883108


# 4. (10pts) 
# Verify that function rutils::to_period() from package 
# rutils produces the same result as agg_s.
# You can use functions unname() and coredata().
# You must use function all.equal().
# hint: use arguments:
# oh_lc=oh_lc[, 1:5]
# end_points=c(0, end_points)

aggs_rutils <- rutils::to_period(oh_lc=oh_lc[, 1:5], end_points=c(0, end_points))
all.equal(unname(coredata(agg_s)), unname(coredata(aggs_rutils)))



############## Part II
# Summary: Calculate the number of trades in an 
# EWMA strategy, and plot the number of trades as 
# a function of the lambda decay parameter.

# 1. (20pts) Calculate the number of trades in an 
# EWMA strategy, using the function simu_ewma() 
# from the lecture code.
# The number of trades is equal to the number of 
# times the EWMA strategy changes its risk position,
# including the first trade when the strategy trades
# out of the initial zero risk position.
# You can use functions NROW(), which(), diff(), 
# and as.numeric().

# Specify EWMA strategy parameters
library(HighFreq)
oh_lc <- rutils::env_etf$VTI
wid_th <- 251
lamb_da <- 0.01

# Source the function simu_ewma() from the file 
# ewma_model.R, using function source().
source("C:/Develop/R/lecture_slides/scripts/ewma_model.R")

# Simulate EWMA strategy using simu_ewma()
ewma_strat <- simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, wid_th=wid_th)
po_sitions <- ewma_strat[, 1]

# Calculate the number of trades
sum(diff(as.numeric(po_sitions)) != 0)
# or
sum(rutils::diff_it(po_sitions) != 0)
# or
(sum(abs(rutils::diff_it(as.numeric(po_sitions))))+1)/2

# You should get the following output:
# [1] 184


# 2. (20pts) Perform an sapply() loop to calculate 
# the number of trades as a function of the lambda 
# decay parameter.

# Specify lamb_das parameters
lamb_das <- seq(0.01, 0.4, 0.01)

number_trades <- sapply(lamb_das, function(lamb_da) {
  po_sitions <- simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, wid_th=wid_th)[, 1]
  sum(rutils::diff_it(po_sitions) != 0)
})  # end sapply

# You should get the following output:
# > number_trades
# [1]   184  194  258  290  364  390  422  466  500  530  546  566  582  616
# [15]  636  666  686  700  720  742  760  788  798  820  842  856  876  892
# [29]  898  902  912  930  954  966  980  996 1002 1008 1024 1036

# Plot the number of trades using plot().
# Your plot should be similar to ewma_number_trades.png
x11()
plot(x=lamb_das, y=number_trades, t="l", 
     main="Number of trades
     as function of the decay parameter lambda")


############## Part III
# Summary: Perform multiple back-tests of momentum strategies 
# for an ETF portfolio, using parallel computing.

# Source the function back_test(), and define the 
# momentum strategy parameters:

library(HighFreq)
source("C:/Develop/R/lecture_slides/scripts/back_test.R")
look_backs <- seq(5, 60, by=5)
re_balance <- "weeks"
bid_offer <- 0.001
# Calculate ETF prices and simple returns
sym_bols <- c("VTI", "IEF", "DBC")
price_s <- rutils::env_etf$price_s[, sym_bols]
price_s <- zoo::na.locf(price_s)
price_s <- na.omit(price_s)
re_turns <- rutils::diff_it(price_s)
# Define aggregation function
agg_fun <- function(re_turns) sum(re_turns)/sd(re_turns)


# 1. (20pts) Perform a parallel apply loop over look_backs, 
# and perform back-tests of momentum strategies using 
# function back_test().
# You should get an xts series of back-test returns called 
# back_tests.
# You can use functions parLapply(), makeCluster(), 
# clusterExport(), rutils::do_call(), cbind(), colnames(), 
# and paste0(). 

# NOTE: the function back_test() in file back_test.R was 
# renamed to back_test_ep() in the newest version of 
# file back_test.R
# Full credit will be given for using either one of the 
# functions.

# initialize compute cluster
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter, 
  varlist=c("back_test_ep", "agg_fun", "re_turns", 
            "price_s", "re_balance", "bid_offer"))

# perform parallel loop over look_backs under Windows
back_tests <- parLapply(clus_ter, look_backs, 
  function(look_back) {
    # perform back-test
    back_test_ep(re_turns=re_turns, 
              price_s=price_s, 
              agg_fun=agg_fun,
              look_back=look_back, 
              re_balance=re_balance, 
              bid_offer=bid_offer)
  })  # end parLapply

# or
back_tests <- parLapply(clus_ter, 
                        X=look_backs, 
                        fun=back_test_ep, 
                        re_turns=re_turns, 
                        price_s=price_s, 
                        agg_fun=agg_fun,
                        re_balance=re_balance, 
                        bid_offer=bid_offer
)  # end parLapply

# perform parallel loop over look_backs under Mac-OSX or Linux
back_tests <- mclapply(look_backs, function(look_back) {
  # perform back-test
  back_test_ep(re_turns=re_turns, 
            price_s=price_s, 
            agg_fun=agg_fun,
            look_back=look_back, 
            re_balance=re_balance, 
            bid_offer=bid_offer)
})  # end mclapply

# or
back_tests <- mclapply(clus_ter, 
                       X=look_backs, 
                       FUN=back_test_ep, 
                       re_turns=re_turns, 
                       price_s=price_s, 
                       agg_fun=agg_fun,
                       re_balance=re_balance, 
                       bid_offer=bid_offer
)  # end parLapply


# stop R processes over cluster under Windows
stopCluster(clus_ter)

# flatten list into xts
back_tests <- rutils::do_call(cbind, back_tests)
colnames(back_tests) <- paste0("back=", look_backs)

# You should get the following output:
# > round(tail(back_tests[, 1:6]), 3)
#            back=5 back=10 back=15 back=20 back=25 back=30
# 2017-12-15  0.297   0.667   0.767   0.899   0.952   0.786
# 2017-12-22  0.291   0.672   0.770   0.904   0.960   0.792
# 2017-12-29  0.302   0.686   0.786   0.920   0.974   0.807
# 2018-01-05  0.317   0.701   0.802   0.935   0.990   0.822
# 2018-01-12  0.318   0.707   0.805   0.939   0.993   0.825
# 2018-01-19  0.335   0.723   0.822   0.955   1.011   0.843


# 2. (10pts) Plot the cumulative strategy pnls as a 
# function of look_back, by plotting the last row 
# of back_tests.
# You can use functions as.numeric(), plot(), NROW(), 
# and cbind().

pro_files <- as.numeric(back_tests[NROW(back_tests), ])
pro_files <- cbind(look_backs, pro_files)
plot(pro_files, t="l", 
     main="Strategy PnL as function of look_back", 
     xlab="look_back (weeks)", ylab="pnl")

# Your plot should be similar to look_back_profile.png


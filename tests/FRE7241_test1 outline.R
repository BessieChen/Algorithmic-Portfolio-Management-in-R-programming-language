#################################
### FRE7241 Test #1 Outline January 30, 2018
#################################
# Max score 110pts

# This file is an outline for test #1, designed to help prepare for it. 
# The actual test #1 file will be more detailed.

############## Part I
# 1. (20pts) 
# Calculate the percentage (log) returns of EuStockMarkets 
# and call them re_turns.
# You can use functions diff() and log().

# load quantmod
library(quantmod)

### write your code here


# For each column of EuStockMarkets, calculate the dates 
# with extreme returns, either above +3% or below -3%.
# You should produce a list of four elements, with each 
# element being a vector of extreme dates.
# You can use functions lapply(), time(), and an 
# anonymous function. 

### write your code here


# Calculate the number of extreme dates for each symbol.
# You can use functions sapply() and NROW(). 

### write your code here



# 2. (10pts) 
# Set all the extreme returns (either above +3% or below -3%) in 
# re_turns to zero.

### write your code here


# 3. (10pts) 
# Calculate the cumulative DAX returns from the first column 
# of re_turns, and call them cum_returns.
# Append 1 to the beginning of cum_returns, so that it has 
# the same number of elements as there are rows in EuStockMarkets.
# Multiply cum_returns by EuStockMarkets[1, 1].
# You can use functions exp(), cumsum(), and c().

### write your code here


# 4. (10pts) Coerce the DAX prices in EuStockMarkets into an xts 
# time series called dax_prices.
# The time index of xts_eustocks should be of class POSIXct, and
# the timezone should be equal to "America/New_York".
# You can use functions coredata(), xts(), index(),
# and date_decimal().

library(lubridate)

### write your code here


# cbind() cum_returns as a second column to dax_prices, 
# and assign the column names "DAX", "DAX clean".
# You can use functions cbind(), colnames(), and c().

### write your code here


# 5. (10pts) Create a dygraphs plot of dax_prices.

### write your code here



############## Part II
# Summary: Select recurring times of day from OHLC time series 
# bar data without using the "T notation".

# load package HighFreq
library(HighFreq)

# The package HighFreq contains the SPY dataset, which is an xts 
# time series containing OHLC prices and trading volumes for the 
# SPY etf. 
# The SPY dataset contains several years of data, collected 
# during trading hours. 
# Each day of the SPY data starts at 09:31 and ends at 16:01 
# (with the exception of some trading days before holidays). 
# Each row of SPY contains a single bar of prices at 1-minute 
# intervals, with Open, High, Low, and Close prices, and Volume.

head(HighFreq::SPY)
tail(HighFreq::SPY)


# 1. (10pts) Select recurring times of day from the SPY time 
# series using the "T notation".
# Select the data for all the days in April 2012, starting from 
# 09:41 to 16:00 using the "T notation", and call it sub_SPY.

### write your code here


# 2. (20pts) Select the same data as in p.1, but without using 
# the "T notation", and call it sub_SPY_bis.
# You can use the functions format(), index(), c(), 
# and paste0(), and the operators "%in%", or ">" and "<".

### write your code here


# Verify that sub_SPY is identical to sub_SPY_bis using 
# the function identical():

identical(sub_SPY, sub_SPY_bis)



############## Part III
# 1. (20pts) Create a vector of weekly POSIXct dates corresponding
# to Mondays at 09:30AM, and call it "mon_days",
# start with the date "2015-02-09", and end at the most recent Monday
# before today (today is defined by Sys.time()),
# set the timezone to "America/New_York",
# first calculate the number of weeks between today and the start date,
# and use that number to create a vector of weekly POSIXct dates,
# use functions Sys.setenv(), as.POSIXct(), difftime() and ceiling(),
# and lubridate function weeks(),

### write your code here


# Convert "mon_days" to the days of the week, using three different methods,
# to verify that all the dates in "mon_days" are indeed Mondays,

# use function weekdays(),

### write your code here

# use function as.POSIXlt(),

### write your code here

# use lubridate function wday(),

### write your code here


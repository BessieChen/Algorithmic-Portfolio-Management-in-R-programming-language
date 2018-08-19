#################################
### FRE7241 Test #1 Solution January 30, 2018
#################################
# Max score 110pts

# The below solutions are examples,
# Slightly different solutions are also possible.

############## Part I
# 1. (20pts) 
# Calculate the percentage (log) returns of EuStockMarkets 
# and call them re_turns.
# You can use functions diff() and log().

# load quantmod
library(quantmod)

re_turns <- diff(log(EuStockMarkets))

# For each column of EuStockMarkets, calculate the dates 
# with extreme returns, either above +3% or below -3%.
# You should produce a list of four elements, with each 
# element being a vector of extreme dates.
# You can use functions lapply(), time(), and an 
# anonymous function. 

extreme_dates <- lapply(re_turns, function(col_umn) {
  time(col_umn)[(col_umn > 0.03) | (col_umn < -0.03)]
})  # end lapply

# You should get the following output:
# > extreme_dates
# $DAX
# [1] 1991.631 1991.638 1992.708 1992.765 1993.527 1994.208 1994.785 1995.742 1997.269 1997.285
# [11] 1997.577 1997.638 1997.692 1997.719 1997.731 1997.835 1997.846 1997.850 1997.900 1997.938
# [21] 1998.015 1998.031 1998.354 1998.427 1998.592 1998.635
# 
# $SMI
# [1] 1991.631 1991.638 1992.708 1992.765 1994.477 1996.200 1996.608 1997.269 1997.581 1997.654
# [11] 1997.665 1997.681 1997.692 1997.846 1997.850 1998.054 1998.362 1998.635
# 
# $CAC
# [1] 1991.631 1991.638 1991.981 1992.273 1992.650 1992.681 1992.708 1992.727 1992.746 1992.765
# [11] 1992.808 1995.365 1995.531 1995.742 1997.419 1997.427 1997.692 1997.835 1997.846 1997.850
# [21] 1997.969 1998.354
# 
# $FTSE
# [1] 1991.631 1992.281 1992.719 1992.723 1992.765 1997.762 1997.835


# Calculate the number of extreme dates for each symbol.
# You can use functions sapply() and NROW(). 

sapply(extreme_dates, NROW)

# You should get the following output:
# DAX  SMI  CAC FTSE 
#  26   18   22    7 


# 2. (10pts) 
# Set all the extreme returns (either above +3% or below -3%) in 
# re_turns to zero.

re_turns[(re_turns > 0.03) | (re_turns < -0.03)] <- 0

# 3. (10pts) 
# Calculate the cumulative DAX returns from the first column 
# of re_turns, and call them cum_returns.
# Append 1 to the beginning of cum_returns, so that it has 
# the same number of elements as there are rows in EuStockMarkets.
# Multiply cum_returns by EuStockMarkets[1, 1].
# You can use functions exp(), cumsum(), and c().

cum_returns <- exp(cumsum(re_turns[, 1]))
cum_returns <- EuStockMarkets[1, 1] * c(1, cum_returns)


# 4. (10pts) Coerce the DAX prices in EuStockMarkets into an xts 
# time series called dax_prices.
# The time index of xts_eustocks should be of class POSIXct, and
# the timezone should be equal to "America/New_York".
# You can use functions coredata(), xts(), index(),
# and date_decimal().

library(lubridate)
dax_prices <- xts(coredata(EuStockMarkets[, 1]),
                  order.by=date_decimal(index(EuStockMarkets),
                                        tz="America/New_York"))


# cbind() cum_returns as a second column to dax_prices, 
# and assign the column names "DAX", "DAX clean".
# You can use functions cbind(), colnames(), and c().

dax_prices <- cbind(dax_prices, cum_returns)
colnames(dax_prices) <- c("DAX", "DAX clean")


# 5. (10pts) Create a dygraphs plot of dax_prices.

dygraphs::dygraph(dax_prices, main="DAX prices")

# Your plot should be similar to dax_prices.png



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

sub_SPY <- HighFreq::SPY["2012-04"] ["T09:41:00/T16:00:00"]


# 2. (20pts) Select the same data as in p.1, but without using 
# the "T notation", and call it sub_SPY_bis.
# You can use the functions format(), index(), c(), 
# and paste0(), and the operators "%in%", or ">" and "<".

sub_SPY_bis <- SPY["2012-04"]
in_dex <- format(index(sub_SPY_bis), "%H:%M")
in_dex <- (in_dex > "09:40") & (in_dex < "16:01")
sub_SPY_bis <- sub_SPY_bis[in_dex, ]

# or
sub_SPY_bis <- SPY["2012-04"]
in_dex <- format(index(sub_SPY_bis), "%H:%M")
in_dex <- in_dex %in% c(paste0("09:", 31:40), "16:01")
sub_SPY_bis <- sub_SPY_bis[!in_dex, ]


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

Sys.setenv(TZ="America/New_York")
start_date <- as.POSIXct("2015-02-09 09:30:00")
end_date <- Sys.time()

num_weeks <- ceiling(difftime(end_date, start_date, units="weeks"))

mon_days <- start_date + lubridate::weeks(0:num_weeks)
mon_days <- mon_days[(mon_days <= end_date)]
head(mon_days)
tail(mon_days)

# Convert "mon_days" to the days of the week, using three different methods,
# to verify that all the dates in "mon_days" are indeed Mondays,

# use function weekdays(),
weekdays(mon_days)

# use function as.POSIXlt(),
as.POSIXlt(mon_days)$wday

# use lubridate function wday(),
lubridate::wday(mon_days, TRUE)



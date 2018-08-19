# Algorithmic-Portfolio-Management-in-R-programming-language
The course, authored by Prof. Jerzy in NYU, applies the R programming language to momentum trading, statistical arbitrage (pairs trading), and other active portfolio management strategies. The course implements volatility and price forecasting models, asset pricing and factor models, and portfolio optimization. The course will apply machine learning techniques, such as backtesting (cross-validation) and parameter regularization (shrinkage). 

## The following are some interesting cases I learnt from class:



### Case one:
Plot the cumulative maximum of the adjusted close prices from utils::env_etf$VTI:
![drawdown_plot](https://user-images.githubusercontent.com/33269462/44305868-5b0c2080-a351-11e8-9473-5482ff133bd6.png)

![drawdown_vti](https://user-images.githubusercontent.com/33269462/44305874-6cedc380-a351-11e8-841d-581675b926ff.png)



### Case two:
Plot the sub_portfolios from worst to best (based on final price) using a color ramp from red to blue.
![sp500_sub_portfolios](https://user-images.githubusercontent.com/33269462/44305911-0d43e800-a352-11e8-9a95-37cb9c39443f.png)



### Case three:
Perform a rolling portfolio optimization over annual intervals, calculate optimized portfolio weights in each year, and apply them to out-of-sample returns in the following year, at the end, plot the cumulative returns of this max sharpe portfolio strategy.
![backtest_sharpe](https://user-images.githubusercontent.com/33269462/44305943-99560f80-a352-11e8-98d6-03394244843b.png)



### Case four:
Calculate efficient portfolios with the lowest variance given a fixed target return.

Plot the efficient frontier and the capital market lines for different values of the fixed target return.




<img width="954" alt="interactive_plot beixi chen" src="https://user-images.githubusercontent.com/33269462/44305772-627dfa80-a34e-11e8-9398-d95ac9d385fc.png">

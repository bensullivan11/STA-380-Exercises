install.packages('quantmod')
install.packages('mosaic')
install.packages('foreach')
library(mosaic)
library(quantmod)
library(foreach)


###### First sim for a relativly diverse/ well hedged  portfolio

# Tan and Grid are sustainable - because biden politics
#SPY is S&P index, heavily weighted towards this - tried and true
#GLD is gold - hedges against crazy market movement
# DBA is agraculture - you always need food
#DBEF is a hedged equity fund - another amrket fund
#DRN is realestate - moves with economy & am betting on bull market


portfolio_div = c('DBEF', 'DBA', 'DRN','GLD', 'TAN', 'GRID', 'SPY')

prices = getSymbols(portfolio_div, from = '2016-08-06')

for(ticker in portfolio_div){
  expr = paste0(ticker, 'a = adjustOHLC(', ticker, ')')
  eval(parse(text=expr))
}

all_returns = cbind(ClCl(DBEFa), ClCl(DBAa),ClCl(DRNa),ClCl(GLDa),ClCl(TANa),ClCl(GRIDa),ClCl(SPYa))
all_returns = as.matrix(na.omit(all_returns))
N = nrow(all_returns)

wealth = 100000
simulation = foreach(i=1:5000, .combine = 'rbind') %do% {
  total_wealth = wealth
  my_weights = c(.1,.1,.1,.1,.1,.1,.4)
  holdings = total_wealth * my_weights
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days){
    return.today = resample(all_returns, 1, orig.ids = FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
    wealthtracker
}
head(simulation)
hist(simulation[,n_days], 25)

simulation
plot(wealthtracker, type = 'l')

#This is the VAR for the portfolio
quantile(simulation[,n_days]- wealth, prob=0.05)

total_wealth
holdings

##### Sustainable portfolio

## all indexes are for clean energy, with the biden admin creating policies that are enviro-oriented these could work out


portfolio_sus = c('FAN', 'CNRG', 'ERTH','ACES', 'TAN')

prices = getSymbols(portfolio_sus, from = '2016-08-06')

for(ticker in portfolio_sus){
  expr = paste0(ticker, 'a = adjustOHLC(', ticker, ')')
  eval(parse(text=expr))
}

all_returns = cbind(ClCl(FANa), ClCl(CNRGa),ClCl(ERTHa),ClCl(ACESa),ClCl(TANa))
all_returns = as.matrix(na.omit(all_returns))
N = nrow(all_returns)

wealth = 100000
simulation = foreach(i=1:5000, .combine = 'rbind') %do% {
  total_wealth = wealth
  my_weights = c(.2,.2,.2,.2,.2)
  holdings = total_wealth * my_weights
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days){
    return.today = resample(all_returns, 1, orig.ids = FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}
head(simulation)
hist(simulation[,n_days], 25)

simulation
plot(wealthtracker, type = 'l')

#This is the VAR for the portfolio
quantile(simulation[,n_days]- wealth, prob=0.05)

total_wealth
holdings

##### Tech indexes
## With the increase in technology this could be a good choice. 

portfolio_tech = c('SOXX', 'SKYY', 'CIBR','HACK', 'CLOU', 'BLOK','FINX','XNTK')

prices = getSymbols(portfolio_tech, from = '2016-08-06')

for(ticker in portfolio_tech){
  expr = paste0(ticker, 'a = adjustOHLC(', ticker, ')')
  eval(parse(text=expr))
}

all_returns = cbind(ClCl(SOXXa), ClCl(SKYYa),ClCl(CIBRa),ClCl(HACKa),ClCl(CLOUa),ClCl(BLOKa),ClCl(FINXa),ClCl(XNTKa))
all_returns = as.matrix(na.omit(all_returns))
N = nrow(all_returns)

wealth = 100000
simulation = foreach(i=1:5000, .combine = 'rbind') %do% {
  total_wealth = wealth
  my_weights = c(.125,.125,.125,.125,.125)
  holdings = total_wealth * my_weights
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days){
    return.today = resample(all_returns, 1, orig.ids = FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}
head(simulation)
hist(simulation[,n_days], 25)

simulation
plot(wealthtracker, type = 'l')

#This is the VAR for the portfolio
quantile(simulation[,n_days]- wealth, prob=0.05)

total_wealth
holdings





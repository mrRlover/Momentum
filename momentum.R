options(scipen = 4)

list <- read.csv("constituents_csv.csv")
tickers <- as.character(list[,"Symbol"])

n = 1

for(ticker in tickers) {
  if(grepl("\\.", ticker) == TRUE) {
    ticker <- gsub("\\.", "-", ticker)
    tickers[n] <- ticker
  }
  n <- n + 1
}

# yahoo finance tickers uses - not . e.g. BRK.B is BRK-B on yahoo finance
# this is because . represents exchange e.g. NPN.JO is Naspers on the JSE on yahoo finance
# for loop replace . in tickers with - in order for download not to return an error

remove(n)

path = paste0(getwd(), "/Data/SP 500")
setwd()

load_data(tickers = tickers, startdate = "2000-01-01", enddate = "2018-12-31")

combine_csv(file_path = path)

stocks <- read.csv("stocks.csv")
dates <- ymd(stocks[, "Date"])
stocks <- stocks[,2:ncol(stocks), drop = F]
stocks <- as.xts(stocks, order.by = dates)

returns <- calculate_returns(stocks, periods = "quarterly")
rank_stocks(file_path = "~/Data/", returns = returns)
form_portfolios(file_path = "~/Data/rankings", stocks = stocks,
                holding_period = "quarterly")
# file_path => directory where csv files with portfolios will be saved
# stocks => xts object with stock price data
# wealth => initial amount to be invested; default value is 100000 
# create_csv = save portfolio of each quarter to csv file; default is TRUE
# files get saved in folder "porfolios" which will be inside the rankings folder







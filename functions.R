library(quantmod)
library(lubridate)

load_data <- function(tickers, startdate, enddate){
  
  # function to automate download of stock price data
  # fuction requires quantmod package
  # data is download from yahoo
  # function uses getSymbols() from quantmod package
  
  # tickers is a vector which contains stock tickers
  # startdate takes a date as a string in the form of "YYYY-MM-DD" e.g. "2000-12-13"
  # enddate takes a date as a string in the form of "YYYY-MM-DD" e.g. "2018-12-13"
  
  wd <- getwd()
  
  path_file <- paste0(wd, "/Data")
  
  if(file.exists(path_file)==F){
    dir.create(path_file)
  }
  
  path_file <- paste0(path_file, "/SP 500")

  
  if(file.exists(path_file)==F){
    dir.create(path_file)
  }
  # above checks if the directory exists. If it does not, it creates it
  # this directory is where the csv files with the stock price data will be stored
  # ~ represents first folder e.g. "C/User/MK/Documents" first level here is Documents
  
  setwd(path_file)
  
  for(ticker in tickers){
    temp <- getSymbols(ticker, from = startdate, to = enddate, auto.assign = F)
    for(numcol in 1:ncol(temp)){
      column <- colnames(temp)[numcol]
      if(grepl("\\.", column)==T){
        column <- gsub(paste0(ticker,"."), "", column)
        colnames(temp)[numcol] <- column
      }
    }
    
    date_index <- as.matrix(as.character(index(temp)))
    temp <- cbind(date_index, as.matrix(temp))
    colnames(temp)[1] <- "Date"
    filename <- paste0("", ticker, ".csv")
    
    if(file.exists(filename)==F){
      write.csv(temp, filename, row.names = F)
    }
  }
  
  # for loop to loop through each ticker in tickers and downloads the stock price data
  # paste0 concatenates the ticker name stored in ticker with .csv to create ticker.csv 
  # this is stored to filename and this will be the file name of the csv file
  
  setwd(wd) 
  # sets working directory to the original working directory
  
}

combine_csv <- function(file_path){
  # function combines the csv files downloaded with load_data into one csv file
  
  # file_path is the path where the csv files with the stock price data is saved
  
  # file_path has to be a string
  
  wd <- getwd()
  
  path_file <- paste0(wd, "/Stocks")
  
  if(file.exists(path_file)==F){
    dir.create(path_file)
  }
  
  setwd(path_file)
  
  if(substr(file_path, nchar(file_path), nchar(file_path)) != "/"){
    file_path <- paste0(as.character(file_path),"/")
  }
  
  # above checks if / has been added to the end of the path e.g. "C:/User/MK/Documents/"
  
  # if not (e.g. file_path = "C:/User/MK/Documents") / is added to the end
  
  # need when reading csv files
  
  stocks <- matrix(nrow=0,ncol=0) 
  # initialise matrix where data will be stored. Empty matrix
  
  tickers = list.files(file_path, pattern = "*.csv")
  # get the names of all the files with stock price data
  num.rows = 0
  # initialise counter of rows

  for (ticker in tickers){
    fp <- paste0(as.character(file_path),"", as.character(ticker))
    df <- as.matrix(read.csv(fp))
    if(nrow(df) > num.rows){
      num.rows <- nrow(df)
    }
  }
  
  # fp = file_path/ticker.csv e.g. "C:/User/MK/Documents/AAPL.csv"
  
  # for loop goes counts the rows of each csv file
  # not all stocks in the SP 500 were listed in 2000
  # if it listed later, it will have fewer rows than a stock that listed in 2000
  # num.rows will be the number of rows of the oldest listing in the dataset
  # needed later for cbinding
  
    for (ticker in tickers) {
      fp <- paste0(as.character(file_path),"/", as.character(ticker))
      df <- as.matrix(read.csv(fp))
      if(any((colnames(df)=="Adjusted"))==T){
        colref <- which((colnames(df)=="Adjusted"))
      }else{
        colref <- which(colnames(df)=="Adj.Close")
      } 
      df <- df[,colref, drop = F]
      colnames(df) <- gsub("\\..*","",ticker)
    
      # for loop imports the stock price data, extracts the adjusted close
      # gsub() removes the .csv part of ticker to name the column
      # column name will be AAPL for example
      
      if (nrow(stocks) == 0) {
        if(nrow(df)==num.rows){
        stocks <- df
        }else{
          n = num.rows - nrow(df)
          vec <- 1:n
          for (rows in 1:n) vec[rows] <- NA
          df <- rbind(as.matrix(vec), df)
          stocks <- df
        }
        # if stocks is empty, assign df to it
      }else{
        if(nrow(df)==num.rows){
          stocks = cbind(stocks[,, drop = F], df[,gsub("\\..*","",ticker), drop = F])
          dates = as.matrix(read.csv(fp))
          
          # if the number of rows of the df equals the number of rows of the oldest listing
          # cbind
          # import csv file and store to dates. This will be used to extract dates
        }else{
          n = nrow(stocks) - nrow(df)
          new.rows <- data.frame(1:n)
          for(i in 1:nrow(new.rows)){
            new.rows[i,1] <- NA
          }
          
          # create a data frame and fill it with NAs
        colnames(new.rows) <- colnames(df)
        df <- rbind(new.rows, df)
        # df now has the same number of rows as the oldest listing
        # observations before listing are NAs
        # necessary as cbind requires equal number of rows
        stocks = cbind(stocks[,, drop = F], df[,gsub("\\..*","",ticker), drop = F])
      }
    }
    print(ticker)
    # shows progress
  }
  
  # stocks is now a single dataframe with all stock price data in it
  
  df <- as.matrix(read.csv(fp))
  df <- dates[,'Date', drop = F]
  stocks = cbind(df[,'Date', drop = F], stocks[,, drop = F])
  write.csv(stocks, "Stocks.csv", row.names = F)
  
  # extract dates and add to the stocks dataframes
  
  setwd(wd)
  
}

calculate_returns <- function(prices, periods){
  
  # function calulates the period returns for mulitple stocks in an xts object, matrix or dataframe
  
  # prices is an xts object, matrix, or dataframe with stock price data
  
  # periods is the period to calculate e.g. "monthly", "quaterly", "yearly"
  
  df <- prices[, colSums(is.na(prices)==F) == nrow(prices), drop = F]
  
  # colSums counts the number of non-NAs in the column
  
  # if it matches the number of rows, extract the column
  
  # only columns with NAs are extracted
  
  ini_df <- periodReturn(df[,1,drop=F], period = periods)
  
  # initialise object with period returns on first column of df
  
  # arbitrary placeholder
  
  for(i in colnames(prices)){
    temp <- suppressWarnings(periodReturn(prices[,i], period = periods))
    colnames(temp) <- i
    ini_df <- cbind(ini_df, temp)
    remove(temp)
  }
  
  # calculate period returns
  
  # suppresswarnings to avoid printing of warnings due to NAs
  
  main_df <- ini_df[,-1,drop=F]
  # remove placeholder column
  return(main_df)
  # return final dataframe 
}

rank_stocks <- function(file_path, returns){
  
  # function to rank the returns and sort the returns
  
  # final output is a csv file with bottom losers decile and top winners decile
  
  # stocks are ranked on their returns in the past 3 months
  
  # file_path is the location where the csv files with rankings in each period will be saved
  
  # returns is an is an xts object, martrix or dataframe with period returns
  
  if(substr(file_path, nchar(file_path), nchar(file_path)) == "/"){
    file_path <- substr(file_path, 1, nchar(file_path)-1)
  }
  
  wd <- getwd()
  
  for (num in 1:nrow(returns)){  
    
  # nrow is the number of periods, e.g. number of quarters
    
    temp <- returns[num,,drop = F]
    # extract first period e.g. first quarter
    
    # one row, multiple columns
    
    temp <- temp[, colSums(is.na(temp)) != nrow(temp), drop = F]
    
    # is.na(temp) if NA will be T (aka 1) colSums will be 1
    # is colSums (e.g. 1) not equal to the nrows of temp (1)
    # if it is equal, it has an NA
    # if not equal it has no NA extract it
    
    test <- sort(as.numeric(temp))
    
    # sort the returns from lowest to highest
        
    test <- as.matrix(test)
    
    colnames(test) <- as.character(index(temp))
    rownames(test) <- colnames(temp)
    
    for (i in 1:nrow(test)){
      r <- temp[,which(temp[1,] == test[i,])]
      if(ncol(r) == 1){  
        rownames(test)[i] <- colnames(r)
      }else{
        if(exists("ref")==F){ ref <- i}
        if(rownames(test)[ref] != colnames(r)[1]){
          rownames(test)[i:(i+ncol(r)-1)] <- colnames(r)
        }
      }
    }
    
    # for loop above matches the returns in the sorted matrix to the returns in temp
    # this is to get which company had the return
    # if the return matches more than one company (i.e. > 1 company has the exact same return)
    # nrow of r will be > 1
    # create a reference value assigning i to ref
    # if exists checks if the ref variable is already assigned. 
    # this "protects" it from being reassigned after each iteration
    # reference value is the point (row) at which the return matched multiple stocks
    # if the rowname of this row is not equal to the colname of the first column in r
    # name the rows that match return with the colnames of r
    # since returns are sorted all identical returns will be in a row
    # exact match not relevant, neither is order
    # identical returns unlikely unless its a zero
    # this will not work if there are > 1 incidence of identical returns
    # due to precision of calculations (large n of decimals) non-zero identicality is low prob

    subDir <- "rankings"
    if(file.exists(file_path)==F){
      dir.create(file.path(file_path))
    }
    dir.create(file.path(file_path, subDir), showWarnings = F)
    setwd(file.path(file_path, subDir))
    
    # create new folder in directory supplied to the function
    
    rank <- test[c(1:10,length(test) - (9:0)),,drop=F]
    # extract bottom 10 losers and top 10 winners
    # losers at the top and winners at bottom as per Jegedeesh and Titman (1993)

    fileOutput <- paste0(colnames(rank)[1], ".csv")
    write.csv(rank, file = fileOutput, row.names = TRUE)
  }
  return(paste0("The files are stored in ", getwd()))
  setwd(wd)
}

form_portfolios <- function(file_path, stocks, wealth=100000, holding_period, create_csv=TRUE){
  
  # function to form portfolios based on rankings created with rank_stocks
  # buy 10 winners, equally weighted, no rebalancing, no skipping of one week
  
  # file_path argument is the directory where the files with rankings are located 
  # stocks is an is an xts object with stock price data
  # wealth is the initial investment
  # holding_period is the how long the momentum portfolios will be held before selling
  # create_csvs takes either TRUE or FALSE. Default is TRUE. If true, each portfolio will be saved as a csv. 
  
  if(is.Date(index(stocks))==F){
    stop("argument stocks requires an xts object with Dates as row index")
  }
  # stop stops the execution of the function and returns the error message
  # stocks needs to be an xts object for matching and subsetting using dates
  
  wd <- getwd()
  
  returns <- calculate_returns(stocks, periods = holding_periods)
  # use previously defined function to create returns
  
  setwd(file_path)
  my_files <- list.files(file_path, pattern = "\\.csv")
  # get names of csv files to be imported
  my_data <- lapply(my_files, read.csv)
  # import csv files and store in a list
  names(my_data) <- gsub("\\.csv$", "", my_files)
  
  returns <- calculate_returns(stocks, periods = holding_periods)
  
  setwd(file_path)
  my_files <- list.files(file_path, pattern = "\\.csv")
  # get names of csv files to be imported
  my_data <- lapply(my_files, read.csv)
  # import csv files and store in a list
  names(my_data) <- gsub("\\.csv$", "", my_files)
  
  for(i in 1:length(my_data)){
    
    # if wealth argument is not supplied set the default to 10 000 000
    # store first period wealth as w1
    # else if the wealth argument is supplied check w1 exists if not assign wealth to it
    # w1 will only not exist in first iteration (if wealth is supplied)
    # if statements needed to "protect" w1 and default wealth assignment from for loop iterations
    
    port <- my_data[[i]]
    port <- port[-(1:10),,drop = F] # cut losers
    
    endperiod <- ymd(gsub("\\.", "-", substr(colnames(port)[2], 2, nchar(colnames(port)[2]))))
    
    # because the values are coming from a list, the colnames will be X and X2000.03.31
    # X will have tickers, X2000-03-31 will have period returns
    # this code cuts the X and replaces the dots with dashes - turns into 2000-03-31
    
    if(endperiod != index(returns[nrow(returns)])){
      port$buyprice <- t(stocks[which(index(stocks) == endperiod) + 1, as.character(port$X)])
      port$sellprice <- t(stocks[index(returns[which(index(returns) == endperiod) + 1]), as.character(port$X)])
      port$shares <- (wealth/nrow(port)) / port$buyprice
      port$beg.value <- port$shares * port$buyprice
      port$end.value <- port$shares * port$sellprice
      port$returns <- log(port$end.value/port$beg.value)
      
      # line 317 : if statement checks if the date in endperiod is equal to the last date in returns
      # if it is then no portfolio will be formed as this will be the last quarter - no data past this
      # line 318 : which(index(stocks) == endperiod) + 1 > e.g. quarter ends 2000-03-31
      # ; + 1 = next row in stocks
      # so this will get the row number of the trading day of the next quarter in stocks
      # take this row number and extract the rows to get the prices of all stocks prices in that row
      # this will be the price paid for the stocks
      
      # line 319 : index(returns[which(index(returns) == endperiod) + 1]) get the end date of the next quarter
      # use this to extract the prices from stocks to get closing prices
      
      port$beg.weights <- port$beg.value / wealth
      port$end.weights <- port$end.value / sum(port$end.value)
      
      wealth <- sum(port$end.value)
      
      if(exists("tot.rets")==F){
        tot.rets <- as.matrix(log(sum(port$end.value)/sum(port$beg.value)))
        row.index <- index(returns)[which(index(returns) == endperiod) + 1]
        rownames(tot.rets) <- as.character(row.index)
      }else{
        temp <- as.matrix(log(sum(port$end.value)/sum(port$beg.value)))
        row.index <- index(returns)[which(index(returns) == endperiod) + 1]
        rownames(temp) <- as.character(row.index)
        tot.rets <- rbind(tot.rets, temp)
      }
      colnames(port)[2] <- as.character(endperiod)
      colnames(tot.rets) <- "total returns"
      
      if(create_csv==T){
        dir.create(file.path(file_path, "portfolios"), showWarnings = FALSE)
        setwd(file.path(file_path, "portfolios"))    
        fileOutput <- paste0(endperiod, ".csv")
        write.csv(port, file = fileOutput, row.names = F)
      }

    }
    if(exists("rich")==F){
      rich <- data.frame(wealth)
      rownames(rich) <- as.character(endperiod)
    }else{
      rich <- rbind(rich, wealth)
      rownames(rich)[i] <- as.character(endperiod)
    }
  }
  
  write.csv(tot.rets, file = "total returns.csv", row.names = T)

  setwd(wd)
  
  colnames(rich) <- "wealth"
  return(rich)
}

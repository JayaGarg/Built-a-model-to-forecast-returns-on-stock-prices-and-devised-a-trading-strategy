rm(list=ls())
setwd("C:\\Jaya\\GL\\FRA\\GA")

#load the data
library(readxl)
regression_data <- read_excel("regression_data.xlsx",sheet = "data")

metrics_data <- read_excel("regression_data.xlsx",sheet = "Metrics")
data <- regression_data[-c(1:2),]
str(data)
data$Date <- as.Date(data$Date)

for(i in (1:length(metrics_data$Date_Starting))){
  Date1 = metrics_data$Date_Starting[i]
  Date2 = metrics_data$Week_Ending[i]
  Date1
  data_mod <- with(data, data[(Date >= Date1 & Date <= Date2), ])
  attach(data_mod)
  mod <- lm(Return~Lagged_Return, data = data_mod)
  summary(mod)
  data_mod$Pred_Return <- round((round(mod$coefficients[1],6) + (round(mod$coefficients[2],6) * data_mod$Return)),6)
  data_mod$Profit <- ifelse(data_mod$Pred_Return > 0,diff(data_mod$Adj_Close),-diff(data_mod$Adj_Close))
  n <- length(data_mod$Profit)
  data_mod$Ini_wealth <- 100
  data_mod$units <- data_mod$Ini_wealth/data_mod$Adj_Close
  data_mod$Actual_profit <- data_mod$Profit*data_mod$units
  data_mod$Fin_wealth <- data_mod$Ini_wealth+data_mod$Actual_profit
  data_mod$Dumb_trader <- 100
  unit_per100 <- 100/data_mod$Adj_Close[1]
  for(j in 2:(length(data_mod$Fin_wealth))){
    data_mod$Ini_wealth[j] <- data_mod$Fin_wealth[j-1]
    data_mod$units[j] <- data_mod$Ini_wealth[j]/data_mod$Adj_Close[j]
    data_mod$Actual_profit[j] <- data_mod$Profit[j]*data_mod$units[j]
    data_mod$Fin_wealth[j] <- data_mod$Ini_wealth[j]+data_mod$Actual_profit[j]
    data_mod$Dumb_trader[j] <- unit_per100*data_mod$Adj_Close[j]
  }
  #Metrics
  metrics_data$alpha[i] <- round(mod$coefficients[1],6)
  metrics_data$beta[i] <- round(mod$coefficients[2],6)
  metrics_data$R_square[i] <- summary(mod)$r.squared
  metrics_data$Pattern[i] <- ifelse(metrics_data$beta[i]>0,"M","C")
  metrics_data$profit_unit[i] <- sum(data_mod$Profit[-n])
  metrics_data$Ini_wealth[i] <- 100
  metrics_data$Term_wealth[i] <- round(data_mod$Ini_wealth[n],4)
  metrics_data$Profit_100dollar[i] <- metrics_data$Term_wealth[i] - 100
  metrics_data$eval_Dumb_trader[i] <- round(data_mod$Dumb_trader[n],4)
  #out of sample performance
  Date3 = metrics_data$Week_Ending[length(metrics_data$Date_Starting)]
  data_mod_perf <- with(data, data[(Date >= Date2 & Date <= Date3), ])
  data_mod_perf$Pred_Return <- round((round(mod$coefficients[1],6) + (round(mod$coefficients[2],6) * data_mod_perf$Return)),6)
  data_mod_perf$Profit <- ifelse(data_mod_perf$Pred_Return > 0,diff(data_mod_perf$Adj_Close),-diff(data_mod_perf$Adj_Close))
  n_perf <- length(data_mod_perf$Profit)
  data_mod_perf$Ini_wealth <- 100
  data_mod_perf$units <- data_mod_perf$Ini_wealth/data_mod_perf$Adj_Close
  data_mod_perf$Actual_profit <- data_mod_perf$Profit*data_mod_perf$units
  data_mod_perf$Fin_wealth <- data_mod_perf$Ini_wealth+data_mod_perf$Actual_profit
  data_mod_perf$Dumb_trader <- 100
  unit_per100_perf <- 100/data_mod_perf$Adj_Close[1]
  for(k in 2:(length(data_mod_perf$Fin_wealth))){
    data_mod_perf$Ini_wealth[k] <- data_mod_perf$Fin_wealth[k-1]
    data_mod_perf$units[k] <- data_mod_perf$Ini_wealth[k]/data_mod_perf$Adj_Close[k]
    data_mod_perf$Actual_profit[k] <- data_mod_perf$Profit[k]*data_mod_perf$units[k]
    data_mod_perf$Fin_wealth[k] <- data_mod_perf$Ini_wealth[k]+data_mod_perf$Actual_profit[k]
    data_mod_perf$Dumb_trader[k] <- unit_per100_perf*data_mod_perf$Adj_Close[k]
  }
  #Metrics for out-of-sample 
  metrics_data$sum_profit_perf[i] <- sum(data_mod_perf$Profit[-n_perf])
  metrics_data$Term_wealth_perf[i] <- round(data_mod_perf$Ini_wealth[n_perf],4)
  metrics_data$Profit_100dollar_perf[i] <- metrics_data$Term_wealth_perf[i]- 100
  metrics_data$eval_Dumb_trader_perf[i] <- round(data_mod_perf$Dumb_trader[n_perf],4)
}


metrics_data$sum_profit_perf[i] <- NA
metrics_data$Term_wealth_perf[i] <- NA
metrics_data$Profit_100dollar_perf[i] <- NA
metrics_data$eval_Dumb_trader_perf[i] <- NA

write.csv(metrics_data,file = "metrics_part5.csv",row.names=FALSE, na="")

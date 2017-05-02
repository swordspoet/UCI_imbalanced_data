# Data Manipulation
# combine factor levels with less than 5% values
library(mlr)
for(i in names(cat_train)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_train[[i]])) < p ))
  levels(cat_train[[i]])[levels(cat_train[[i]]) %in% ld] <- "Other"
}


for(i in names(cat_test)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_test[[i]])) < p ))
  levels(cat_test[[i]])[levels(cat_test[[i]]) %in% ld] <- "Other"
}

# "nlevs"参数：返回每列下维度的个数，测试集和训练集是否匹配

summarizeColumns(cat_train)[,"nlevs"] 
summarizeColumns(cat_test)[,"nlevs"]

num_train[,.N,age][order(age)]
num_train[,.N,wage_per_hour][order(-N)]

num_train[,age:= cut(x = age,breaks = c(0,30,60,90),
                     include.lowest = TRUE,
                     labels = c("young","adult","old"))]
num_train[,age := factor(age)]

num_test[,age:= cut(x = age,breaks = c(0,30,60,90),
                    include.lowest = TRUE,
                    labels = c("young","adult","old"))]
num_test[,age := factor(age)]

num_train[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_train[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_train[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_train[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

num_test[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_test[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_test[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_test[,dividend_from_Stocks := ifelse(dividend_from_Stocks ==0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

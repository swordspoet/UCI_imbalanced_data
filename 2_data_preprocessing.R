## 数据预处理

# 查看目标变量取值，返回"-50000" "+50000"
unique(train$income_level)
unique(test$income_level)
# 替换目标变量取值为0,1，ifelse(test,yes,no)
train[,income_level:= ifelse(income_level == "-50000",0,1)]
test[,income_level:= ifelse(income_level == "-50000",0,1)]
# 计算目标变量各个取值占比
round(prop.table(table(train$income_level))*100)
# 设置变量的对应类别为数值型和名义型
factcols <- c(2:5,7,8:16,20:29,31:38,40)
numcols <- setdiff(1:41,factcols)
# lapply的.SD的用法
train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]
test[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

cat_train <- train[,factcols, with=FALSE]
cat_test <- test[,factcols,with=FALSE]

num_train <- train[,numcols,with=FALSE]
num_test <- test[,numcols,with=FALSE]
# 移除以节省内存
rm(train,test) 
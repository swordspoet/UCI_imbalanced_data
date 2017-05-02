# 加载包
library(caret)
library(data.table)
library(mlr)
library(xgboost)

# 加载数据集
train <- fread("E:/R/imbalancedata/train.csv",na.string=c(""," ","?","NA",NA)) 
test <- fread("E:/R/imbalancedata/test.csv",na.string=c(""," ","?","NA",NA))
table(is.na(train));table(is.na(test))
#convert data frame to data table
setDT(train)
setDT(test)
# 将train,test数据切分为数值型和类别型
factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
numcols <- setdiff(1:40,factcols)
cat_train <- train[,factcols, with=FALSE];cat_test <- test[,factcols,with=FALSE]
num_train <- train[,numcols,with=FALSE];num_test <- test[,numcols,with=FALSE]
# 去掉数值型(num)数据中高度相关的变量
ax <- findCorrelation(cor(num_train), cutoff = 0.7)
num_train <- num_train[,-ax,with=FALSE];num_test <- num_test[,-ax,with=FALSE]
# 处理类别型(cat)数据中的遗漏值
mvtr <- sapply(cat_train, function(x){sum(is.na(x))/length(x)}*100)
mvte <- sapply(cat_test, function(x){sum(is.na(x)/length(x))}*100)
# 将遗漏率小于5%的列单独挑选出来
cat_train <- subset(cat_train, select = mvtr < 5 )
cat_test <- subset(cat_test, select = mvte < 5)
cat_train[is.na(cat_train)] <- "Missing";cat_test[is.na(cat_test)] <- "Missing" 
# 合并数值型和分类型数据
d_train <- cbind(num_train, cat_train);d_test <- cbind(num_test, cat_test)
rm(train,test,num_train,num_test,cat_train,cat_test)
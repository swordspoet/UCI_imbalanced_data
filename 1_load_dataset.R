## 加载包和数据集
# na.string=c()定义多个变量为NA
library(data.table)
train <- fread("E:/R/imbalancedata/train.csv",na.string=c(""," ","?","NA",NA)) 
test <- fread("E:/R/imbalancedata/test.csv",na.string=c(""," ","?","NA",NA))
# 查看数据维度（dim）、各变量属性和取值范围（str）、打开数据（view）
dim(train);str(train);View(train)
dim(test);str(test);view(test)

train[1:5]
test[1:5]

## Data cleaning
table(is.na(num_train))
table(is.na(num_test))

# 过滤高度相关的变量
library(caret)
x <- cor(num_train)
ax <- findCorrelation(x=cor(num_train), cutoff=0.7)

num_train <- num_train[,-ax,with=FALSE]
num_test <- num_test[,weeks_worked_in_year := NULL]


#检查每一列数据的遗漏情况
mvtr <- sapply(cat_train, function(x){sum(is.na(x))/length(x)})*100
mvte <- sapply(cat_test, function(x){sum(is.na(x)/length(x))}*100)

mvtr
mvte

#然后我们发现有的列甚至有超过50%的数据遗漏，这有可能是由于采集数据难度所致
#将遗漏率小于5%的列挑选出来
cat_train <- subset(cat_train, select = mvtr < 5 )
cat_test <- subset(cat_test, select = mvte < 5)

#对于cat_train与cat_test中剩下的遗漏值，比较好的办法是将其标记为“Unavailable”

cat_train <- cat_train[,names(cat_train) := lapply(.SD, as.character),.SDcols = names(cat_train)]

seq_along(cat_train)

for (i in seq_along(cat_train)) set(cat_train, i=which(is.na(cat_train[[i]])), j=i, value="Unavailable")

cat_train <- cat_train[, names(cat_train) := lapply(.SD,factor), .SDcols = names(cat_train)]

cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, as.character), .SDcols = names(cat_test)]

for (i in seq_along(cat_test)) set(cat_test, i=which(is.na(cat_test[[i]])), j=i, value="Unavailable")

cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, factor), .SDcols = names(cat_test)]
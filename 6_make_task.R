library(mlr)
d_train <- cbind(num_train, cat_train)
d_test <- cbind(num_test, cat_test)

# convert to factors
fact_cols <- colnames(d_train)[sapply(d_train, is.character)]
integer_cols <- colnames(d_train)[sapply(d_train, is.integer)]
num_cols <- colnames(d_train)[sapply(d_train, is.numeric)]

for(i in fact_cols) set(d_train,j=i,value = factor(d_train[[i]]))
for(i in integer_cols) set(d_train,j=i,value = factor(d_train[[i]]))
for(i in num_cols) set(d_train,j=i,value = factor(d_train[[i]]))

for(i in fact_cols) set(d_test,j=i,value = factor(d_test[[i]]))
for(i in integer_cols) set(d_test,j=i,value = factor(d_test[[i]]))
for(i in num_cols) set(d_test,j=i,value = factor(d_test[[i]]))

train.task <- makeClassifTask(data = d_train, target = "income_level")
test.task <- makeClassifTask(data = d_test, target = "income_level")
train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)

var_imp <- generateFilterValuesData(train.task, method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)

#下采样 上采样 SMOTE

train_under <- undersample(sample_train.task, rate = 0.1)
table(getTaskTargets(train_under))

train_over <- oversample(sample_train.task, rate = 15)
table(getTaskTargets(train_over))

system.time(train_smote <- smote(train.task, rate = 15, nn=3))
table(getTaskTargets(train_smote))

d_train <- cbind(num_train, cat_train)
d_test <- cbind(num_test, cat_test)

train.task <- makeClassifTask(data = d_train, target = "income_level")
test.task <- makeClassifTask(data = d_test, target = "income_level")

train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)

var_imp <- generateFilterValuesData(train.task, method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)

#下采样 上采样 SMOTE

train_under <- undersample(train.task, rate = 0.1)
table(getTaskTargets(train_under))

train_over <- oversample(train.task, rate = 15)
table(getTaskTargets(train_over))

system.time(train_smote <- smote(train.task, rate = 15, nn=3))
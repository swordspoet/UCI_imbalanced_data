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
# convert characters to factors
char_cols <- colnames(train)[sapply(train,is.character)]
for(i in char_cols) set(train,j=i,value = factor(train[[i]]))
for(i in char_cols) set(test,j=i,value = factor(test[[i]]))
# 创建任务
train_task <- makeClassifTask(data = train, target = "income_level")
test_task <- makeClassifTask(data = test, target = "income_level")
# one-hot编码
train_task <- createDummyFeatures(obj = train_task)
test_task <- createDummyFeatures(obj = test_task)
# make a xgboost learner
set.seed(2002)
xgb_learner <- makeLearner("classif.xgboost",predict.type = "response")
xgb_learner$par.vals <- list(
  objective = "binary:logistic",
  eval_metric = "error",
  nrounds = 150,
  print.every.n = 50)
xg_ps <- makeParamSet( 
  makeIntegerParam("max_depth",lower=3,upper=10),
  makeNumericParam("lambda",lower=0.05,upper=0.5),
  makeNumericParam("eta", lower = 0.01, upper = 0.5),
  makeNumericParam("subsample", lower = 0.50, upper = 1),
  makeNumericParam("min_child_weight",lower=2,upper=10),
  makeNumericParam("colsample_bytree",lower = 0.50,upper = 0.80))
#define search function
rancontrol <- makeTuneControlRandom(maxit = 5L) #do 5 iterations
#5 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 5L,stratify = TRUE)
#set parallel backend
library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores())
#tune parameters
xgb_tune <- tuneParams(learner = xgb_learner, task = train_task, resampling = set_cv, 
                       measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = xg_ps, 
                       control = rancontrol)
xgb_tune$y
#set optimal parameters
xgb_new <- setHyperPars(xgb_learner, par.vals = xgb_tune$x)
#train model
xgmodel = train(xgb_new, train_task)
predict_xgb <- predict(xgmodel, test_task)
xgb_prediction <- predict_xgb$data$response
test[,income_level:= ifelse(income_level == "-50000","-50000","+50000")]
xg_confused <- confusionMatrix(test$income_level,xgb_prediction)

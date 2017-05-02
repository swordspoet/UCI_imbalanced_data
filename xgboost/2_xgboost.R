#using one hot encoding
tr_labels <- d_train$income_level
ts_labels <- d_test$income_level
new_tr <- model.matrix(~.+0,data = d_train[,-c("income_level"),with=F])
new_ts <- model.matrix(~.+0,data = d_test[,-c("income_level"),with=F])
#convert factor to numeric
tr_labels <- as.numeric(tr_labels)-1
ts_labels <- as.numeric(ts_labels)-1
# 准备矩阵
dtrain <- xgb.DMatrix(data = new_tr,label = tr_labels) 
dtest <- xgb.DMatrix(data = new_ts,label= ts_labels)

params <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eta=0.3, gamma=0, max_depth=6, 
               min_child_weight=1, subsample=1, 
               colsample_bytree=1)
xgbcv <- xgb.cv( params = params, 
                 data = dtrain, nrounds = 100, 
                 nfold = 5, showsd = T, 
                 stratified = T, print.every.n = 10,
                 early.stop.round = 20, maximize = F)
xgb1 <- xgb.train (params = params, 
                   data = dtrain, nrounds = 100, 
                   watchlist = list(val=dtest,train=dtrain), 
                   print.every.n = 10, 
                   early.stop.round = 10, 
                   maximize = F , eval_metric = "error")
xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)
library(caret)
confusionMatrix(xgbpred, ts_labels)
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20]) 

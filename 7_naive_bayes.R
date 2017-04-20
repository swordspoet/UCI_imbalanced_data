naive_learner <- makeLearner("classif.naiveBayes", predict.type = "response")
naive_learner$par.vals <- list(laplace = 1)

folds <- makeResampleDesc("CV", iters = 10, stratify = TRUE)

fun_cv <- function(a){
  crv_val <- resample(naive_learner,a,folds,measures = list(acc,tpr,tnr,fpr,fp,fn))
  crv_val$aggr
}
# 训练结果，从训练结果得知，对不平衡数据集采取不同的采样
# 方法得出的结果截然不同
fun_cv(train.task)
# Result: 
# acc.test.mean=0.732,tpr.test.mean=0.721,tnr.test.mean=0.896,
# fpr.test.mean=0.104,fp.test.mean= 129,fn.test.mean=5.21e+03
fun_cv(train_under)
# Result:
# acc.test.mean=0.785,tpr.test.mean=0.656,tnr.test.mean=0.914,
# fpr.test.mean=0.0856,fp.test.mean=1.59e+03,fn.test.mean=6.44e+03
fun_cv(train_over)
# Result: acc.test.mean=0.785,tpr.test.mean=0.656,
# tnr.test.mean=0.914,fpr.test.mean=0.0856,fp.test.mean=1.59e+03,fn.test.mean=6.44e+03
fun_cv(train_smote)
# Result: acc.test.mean=0.896,tpr.test.mean=0.842,tnr.test.mean=0.951,
# fpr.test.mean=0.0488,fp.test.mean= 906,fn.test.mean=2.96e+03

nB_model <- train(naive_learner, train_smote)
nB_predict <- predict(nB_model, test.task)

nB_prediction <- nB_predict$data$response
dCM <- confusionMatrix(d_test$income_level,nB_prediction)
precision <- dCM$byClass['Pos Pred Value']
recall <- dCM$byClass['Sensitivity']
Specificity <- dCM$byClass['Specificity']
# 准确率precision ：0.844
# 召回率recall 0.985
# Specificity（真阴性率）：0.254

# F值
f_measure <- 2*((precision*recall)/(precision+recall))
f_measure
# Pos Pred Value 
#     0.9089276 
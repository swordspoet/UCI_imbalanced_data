---
title: "处理不平衡数据——基于UCI人口调查数据集（一）"
date: "2017/4/20 9:55:45  "
categories: [编程]
tags: [数据分析,R,机器学习]
english_title: R-imbalanced-data-1

---


>Like it or not , a bitter truth about data science industry is that self-learning, certification, coursework is  not sufficient to get you a job.

<!--more-->

这是一篇关于如何处理不平衡数据集的文章，学习R语言，如果没有在实际当中用，说到底还是纸上谈兵。大家还是相信这句话，Talk is cheap, show me the code.R语言的学习曲线比较高，当中的东西太多太杂（个人以为不够优雅），要想在短时间内在R中做到触类旁通简直艰难。在接触到这个小项目（姑且称为小项目）后，踩了好多坑，书上看到的数据清洗和工程应用中的数据清洗可是完全不同的概念。写完这篇文章之后，感觉自己的编程能力太渣，代码的编写阅读量还是太少，任重道远~

在实际工程中，有一些极端条件（欺诈检测、癌症诊断、在线广告捕捉）会使得数据集十分不平衡，目标变量的分布可能会呈现极端分布态势。做这个项目有什么好处呢？

- 处理不平衡数据是一个技巧性的工作
- 数据的维度高，因此，它可以帮助你理解和克服机器的内存问题
- 提升你的编程、数据分析和机器学习技能


# 一、描述问题&生成假设 #

>任务要求：给定拥有多个变量的数据集，目标是根据这些变量来建立一个预测收入水平是大于50000还是小于50000的分类模型。

>Prediction task is to determine the income level for the person represented by the record. Incomes have been binned at the $50K level to present a binary classification problem, much like the original UCI/ADULT database. 

从问题的描述来看，这是一个二分类问题，任务要求目标函数输出的结果只有两种类型，即“是”或者“否”。

在这个项目中我们用到的是来自UCI机器学习的[数据集](http://archive.ics.uci.edu/ml/machine-learning-databases/census-income-mld/)，这是一份美国人口的调查数据，打开数据集，我们会发现列名全部都丢失了，完好的数据集在这里下载[训练集](https://www.analyticsvidhya.com/wp-content/uploads/2016/09/train.zip)、[测试集](https://www.analyticsvidhya.com/wp-content/uploads/2016/09/test.zip)。

我们拿到数据要做的第一件事便是先打开看看，思考数据量有多大？有多少个变量？变量是连续型的还是非连续型的？有没有缺失数据？······

# 二、探索数据 #

```r
# 加载包和数据集
# na.string=c()定义多个变量（""   " "  "?"  "NA" NA）为NA

library(data.table)
train <- fread("E:/R/imbalancedata/train.csv",na.string=c(""," ","?","NA",NA)) 
test <- fread("E:/R/imbalancedata/test.csv",na.string=c(""," ","?","NA",NA))
```

<div><img src="http://ww3.sinaimg.cn/large/778d5ca9gw1fa14n6fodqj213f0lkqjj.jpg">

<img src="http://ww2.sinaimg.cn/large/778d5ca9gw1fa14n6f9roj213j0m9h2n.jpg"></div>

从数据加载之后的情况来看，训练集中有199523个观测值，41个变量，测试集的观测值有99762个，同样也是41个变量。回到我们的目标，我们的目标是要建立预测收入的模型，那么因变量是什么？是收入水平。

找到因变量（income_level），训练集和测试集下的`income_level`变量下只有`-50000`和`+50000`两种取值分别对应小于50000和大于50000两种取值。为了方便，将`income_level`下的取值替换为0（小于50000）和1（大于50000）。

```r
unique(train$income_level)
unique(test$income_level)
[1] "-50000" "+50000"
[1] "-50000"  "50000+."
# 将目标变量取值替换为0,1，ifelse(test,yes,no)
train[,income_level:= ifelse(income_level == "-50000",0,1)] 
test[,income_level:= ifelse(income_level == "-50000",0,1)] 
```

接着，继续查看训练集的正负样本分布情况

```r
round(prop.table(table(train$income_level))*100)
 0  1 
94  6 
```

从返回结果可以看出，原始训练集的正负样本分布非常不均衡，收入水平小于5万的人群占据了94%，大于5万的人群仅仅占了6%（毕竟有钱人还是少！），这是一个典型的不平衡数据集，正负样本差别太大，则会对模型的准确性造成误解。例如有98个正例，2个反例，那么只需要建立一个永远返回正例的预测模型就能轻松达到98%的精确度，这样显然是不合理的。

那么如何由这种不平衡数据集学习到一个准确预测收入水平的模型呢？这是主要要解决的难题。

# 三、数据预处理 #

首先，从数据集介绍的[页面](http://archive.ics.uci.edu/ml/machine-learning-databases/census-income-mld/census-income.names)可以了解到，变量被分为nominal（名义型）和continuous（连续型）两种类型，即分别对应类别型和数值型。

对数据进行预处理时，首先要解决的便是将这两种不同的数据类型切分开来，`data.table`包可以帮助我们快速简单地完成这个任务。

```r
#数据集介绍的页面已经告诉我们哪些特征为nominal或是continuous
factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
numcols <- setdiff(1:40,factcols)
# lapply的.SD的用法
train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]
test[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]
# 将训练集和测试集中的类别变量和数值变量分别提取出来
cat_train <- train[,factcols, with=FALSE]
cat_test <- test[,factcols,with=FALSE]
num_train <- train[,numcols,with=FALSE]
num_test <- test[,numcols,with=FALSE]
```

# 四、数据可视化 #

单纯查看数据集无法得到直观的感受，“一图胜千言”，图形是最简单直观的办法，下面我们会用到`ggplot2`和`plotly`两个强大的绘图包。

```r
library(ggplot2)
library(plotly)
# geom_histogram()直方图
# geom_density()密度图
# aes设定x轴y轴的名称
tr <- function(a){ggplot(data = num_train, aes(x= a, y=..density..)) +
geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
ggplotly()
}

tr(num_train$age)
tr(num_train$capital_losses)
```
<div><img src="http://p1.bqimg.com/4851/15cb41a4b8d7e6ef.png"></div>



以上两个分布图符合我们的常识，年龄分布在0~90岁之间，年龄越大，所占比例越小。

## 自变量与因变量之间的关系 ##

我们分别考虑训练集中的数值型变量和类别型变量与`income_level`的关系。

首先看数值型的，数值型训练集`num_train`下有`wage_per_hour`、`capital_gains`、`capital_losses`、`dividend_from_Stocks`等等几个变量，我们选取了四个关联程度较大的指标，可以看出，大部分年龄段处于25-65的人收入水平`income_level`为1（大于50000），他们的小时工资（wage per hour）处在1000美元到4000美元的水平。这个事实进一步强化了我们认为年龄小于20的人收入水平小于50000的假设。

![年龄vs时薪](http://i.imgur.com/EFjZxGn.jpg)

```r
#income_level属于类别型的，被切分到了cat_train中
num_train[,income_level := cat_train$income_level]
ggplot(data=num_train,aes(x = age,
y=wage_per_hour))+geom_point(aes(colour=income_level))+scale_y_continuous("wage per
hour", breaks = seq(0,10000,1000))
```

股票收益对收入的影响也是比较大的，收入水平大于50000的人群基本上股票分红都超过了30000美元

![年龄vs股票收益](http://ww2.sinaimg.cn/large/778d5ca9gw1fa14cc0fynj20m80dntbg.jpg)

```r
num_train[,income_level := cat_train$income_level]
ggplot(data=num_train,aes(x = age,y=dividend_from_Stocks))+geom_point(aes(colour=income_level))+
  scale_y_continuous("dividend from stocks", breaks = seq(0,10000,5000))
```
                   
类似地，我们可以还可以查看`capital_gains`资本增值和`capital_losses`资本贬值与收入水平的关系，如下图所示

<div><img src="http://ww2.sinaimg.cn/large/778d5ca9gw1fa14cc0fynj20m80dntbg.jpg">
<img src="http://ww3.sinaimg.cn/large/778d5ca9gw1fa14cc23pyj20m80dnadq.jpg"></div>

同样的，我们也可以将分类变量以可视化的形式展现出来，对于类别数据，dodged条形图比一般条形图能展更多的信息。在dodged条形图中，可以发现很多有趣的东西，比如本科毕业生年薪超过5万的人数最多，拥有博士学位的人年薪超过5万和低于5万的比例是相同的，白人年薪超过5万的人群远远多于其他肤色的种族

```r
all_bar <- function(i){
 ggplot(cat_train,aes(x=i,fill=income_level))+geom_bar(position = "dodge",  color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle = 60,hjust = 1,size=10))
}
```
![allbar_class_of_worker](http://i.imgur.com/PGddtZc.jpg)

![allbar_race](http://i.imgur.com/kvhq5PH.jpg)

# 五、数据清洗 #

## 检查遗漏值 ##

数据清洗时数据分析的一个重要步骤，首先检查训练集和测试集中是否有遗漏值

```r
table(is.na(num_train))
table(is.na(num_test))
```
从反馈的结果来看，FALSE分别等于1596184=199523×8，698334=99762×7，故训练集和测试集中没有一个遗漏值，这是一个不错的消息！


## 删除高度相关变量 ##

变量之间的相关性对模型的准确性存在影响，`caret`包能够帮助我们检查数值型变量之间的相关性，筛选出高度相关的变量

```r
library(caret)
x <- cor(num_train)
ax <- findCorrelation(x, cutoff=0.7)
num_train <- num_train[,-ax,with=FALSE]
num_test <- num_test[,weeks_worked_in_year := NULL]
```
![变量相关性](http://wx4.sinaimg.cn/large/778d5ca9ly1fescq3xyk0j20vz05zdha.jpg)

筛选的结果显示，`weeks_worked_in_year`变量与其他变量存在相当高的相关性。这很好理解，因为一年之中工作的时间越长那么相应的工资、回报也会随之上涨，所以要把这个高度相关性的变量剔除掉，这样`num_train`当中就只剩下7个变量了。                     

似乎训练集中数值型的数据已经处理得差不多了，下面我们来看看训练集中类别型的数据`cat_train`，首先检查每一列数据的遗漏情况

```r
mvtr <- sapply(cat_train, function(x){sum(is.na(x))/length(x)})*100
mvte <- sapply(cat_test, function(x){sum(is.na(x)/length(x))}*100)

mvtr
mvte
```
![](http://wx1.sinaimg.cn/large/778d5ca9ly1fescvbx2wej20mv09r3zu.jpg)

![](http://wx2.sinaimg.cn/large/778d5ca9ly1fescxcw8egj20mw09pgmy.jpg)

大部分的列情况比较乐观，但是有的列甚至有超过50%的数据遗漏（这有可能是由于采集数据难度所致，特别是人口普查），将遗漏率小于5%的列挑选出来，遗漏率太高的变量剔除掉。

```r
cat_train <- subset(cat_train, select = mvtr < 5 )
cat_test <- subset(cat_test, select = mvte < 5)
```
                
# 六、数据操作——规范化处理 #

在前面的分析中，有的类别变量下个别水平出现的频率很低，这样的数据对我们的分析作用不是特别大。在接下来的步骤中，我们的任务是将这些变量下频率低于5%的水平字段
设置为"Other"。处理完类别型数据之后，对于数值型数据，各个变量下的水平分布过于稀疏，所以需要将其规范化。

```r
#将cat_train和cat_test中每列下出现频率低于5%的水平设置为“Other”
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
```


```r
#"nlevs"参数：返回每列下维度的个数，测试集和训练集是否匹配

summarizeColumns(cat_train)[,"nlevs"] 
summarizeColumns(cat_test)[,"nlevs"]

num_train[,.N,age][order(age)]
num_train[,.N,wage_per_hour][order(-N)]

#以0，30，90为分隔点，将年龄段划分为三个区段，“young”，“adult”，“old”
num_train[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_train[,age := factor(age)]

num_test[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_test[,age := factor(age)]

#将wage_per_hour，capital_gains，capital_losses，dividend_from_Stocks设置为只有0和大于0两个水平
num_train[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_train[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_train[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_train[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

num_test[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_test[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_test[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_test[,dividend_from_Stocks := ifelse(dividend_from_Stocks ==0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

num_train[,income_level := NULL]
```

![标准化处理之后的结果](http://ww1.sinaimg.cn/large/778d5ca9gw1fa87ivtwyej20jh0e3n13.jpg)


## 组合数据 ##

```r
d_train <- cbind(num_train, cat_train)
d_test <- cbind(num_test, cat_test)

library(mlr)
train.task <- makeClassifTask(data = d_train, target = "income_level")
test.task <- makeClassifTask(data = d_test, target = "income_level")
# Remove constant features from a data set.
# Constant features can lead to errors in some models 
# and obviously provide no information in the training set that can be learned from
train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)
```

# 七、处理不平衡数据集

应对不平衡数据集，通常的技巧有上采样(oversampling)、下采样(undersampling)，以及过采样的一种代表性方法SMOTE(Synthetic
Minority Oversampling TEchnique)算法。

- 上采样：即增加一些正例使得正、反例数目接近，然后再进行学习
- 下采样：去除一些反例使得正、反例数目接近，然后进行学习
- SMOTE：通过对训练集里的正例进行插值来产生额外的正例，主要思想是通过在一些位置相近的少数类样本中插入新样本来达到平衡的目的

下采样法的时间开销通常远远小于上采样法，因为前者丢弃了很多反例，使得训练集远小于初始训练集，下采样另外一个可能的缺陷在于
它可能会导致信息的丢失。上采样法增加了很多正例，训练集的大小大于初始训练集，训练时间开销变大，而且容易导致过拟合。

更多关于SMOTE采样方法，Chawla 的这篇[文章](http://http://www.jair.org/media/953/live-953-2037-jair.pdf)有详细的说明。

```r
train_under <- undersample(train.task, rate = 0.1)
table(getTaskTargets(train_under))

train_over <- oversample(train.task, rate = 15)
table(getTaskTargets(train_over))

system.time(train_smote <- smote(train.task, rate = 15, nn=3))
```

# 八、学习一个Naive Bayesian分类器  #

```r
naive_learner <- makeLearner("classif.naiveBayes", predict.type = "response")
naive_learner$par.vals <- list(laplace = 1)

folds <- makeResampleDesc("CV", iters = 10, stratify = TRUE)

fun_cv <- function(a){
  crv_val <- resample(naive_learner,a,folds,measures = list(acc,tpr,tnr,fpr,fp,fn))
  crv_val$aggr
}
```

## 模型评估 ##

```r
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
     0.9089276 
```
由实验结果，学习得到的模型达到0.844的准确率和0.985的召回率，而真反例仅仅为0.254。这就说明模型在预测正例上表现良好，而对个数较少的反例预测精度不高。这样的结果不太令人满意，在接下来的文章中我们继续探讨其他模型是不是有更好的效果。

（未完）

# 参考文献 #

1. Chawla N V, Bowyer K W, Hall L O, et al. SMOTE: synthetic minority over-sampling technique[J]. Journal of artificial intelligence research, 2002, 16: 321-357.
2. Chawla N V. Data mining for imbalanced datasets: An overview[M]//Data mining and knowledge discovery handbook. Springer US, 2005: 853-867.
3. [This Machine Learning Project on Imbalanced Data Can Add Value to Your Resume](https://www.analyticsvidhya.com/blog/2016/09/this-machine-learning-project-on-imbalanced-data-can-add-value-to-your-resume/)


# 处理不平衡数据——基于UCI人口调查数据集（二）#


本文是处理不平衡数据系列之二，在上一篇文章中，我们完成了对数据的预处理、可视化以及模型训练与预测等等工作，对数据有了整体的认识。在对实验数据进行预处理的时候，<font color="blue">缺失值（missing values）和高相关性变量（variables with high correlation）</font>是重点关注的对象。解决了这两个问题后，数据集样本不平衡的缺陷仍旧没有根除，所以针对数据分别进行了上采样、下采样以及SMOTE三种采样方法。显然，采样花费时间最久的SMOTE在模型中表现最佳，拿到了最高的准确率0.896，可是正当准备庆祝的时候，一个不幸的“消息”告诉我们：特异度（Specificity）只有0.254。也就是说，模型对预测收入高于5w的少数人群（minority class）表现不太好，这样的模型结果是不太令人满意的，能够拿到0.896的准确率自然也是在情理之中，毕竟正反样本的比例（96:4）摆在那里。为了克服这个缺陷，我们在R语言中采用了高效、性能强大的xgboost处理框架，最终得到理想的数据。

说句题外话，原本计划完成任务需花费10个番茄，实际耗时远远多出了预期的1倍多，整个五一就窝在实验室了。经过这个小小的项目后，深感“单兵作战”孤立无援的苦楚，唯有不断google，不断将写好的代码推倒重来，不断input、output······

<!--more-->

> 本项目github地址：

- 一、初次尝试xgboost
	- xgboost模型参数解释与设定
- 二、xgboost in mlr
	- 数据预处理
	- 调参与模型训练
- 参考链接

# 一、初次尝试xgboost #

为了训练出一个能够在预测正负样本表现均良好的模型，在这篇文章中我们会用到xgboost算法，xgboost(eXtreme Gradient Boosting)的作者是华盛顿大学的[陈天奇](http://homes.cs.washington.edu/~tqchen/)，xgboost最大的特点在于，它能够自动利用CPU的多线程进行并行计算，同时在算法上加以改进提高了精度。随着越来越多的队伍借助xgboost取得了kaggle比赛中的胜利，xgboost在近年来变得十分流行。

![xgboost](http://wx4.sinaimg.cn/mw690/778d5ca9gy1ff6pzp809jj20dw05wwg3.jpg)

xgboost不仅被封装成了Python库，[何通](https://github.com/hetong007)制作了xgboost工具的R语言接口，所以R中安装后便可以直接使用。

照例，我们先对数据进行预处理，预处理的思路是：分别考虑训练集、测试集中的数值型变量和类别型变量，对数值型，剔除高度相关的变量，对类别型，剔除数据遗漏严重的变量。经过上述两个步骤后，再将
数值型和类别型变量重新组合。因为R对内存的要求太苛刻了，完成数据预处理后，还需要将`train,test,num_train,num_test,cat_train,cat_test`从RStudio中移除以减少内存占用，不然会出现内存不够的情况。以笔者8G内存的台式机器为例，本次实验中CPU与内存满负荷运算是常事，还出现几次假死、崩溃的情况，所以在R中进行数据处理的时候一定要注意内存的管理。

```r
# 加载包
library(caret)
library(data.table)
library(xgboost)

# 加载数据集
train <- fread("E:/R/imbalancedata/train.csv",na.string=c(""," ","?","NA",NA)) 
test <- fread("E:/R/imbalancedata/test.csv",na.string=c(""," ","?","NA",NA))
table(is.na(train));table(is.na(test))
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
```

## xgboost模型参数解释与设定 ##

完成了数据预处理，接着便是分类模型的构建。在运行xgboost前，有三类参数需要人工设定： general parameters, booster parameters and task parameters，xgboost的[官方文档](http://xgboost.readthedocs.io/en/latest//parameter.html)有关于这些参数的详细解释：

>- **General parameters** relates to which booster we are using to do boosting, commonly tree or linear model
- **Booster parameters** depends on which booster you have chosen
- **Learning Task parameters** that decides on the learning scenario, for example, regression tasks may use different parameters with ranking tasks

1. Tree **Booster**参数解释：

- **eta [default=0.3, alias: learning_rate]**
    - 学习率，防止模型出现过拟合，默认取0.3，通常取值范围[0.01,3]
    - 在每次迭代后，变量的权重会随之衰减
- **gamma [default=0, alias: min_split_loss]**
    - 模型正则化系数，gamma越大，意味着模型越不容易出现过拟合
- **max_depth [default=6]**
    - max_depth值越大，意味着模型越复杂，也越容易出现过拟合
    - max_depth的取值没有规定
- **min_child_weight [default=1]**
    - In classification, if the leaf node has a minimum sum of instance weight (calculated by second order partial derivative) lower than min_child_weight, the tree splitting stops.
- **subsample[default=1][range: (0,1)]**
    - It controls the number of samples (observations) supplied to a tree.
    - 通常取值范围为 (0.5,0.8)
- **colsample_bytree[default=1][range: (0,1)]**
    - It control the number of features (variables) supplied to a tree
    - 通常取值范围为 (0.5,0.9)

2. **Learning Task**参数解释：

- **Objective[default=reg:linear]**
    - Objective规定模型需要处理的任务
    - `reg:linear` - 线性回归
    - `binary:logistic` - 二分类LR回归
    - `multi:softmax` - 多分类softmax回归

```r
> tr_labels <- d_train$income_level
> ts_labels <- d_test$income_level
> new_tr <- model.matrix(~.+0,data = d_train[,-c("income_level"),with=F])
> new_ts <- model.matrix(~.+0,data = d_test[,-c("income_level"),with=F])
> tr_labels <- as.numeric(tr_labels)-1
> ts_labels <- as.numeric(ts_labels)-1
> dtrain <- xgb.DMatrix(data = new_tr,label = tr_labels)
> dtest <- xgb.DMatrix(data = new_ts,label= ts_labels)
> params <- list(booster = "gbtree", 
+                objective = "binary:logistic", 
+                eta=0.3, gamma=0, max_depth=6, 
+                min_child_weight=1, subsample=1, 
+                colsample_bytree=1)
> xgbcv <- xgb.cv( params = params, 
+                  data = dtrain, nrounds = 100, 
+                  nfold = 5, showsd = T, 
+                  stratified = T, print.every.n = 10,
+                  early.stop.round = 20, maximize = F)
[1]	train-error:0.049454+0.000231	test-error:0.050275+0.001244 
Multiple eval metrics are present. Will use test_error for early stopping.
Will train until test_error hasn't improved in 20 rounds.

[11]	train-error:0.045343+0.000408	test-error:0.046691+0.001152 
[21]	train-error:0.042996+0.000356	test-error:0.045323+0.001094 
[31]	train-error:0.041548+0.000311	test-error:0.044180+0.000912 
[41]	train-error:0.040261+0.000405	test-error:0.043739+0.000868 
[51]	train-error:0.039582+0.000303	test-error:0.043514+0.000995 
[61]	train-error:0.038914+0.000295	test-error:0.043308+0.000788 
[71]	train-error:0.038361+0.000195	test-error:0.043088+0.000858 
[81]	train-error:0.037948+0.000252	test-error:0.043003+0.000837 
[91]	train-error:0.037500+0.000189	test-error:0.042937+0.000921 
[100]	train-error:0.037144+0.000316	test-error:0.043063+0.001010 
Warning messages:
1: 'print.every.n' is deprecated.
Use 'print_every_n' instead.
See help("Deprecated") and help("xgboost-deprecated"). 
2: 'early.stop.round' is deprecated.
Use 'early_stopping_rounds' instead.
See help("Deprecated") and help("xgboost-deprecated"). 
> xgb1 <- xgb.train (params = params, 
+                    data = dtrain, nrounds = 100, 
+                    watchlist = list(val=dtest,train=dtrain), 
+                    print.every.n = 10, 
+                    early.stop.round = 10, 
+                    maximize = F , eval_metric = "error")
[1]	val-error:0.049758	train-error:0.049714 
Multiple eval metrics are present. Will use train_error for early stopping.
Will train until train_error hasn't improved in 10 rounds.

[11]	val-error:0.046511	train-error:0.045644 
[21]	val-error:0.044937	train-error:0.042993 
[31]	val-error:0.044396	train-error:0.041504 
[41]	val-error:0.043915	train-error:0.040777 
[51]	val-error:0.044205	train-error:0.039835 
[61]	val-error:0.044486	train-error:0.038888 
[71]	val-error:0.044917	train-error:0.038467 
[81]	val-error:0.045007	train-error:0.038166 
[91]	val-error:0.044797	train-error:0.037890 
[100]	val-error:0.044917	train-error:0.037665 
Warning messages:
1: 'print.every.n' is deprecated.
Use 'print_every_n' instead.
See help("Deprecated") and help("xgboost-deprecated"). 
2: 'early.stop.round' is deprecated.
Use 'early_stopping_rounds' instead.
See help("Deprecated") and help("xgboost-deprecated"). 
> xgbpred <- predict (xgb1,dtest)
> xgbpred <- ifelse (xgbpred > 0.5,1,0)
> library(caret)
> confusionMatrix(xgbpred, ts_labels)
Confusion Matrix and Statistics

          Reference
Prediction     0     1
         0 92366  3271
         1  1210  2915
                                          
               Accuracy : 0.9551          
                 95% CI : (0.9538, 0.9564)
    No Information Rate : 0.938           
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.5427          
 Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.9871          
            Specificity : 0.4712          
         Pos Pred Value : 0.9658          
         Neg Pred Value : 0.7067          
             Prevalence : 0.9380          
         Detection Rate : 0.9259          
   Detection Prevalence : 0.9587          
      Balanced Accuracy : 0.7291          
                                          
       'Positive' Class : 0               
                                          
> mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
> xgb.plot.importance (importance_matrix = mat[1:20])
```

其实，即使是给模型设定默认的参数也能得到意想不到的准确率，xgboost在kaggle社区中如此受欢迎也是有理由的。运行，训练模型（耗时约4分钟）并预测，模糊矩阵`confusionMatrix(xgbpred, ts_labels)`结果显示模型准确率达到了95.51%，然而这并不是重点。提升模型对预测收入高于5w的少数人群（minority class）的预测能力才是我们的目标，结果显示特异度（Specificity）达到47.12%，比上一个朴素贝叶斯提升了11.7%，效果仍然不是特别完美，不过也还可以了！无论是准确率还是其他衡量指标，xgboost得出的结果是全面优于之前的朴素贝叶斯模型的，那么还有没有提升的空间呢？


----------

# 二、xgboost in mlr #

![MLR](http://wx4.sinaimg.cn/large/778d5ca9gy1ff3vmkjh80j203x01s0so.jpg)

2016年，R语言的用户迎来了`mlr`包的诞生，mlr，即machine learning in R。mlr是R语言中为专门应对机器学习问题而开发的包，在mlr出现之前，R语言中是不存在像Scikit-Learn这样的科学计算工具的。mlr包为在R中用机器学习方法解决问题提供了一套自有的框架，涵盖了分类、聚类、回归、生存分析等问题，另外mlr还为参数调优、预测结果分析、数据预处理等与机器学习相关的话题贡献了较为完整的方案。如果说，Scikit-Learn在Python的各个库之间不分伯仲，那么R语言的mlr就可谓一枝独秀。

说了这么多，如果对mlr感兴趣的同学可以去RStudio里一睹“真容”；mlr也专门为用户建立了一个教程网站：[Machine Learning in R: mlr Tutorial](http://mlr-org.github.io/mlr-tutorial/devel/html/index.html)，可以去官网找一个例子来跑一跑；这是mlr的[github项目](https://github.com/mlr-org/mlr/)，由于mlr的普及率还不算太高，官方文档也还在优化中，所以在google上找到关于mlr的资源还不是特别多，所以建议大家在使用过程中出现问题的话去项目中提issue或者在issue中找答案，这是最有效的办法！

在R语言中使用mlr包解决机器学习问题只需要牢记三个步骤即可：

- Create a Task：导入数据集，**创建任务**，可以是分类、回归、聚类等等
- Make a Learner：**构建模型**，模型构建过程中涉及到参数设定、参数调节诸多技巧
- Fit the model：拟合模型
- Make predictions：预测

在R中，变量可以归结为名义型、有序型、或连续型变量，类别（名义型）变量和有序类别（有序型）在R中称为因子（factor）。

>更多关于R语言数据结构的内容参见[这篇文章](http://www.libinx.com/2016/11/13/2016-11-13-R-data-structure/)。

值得注意的是mlr包对数据的格式是有要求的，mlr任务函数不接受字符型（char）变量，所以在构建任务函数前，必须确保将所有的变量转换为因子型（factor），作者的解释是

>All these things are possible pre-processors, which can be a model that wraps xgboost, when before doing train/predict, run the pre-processing and feed processed data to xgboost. So it is not hard.This is also reason why I do not explicit support factor in the tree construction algorithm. There could be many ways doing so, and in all the ways, having an algorithm optimized for sparse matrices is efficient for taking the processed data.
Normal tree growing algorithm only support dense numerical features, and have to support one-hot encoding factor explicitly for computation efficiency reason.

在mlr中的xgboost，似乎并不需要进行太多的数据预处理，xgboost的作者回复[issue](https://github.com/dmlc/xgboost/issues/95)时是这样说的

>"..... xgboost treat every input feature as numerical, **with support for missing values and sparsity**. The decision is at the user
So if you want ordered variables, you can transform the variables into numerical levels(say age). Or if you prefer treat it as categorical variable, do one hot encoding."

也就是说xgboost视每个特征均为数值型，同时还支持遗漏变量和稀疏性数据，至于对数据进行何种预处理，决定权在用户手上。不同于之前，本次数据预处理仅仅是将字符型变量转换成因子，然后feed给mlr，mlr就直接开始创建任务（Create task）、构建模型（Make a learner）了，简单而且粗暴。

## 数据预处理 ##

下面，我们将使用mlr包继续提升模型的预测效果，照例首先加载数据和包。

```r
# 载入数据和包
> library(data.table)
data.table 1.9.8
  The fastest way to learn (by data.table authors): https://www.datacamp.com/courses/data-analysis-the-data-table-way
  Documentation: ?data.table, example(data.table) and browseVignettes("data.table")
  Release notes, videos and slides: http://r-datatable.com
> library(xgboost)
Warning message:
程辑包‘xgboost’是用R版本3.3.3 来建造的 
> library(caret)
载入需要的程辑包：lattice
载入需要的程辑包：ggplot2

载入程辑包：‘caret’
> library(mlr)
载入需要的程辑包：ParamHelpers
Warning messages:
1: 程辑包‘mlr’是用R版本3.3.3 来建造的 
2: 程辑包‘ParamHelpers’是用R版本3.3.3 来建造的 
The following object is masked from ‘package:caret’:

    train

> train <- fread("E:/R/imbalancedata/train.csv",na.string=c(""," ","?","NA",NA))
> test <- fread("E:/R/imbalancedata/test.csv",na.string=c(""," ","?","NA",NA))
> setDT(train)
> setDT(test)
```

在加载包的时候需要<font color = "blue">注意mlr和caret的加载顺序</font>，caret应该在mlr之前载入，否则训练模型的时候R不清楚到底是加载caret的train还是mlr的train，从而导致如下错误

```r
Error in unique.default(x, nmax = nmax) : 
  unique() applies only to vectors
```

一定要确保

```r
The following object is masked from ‘package:caret’:

    train
```

## 调参与模型训练 ##

在对模型进行训练时，R的运算速度一直是一个问题，其中一个就是只能使用单线程计算。但是R在2.14版本之后，R就内置了parallel包，强化了R的并行计算能力。parallel包可以很容易的在计算集群上实施并行计算，在多个CPU核心的单机上，也能发挥并行计算的功能。笔者用的计算机是4核i5-6600K的CPU与8G内存，即使是中端配置的机器也需要满负荷计算约一小时才能得到最优参数。

<div>
<img src="http://wx4.sinaimg.cn/large/778d5ca9ly1ff6vxo7t8oj20zk0qojxg.jpg" width="400">

```r
> char_cols <- colnames(train)[sapply(train,is.character)]
> for(i in char_cols) set(train,j=i,value = factor(train[[i]]))
> for(i in char_cols) set(test,j=i,value = factor(test[[i]]))
> train_task <- makeClassifTask(data = train, target = "income_level")
Warning in makeTask(type = type, data = data, weights = weights, blocking = blocking,  :
  Provided data is not a pure data.frame but from class data.table, hence it will be converted.
> test_task <- makeClassifTask(data = test, target = "income_level")
Warning in makeTask(type = type, data = data, weights = weights, blocking = blocking,  :
  Provided data is not a pure data.frame but from class data.table, hence it will be converted.
> train_task <- createDummyFeatures(obj = train_task)
> train_task <- createDummyFeatures(obj = train_task)
> set.seed(2002)
> xgb_learner <- makeLearner("classif.xgboost",predict.type = "response")
> xgb_learner$par.vals <- list(
+   objective = "binary:logistic",
+   eval_metric = "error",
+   nrounds = 150,
+   print.every.n = 50)
> xg_ps <- makeParamSet( 
+   makeIntegerParam("max_depth",lower=3,upper=10),
+   makeNumericParam("lambda",lower=0.05,upper=0.5),
+   makeNumericParam("eta", lower = 0.01, upper = 0.5),
+   makeNumericParam("subsample", lower = 0.50, upper = 1),
+   makeNumericParam("min_child_weight",lower=2,upper=10),
+   makeNumericParam("colsample_bytree",lower = 0.50,upper = 0.80))
> rancontrol <- makeTuneControlRandom(maxit = 5L) #do 5 iterations
> set_cv <- makeResampleDesc("CV",iters = 5L,stratify = TRUE)
> library(parallel)
> library(parallelMap)
> parallelStartSocket(cpus = detectCores())
Starting parallelization in mode=socket with cpus=4.
> xgb_tune <- tuneParams(learner = xgb_learner, task = train_task, resampling = set_cv, 
+                        measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = xg_ps, 
+                        control = rancontrol)
[Tune] Started tuning learner classif.xgboost for parameter set:
                    Type len Def      Constr Req Tunable Trafo
max_depth        integer   -   -     3 to 10   -    TRUE     -
lambda           numeric   -   - 0.05 to 0.5   -    TRUE     -
eta              numeric   -   - 0.01 to 0.5   -    TRUE     -
subsample        numeric   -   -    0.5 to 1   -    TRUE     -
min_child_weight numeric   -   -     2 to 10   -    TRUE     -
colsample_bytree numeric   -   -  0.5 to 0.8   -    TRUE     -
With control class: TuneControlRandom
Imputation value: -0Imputation value: -0Imputation value: -0Imputation value: 1Imputation value: InfImputation value: Inf
Exporting objects to slaves for mode socket: .mlr.slave.options
Mapping in parallel: mode = socket; cpus = 4; elements = 5.
[Tune] Result: max_depth=5; lambda=0.171; eta=0.295; subsample=0.855; min_child_weight=5.54; colsample_bytree=0.735 : acc.test.mean=0.958,tpr.test.mean=0.989,tnr.test.mean=0.482,fpr.test.mean=0.518,fp.test.mean=1.28e+03,fn.test.mean= 413
> xgb_tune$y
acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean  fp.test.mean  fn.test.mean 
    0.9575036     0.9889762     0.4818275     0.5181725  1283.2000000   412.6000000 
> xgb_tune$x
$max_depth
[1] 5

$lambda
[1] 0.1711398

$eta
[1] 0.295421

$subsample
[1] 0.8545802

$min_child_weight
[1] 5.541689

$colsample_bytree
[1] 0.7345529
```

`xgb_tune$x`查看参数调节得出的最优结果，将最优参数设定在模型`xgb_new`中，然后进行训练，这时便出现了我们前面提到的错误`unique() applies only to vectors`（当然github项目上给出的代码已经修正了）。出现这个错误之后，刚开始并不清楚原因在哪个地方，在下面的代码执行日志中可以发现我在不停地重新赋值再训练还有重新创建任务（因为我之前在R中遇到过将同一段代码先后两次执行，第一次错误，第二次却成功的情况），来来回回尝试了十几次，直到后来在github找到关于这条错误信息的issue，原来是caret和mlr的加载顺序弄错了。然后，用`detach("package:caret")`和`detach("package:mlr")`命令先将两个包移除，再按照先加载caret后加载mlr的顺序，最后再重新赋值训练，成功。

```r
> xgb_new <- setHyperPars(learner = xgb_learner, par.vals = xgb_tune$x)
> xgb_model <- train(learner = xgb_new, task = train_task)
Error in train.default(learner = xgb_new, task = train_task) : 
  argument "y" is missing, with no default
> xgb_new <- setHyperPars(learner = xgb_learner, par.vals = xgb_tune$x)
> xgb_model <- train(xgb_new, train_task)
Error in unique.default(x, nmax = nmax) : 
  unique() applies only to vectors
> rm(xgb_new)
> xgb_new <- setHyperPars(learner = xgb_learner, par.vals = xgb_tune$x)
> xgb_model <- train(xgb_new, train_task)
Error in unique.default(x, nmax = nmax) : 
  unique() applies only to vectors
> xgb_tune$y
acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean  fp.test.mean  fn.test.mean 
    0.9575036     0.9889762     0.4818275     0.5181725  1283.2000000   412.6000000 
> xgb_new <- setHyperPars(xgb_learner, par.vals = xgb_tune$x)
> xgb_model <- train(learner = xgb_new, task = train_task)
Error in train.default(learner = xgb_new, task = train_task) : 
  argument "y" is missing, with no default
> xgb_model <- train(xgb_new, train_task)
Error in unique.default(x, nmax = nmax) : 
  unique() applies only to vectors
> xgb_tune$y
acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean  fp.test.mean  fn.test.mean 
    0.9575036     0.9889762     0.4818275     0.5181725  1283.2000000   412.6000000 
> xgb_new <- setHyperPars(learner = xgb_learner, par.vals = xgb_tune$x)
> xgmodel <- train(xgb_new, train_task)
Error in unique.default(x, nmax = nmax) : 
  unique() applies only to vectors
> xgb_tune
Tune result:
Op. pars: max_depth=5; lambda=0.171; eta=0.295; subsample=0.855; min_child_weight=5.54; colsample_bytree=0.735
acc.test.mean=0.958,tpr.test.mean=0.989,tnr.test.mean=0.482,fpr.test.mean=0.518,fp.test.mean=1.28e+03,fn.test.mean= 413
> xgb_new <- setHyperPars(learner = xgb_learner, par.vals = xgb_tune$x)
> xgmodel <- train(xgb_new, train_task)
Error in unique.default(x, nmax = nmax) : 
  unique() applies only to vectors
> xgb_new <- setHyperPars(makeLearner("classif.xgboost"), par.vals = xgb_tune$x)
> xgmodel <- train(xgb_new, train_task)
Error in unique.default(x, nmax = nmax) : 
  unique() applies only to vectors
> xgb_new
Learner classif.xgboost from package xgboost
Type: classif
Name: eXtreme Gradient Boosting; Short name: xgboost
Class: classif.xgboost
Properties: twoclass,multiclass,numerics,prob,weights,missings,featimp
Predict-Type: response
Hyperparameters: nrounds=1,verbose=0,max_depth=5,lambda=0.171,eta=0.295,subsample=0.855,min_child_weight=5.54,colsample_bytree=0.735

> xgmodel = train(xgb_new, train_task)
Error in unique.default(x, nmax = nmax) : 
  unique() applies only to vectors
> xgb_learner <- makeLearner("classif.xgboost",predict.type = "response")
> xgb_learner$par.vals <- list(
+   objective = "binary:logistic",
+   eval_metric = "error",
+   nrounds = 150,
+   print.every.n = 50)
> xg_ps <- makeParamSet( 
+   makeIntegerParam("max_depth",lower=3,upper=10),
+   makeNumericParam("lambda",lower=0.05,upper=0.5),
+   makeNumericParam("eta", lower = 0.01, upper = 0.5),
+   makeNumericParam("subsample", lower = 0.50, upper = 1),
+   makeNumericParam("min_child_weight",lower=2,upper=10),
+   makeNumericParam("colsample_bytree",lower = 0.50,upper = 0.80))
> xgmodel = train(xgb_new, train_task)
Error in unique.default(x, nmax = nmax) : 
  unique() applies only to vectors
> train_task <- makeClassifTask(data = train, target = "income_level")
Warning in makeTask(type = type, data = data, weights = weights, blocking = blocking,  :
  Provided data is not a pure data.frame but from class data.table, hence it will be converted.
> test_task <- makeClassifTask(data = test, target = "income_level")
Warning in makeTask(type = type, data = data, weights = weights, blocking = blocking,  :
  Provided data is not a pure data.frame but from class data.table, hence it will be converted.
> train_task <- createDummyFeatures(obj = train_task)
> test_task <- createDummyFeatures(obj = test_task)
> xgmodel = train(xgb_new, train_task)
Error in unique.default(x, nmax = nmax) : 
  unique() applies only to vectors
> xgb_new <- setHyperPars(xgb_learner, par.vals = xgb_tune$x)
> xgmodel = train(xgb_new, train_task)
Error in unique.default(x, nmax = nmax) : 
  unique() applies only to vectors
> library(caret)
> xgmodel = train(xgb_new, train_task)
Error in unique.default(x, nmax = nmax) : 
  unique() applies only to vectors
> xgmodel = caret::train(xgb_new, train_task)
Error in unique.default(x, nmax = nmax) : 
  unique() applies only to vectors
> library(caret)
> library(data.table)
> library(mlr)
> library(xgboost)
> xgmodel = train(xgb_new, train_task)
Error in unique.default(x, nmax = nmax) : 
  unique() applies only to vectors
> (packages())
Error: could not find function "packages"
> (.packages())
 [1] "randomForest" "parallelMap"  "parallel"     "caret"        "ggplot2"      "lattice"      "xgboost"     
 [8] "mlr"          "ParamHelpers" "data.table"   "stats"        "graphics"     "grDevices"    "utils"       
[15] "datasets"     "methods"      "base"        
> detach("package:caret")
> detach("package:mlr")
> (.packages())
 [1] "randomForest" "parallelMap"  "parallel"     "ggplot2"      "lattice"      "xgboost"      "ParamHelpers"
 [8] "data.table"   "stats"        "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
[15] "base"        
> library(caret)
> library(mlr)

载入程辑包：‘mlr’

The following object is masked from ‘package:caret’:

    train

Warning message:
程辑包‘mlr’是用R版本3.3.3 来建造的 
> xgmodel = train(xgb_new, train_task)
[1]	train-error:0.050866 
[51]	train-error:0.041344 
[101]	train-error:0.039279 
[150]	train-error:0.037895 
Warning message:
'print.every.n' is deprecated.
Use 'print_every_n' instead.
See help("Deprecated") and help("xgboost-deprecated"). 
```

经历了千辛万苦，模型终于训练好了，胜利的曙光似乎就在前方，终于可以进行预测了！然而，猝不及防，正当我们将测试集的`income_level`与`xgb_prediction`进行对比时，an error thrown again，这次是`The data contain levels not found in the data.`。错误信息直接翻译过来的意思是数据中包含levels not found，分别查看预测结果`xgb_prediction`与`test$income_level`，发现原来是两者的标签设置不一样，`xgb_prediction`预测的结果是`-50000`和`+50000`两种，而原测试集目标变量`test$income_level`的标签是`-50000`和`50000+.`两个level，标签不同自然无法比较。

解决办法也挺简单，执行`test[,income_level:= ifelse(income_level == "-50000","-50000","+50000")]`将`50000+.`替换为`+50000`即可。

```r
> predict_xgb <- predict(xgmodel, test_task)
> xgb_prediction <- predict_xgb$data$response
> confusionMatrix(test$income_level, xgb_prediction)
Error in confusionMatrix.default(test$income_level, xgb_prediction) : 
  The data contain levels not found in the data.
> xgb_prediction
   [1] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
  [16] -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 +50000
  [31] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000 -50000 -50000 -50000
  [46] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
  [61] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
  [76] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
  [91] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [106] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000
 [121] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [136] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [151] -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000 -50000
 [166] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [181] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [196] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [211] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [226] +50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [241] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [256] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 +50000
 [271] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [286] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000
 [301] -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [316] -50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [331] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [346] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000 -50000 -50000
 [361] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [376] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 +50000 -50000 -50000 -50000 -50000
 [391] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [406] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [421] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [436] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [451] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [466] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [481] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [496] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [511] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [526] -50000 -50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [541] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [556] -50000 +50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000 -50000 -50000
 [571] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [586] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000 -50000 -50000 -50000
 [601] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [616] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [631] -50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [646] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [661] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000
 [676] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [691] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [706] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000
 [721] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [736] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [751] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000
 [766] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [781] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [796] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000 -50000
 [811] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [826] -50000 -50000 +50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [841] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [856] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [871] -50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 +50000 -50000 -50000 -50000 -50000 -50000 -50000
 [886] -50000 -50000 -50000 +50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 +50000
 [901] +50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [916] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000 -50000 -50000 -50000
 [931] +50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [946] -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [961] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [976] -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000 -50000
 [991] -50000 -50000 -50000 -50000 -50000 +50000 -50000 -50000 -50000 -50000
 [ reached getOption("max.print") -- omitted 98762 entries ]
Levels: -50000 +50000
> confusionMatrix(test$income_level, xgb_prediction)
Error in confusionMatrix.default(test$income_level, xgb_prediction) : 
  The data contain levels not found in the data.
> confusionMatrix(xgb_prediction$data$response,xgb_prediction$data$truth)
Error in xgb_prediction$data : $ operator is invalid for atomic vectors
> xg_confused <- confusionMatrix(test$income_level,xgb_prediction)
Error in confusionMatrix.default(test$income_level, xgb_prediction) : 
  The data contain levels not found in the data.
> test$income_level
   [1] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
  [14] -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000 
  [27] -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  50000+.
  [40] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
  [53] -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000 
  [66] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
  [79] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
  [92] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000 
 [105] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [118] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000 
 [131] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [144] -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  50000+. -50000 
 [157] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [170] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [183] -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [196] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000 
 [209] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [222] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000 
 [235] -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [248] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [261] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [274] -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [287] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000 
 [300] -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000 
 [313] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000 
 [326] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [339] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [352] -50000  -50000  50000+. 50000+. -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [365] -50000  -50000  -50000  50000+. -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000 
 [378] -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000 
 [391] 50000+. -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [404] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [417] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [430] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [443] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [456] -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000 
 [469] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [482] -50000  -50000  50000+. -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000 
 [495] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [508] -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000 
 [521] -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+.
 [534] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [547] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000 
 [560] -50000  -50000  50000+. -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [573] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [586] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [599] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000 
 [612] -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000 
 [625] -50000  -50000  50000+. -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000 
 [638] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [651] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000 
 [664] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000 
 [677] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [690] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [703] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000 
 [716] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000 
 [729] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [742] -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [755] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000 
 [768] -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. 50000+. -50000  -50000  -50000  -50000 
 [781] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000 
 [794] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+.
 [807] -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000 
 [820] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [833] -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000 
 [846] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [859] -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+.
 [872] -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [885] -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [898] -50000  -50000  50000+. 50000+. -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000 
 [911] -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000 
 [924] 50000+. -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  50000+. -50000  -50000  -50000 
 [937] -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000 
 [950] 50000+. -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [963] -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000 
 [976] -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000 
 [989] -50000  -50000  -50000  -50000  -50000  -50000  -50000  50000+. -50000  -50000  -50000  -50000 
 [ reached getOption("max.print") -- omitted 98762 entries ]
Levels: -50000 50000+.
> test[,income_level:= ifelse(income_level == "-50000","-50000","+50000")]
> test$income_level
   [1] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
  [12] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000"
  [23] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
  [34] "+50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000"
  [45] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
  [56] "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
  [67] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
  [78] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
  [89] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [100] "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [111] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [122] "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [133] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [144] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000"
 [155] "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [166] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [177] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000"
 [188] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [199] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000"
 [210] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [221] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [232] "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000"
 [243] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [254] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [265] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [276] "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [287] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [298] "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000"
 [309] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [320] "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [331] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [342] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [353] "-50000" "+50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [364] "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000"
 [375] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000"
 [386] "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [397] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [408] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [419] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [430] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [441] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [452] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000"
 [463] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [474] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000"
 [485] "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [496] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [507] "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [518] "-50000" "+50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [529] "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [540] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [551] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000"
 [562] "+50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [573] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [584] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [595] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [606] "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [617] "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000"
 [628] "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [639] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [650] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [661] "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [672] "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [683] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [694] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [705] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000"
 [716] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000"
 [727] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [738] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000"
 [749] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [760] "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [771] "-50000" "-50000" "-50000" "-50000" "+50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [782] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000"
 [793] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [804] "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000"
 [815] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [826] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [837] "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [848] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [859] "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [870] "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000"
 [881] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000"
 [892] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "+50000" "-50000"
 [903] "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000"
 [914] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "+50000"
 [925] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "+50000" "-50000" "-50000"
 [936] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000"
 [947] "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [958] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000"
 [969] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "-50000"
 [980] "-50000" "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000"
 [991] "-50000" "-50000" "-50000" "-50000" "-50000" "+50000" "-50000" "-50000" "-50000" "-50000"
 [ reached getOption("max.print") -- omitted 98762 entries ]
> xg_confused <- confusionMatrix(test$income_level,xgb_prediction)
> xg_confused
Confusion Matrix and Statistics

          Reference
Prediction -50000 +50000
    -50000  92699    877
    +50000   3433   2753
                                          
               Accuracy : 0.9568          
                 95% CI : (0.9555, 0.9581)
    No Information Rate : 0.9636          
    P-Value [Acc > NIR] : 1               
                                          
                  Kappa : 0.5398          
 Mcnemar's Test P-Value : <2e-16          
                                          
            Sensitivity : 0.9643          
            Specificity : 0.7584          
         Pos Pred Value : 0.9906          
         Neg Pred Value : 0.4450          
             Prevalence : 0.9636          
         Detection Rate : 0.9292          
   Detection Prevalence : 0.9380          
      Balanced Accuracy : 0.8613          
                                          
       'Positive' Class : -50000          
                                          
```

查看模糊矩阵得到的结果，模型准确率达到95.68%，并且特异度（Specificity），也就是对负样本的预测准确率达到75.84%，可以说，已经非常不错了！至此，UCI人口调查数据的折腾就暂时告一段落了，如果有时间我还会继续发表研究这个数据以及学习xgboost的心得！

（完）

# 参考链接 #

1. 关于xgboost不接受字符型变量的讨论：[Factors #95](https://github.com/dmlc/xgboost/issues/95)
2. 关于出现`unique() applies only to vectors`错误的讨论：[Error in unique.default(x, nmax = nmax) : unique() applies only to vectors #1407](https://github.com/mlr-org/mlr/issues/1407)；[Error in makeParamSet when tuning hyperparameters of nnet #1418](https://github.com/mlr-org/mlr/issues/1418)
3. mlr入门教程：[Machine Learning in R: mlr Tutorial](http://mlr-org.github.io/mlr-tutorial/devel/html/index.html)
4. [Get Started with XGBoost in R ](http://xgboost.readthedocs.io/en/latest//get_started/index.html#r)
4. 在R语言中针对初学者的xgboost和调参教程：[Beginners Tutorial on XGBoost and Parameter Tuning in R](https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/)
5. 知乎专栏：[强大的机器学习专属R包——mlr包](https://zhuanlan.zhihu.com/p/25595297)
6. [mlr-tutorial:Imbalanced Classification Problems](https://mlr-org.github.io/mlr-tutorial/release/html/over_and_undersampling/index.html)








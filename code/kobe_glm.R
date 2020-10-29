
#加载有关包
library(plyr)
library(dplyr)
library(ggplot2)
library(pROC)
library(ROCR)
library(e1071)
library(randomForest)
library(adabag)
library(data.table)
library(glmnet)
library(caret)

set.seed(1234)


#读取数据
data <- read.csv("C://Users//Lenovo//Desktop//kobe_data.csv")


#在action_type中将<20次的action列为others
data %>% count(action_type) %>% arrange(desc(n)) %>% filter(n < 20) -> actions
data$action_type[data$action_type %in% actions$action_type] <- "Others"


#新建特征变量为出手时间距离比赛开始有多场时间
seconds_starting <- (data$period-1)*12*60+(12-data$minutes_remaining)*60+(60-data$seconds_remaining)
seconds_remaining_period <- (data$minutes_remaining)*60+(data$seconds_remaining)
data <- data.frame(data,seconds_starting,seconds_remaining_period)


#选取变量
data <- data[,c(1,2,5:8,10:12,14:24)]


#把总数据集分为测试集与训练集
train <- data[!is.na(data$shot_made_flag),]
test <- data[is.na(data$shot_made_flag),]

#将shot_made_flag##转化为因子
train$shot_made_flag <- as.factor(train$shot_made_flag)
train$shot_made_flag <- factor(train$shot_made_flag, levels = c("1", "0"))

#将训练集中的70%用来训练，其余30%用来测试
N = length(train$shot_made_flag)
ind = sample(2,N,replace=TRUE, prob=c(0.7,0.3))
train_train <- train[ind==1,]
train_test <- train[ind==2,]


#GLM 广义线性回归
modelglm  <- glm(shot_made_flag~. , data=train_train,family='binomial'(link='logit'))
summary(modelglm)
preglm <- predict(modelglm, newdata = train_test, type = 'response')

#绘制ROC曲线
modelglm_roc=roc(train_test$shot_made_flag,preglm)
plot(modeglm_lroc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),
     grid.col=c('green','red'),max.auc.polygon=TRUE,
     auc.polygon.col='skyblue',print.thres=TRUE) 



#三折交叉验证
folds <- createFolds(y=train$shot_made_flag,k=5)
fold_accuracy_3 <- array(1:3)
for(i in 1:3){
  fold_test <- train[folds[[i]],]
  fold_train <-train[-folds[[i]],]
  fold_pre <- glm(shot_made_flag~. ,data = fold_train, family='binomial')
  fold_predict <- predict(fold_pre,type='response',newdata=fold_test)
  fold_predict = ifelse(fold_predict>0.5,1,0)
  fold_test$predict = fold_predict 
  fold_error = fold_predict -(as.numeric(fold_test$shot_made_flag)-1)
  #计算正确率
  fold_accuracy_3[i] = (nrow(fold_test)-sum(abs(fold_error)))/nrow(fold_test)
  print(fold_accuracy_3)
} 

acc_3 = mean(fold_accuracy_3)
sd_3 = sd(fold_accuracy_3)



#五折交叉验证
folds <- createFolds(y=train$shot_made_flag,k=5)
fold_accuracy_5 <- array(1:5)
for(i in 1:5){
  fold_test <- train[folds[[i]],]
  fold_train <-train[-folds[[i]],]
  fold_pre <- glm(shot_made_flag~. ,data = fold_train, family='binomial')
  fold_predict <- predict(fold_pre,type='response',newdata=fold_test)
  fold_predict = ifelse(fold_predict>0.5,1,0)
  fold_test$predict = fold_predict 
  fold_error = fold_predict -(as.numeric(fold_test$shot_made_flag)-1)
  #计算正确率
  fold_accuracy_5[i] = (nrow(fold_test)-sum(abs(fold_error)))/nrow(fold_test)
  print(fold_accuracy_5)
} 

acc_5 = mean(fold_accuracy_5)
sd_5 = sd(fold_accuracy_5)



#十折交叉验证
folds <- createFolds(y=train$shot_made_flag,k=10)
fold_accuracy_10 <- array(1:10)
for(i in 1:10){
  fold_test <- train[folds[[i]],]
  fold_train <-train[-folds[[i]],]
  fold_pre <- glm(shot_made_flag~. ,data = fold_train, family='binomial')
  fold_predict <- predict(fold_pre,type='response',newdata=fold_test)
  fold_predict = ifelse(fold_predict>0.5,1,0)
  fold_test$predict = fold_predict 
  fold_error = fold_predict -(as.numeric(fold_test$shot_made_flag)-1)
  #计算正确率
  fold_accuracy_10[i] = (nrow(fold_test)-sum(abs(fold_error)))/nrow(fold_test)
  print(fold_accuracy_10)
} 

acc_10 = mean(fold_accuracy_10)
sd_10 = sd(fold_accuracy_10)
  
  




#对test集进行预测
predictglm<-predict(modelglm,newdata = test2,type='response')









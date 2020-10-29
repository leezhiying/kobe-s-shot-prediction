
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
library(Matrix)
library(xgboost)

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



#将训练集中的70%用来训练，其余30%用来测试
N = length(train$shot_made_flag)
ind = sample(2,N,replace=TRUE, prob=c(0.7,0.3))
train_train <- train[ind==1,]
train_test <- train[ind==2,]


train_train.y =train_train$shot_made_flag
trainM<-data.matrix(train_train[,-11], rownames.force=NA)
testM <- data.matrix(train_test[,-11],rownames.force=NA)
modelxgb <- xgboost(data=trainM,label=train_train.y,max.depth=4,eta=0.035,nround=200,objective="binary:logistic")
predxgb <- predict(modelxgb,testM)
dtrain <- xgb.DMatrix(data=trainM, label=train_train.y, missing = NaN)
watchlist<-list(trainM=dtrain)
param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "error",
                eta                 = 0.035,
                max_depth           = 4,
                subsample           = 0.40,
                colsample_bytree    = 0.40
)

clf <- xgb.cv(  params              = param, 
                data                = dtrain, 
                nrounds             = 1500, 
                verbose             = 1,
                watchlist           = watchlist,
                maximize            = FALSE,
                nfold               = 10,
                early.stop.round    = 10,
                print.every.n       = 1
)

#draw roc
modelxgb_roc=roc(train_test$shot_made_flag,predxgb)
plot(modelxgb_roc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),
     grid.col=c('green','red'),max.auc.polygon=TRUE,
     auc.polygon.col='skyblue',print.thres=TRUE)




#预测预测集
train.y =train$shot_made_flag
trainM<-data.matrix(train[,-11], rownames.force=NA)
testM <- data.matrix(test[,-11],rownames.force=NA)
modelxgb <- xgboost(data=trainM,label=train.y,max.depth=4,eta=0.035,nround=200,objective="binary:logistic")
predxgb <- predict(modelxgb,testM)
predxgbe <- data.frame(predxgb,test$shot_id)
write.csv(predxgbe,file='C://Users//Lenovo//Desktop//si.csv')




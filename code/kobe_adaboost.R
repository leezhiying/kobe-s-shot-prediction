
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

train$shot_made_flag <- as.factor(train$shot_made_flag)
train$shot_made_flag <- factor(train$shot_made_flag, levels = c("1", "0"))

#将训练集中的70%用来训练，其余30%用来测试
N = length(train$shot_made_flag)
ind = sample(2,N,replace=TRUE, prob=c(0.7,0.3))
train_train <- train[ind==1,]
train_test <- train[ind==2,]

#adaboost
modeladaboost = boosting(shot_made_flag~. , data = train_train)
adapred <- predict(modeladaboost,newdata = train_test)
adap1 <- adapred$prob[,2]

#绘制roc曲线
modelada_roc=roc(train_test$shot_made_flag,adap1)
plot(modelada_roc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),
     grid.col=c('green','red'),max.auc.polygon=TRUE,
     auc.polygon.col='skyblue',print.thres=TRUE)




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
##数据预处理

#读取数据
data <- read.csv("C://Users//Lenovo//Desktop//kobe_data.csv")

#查看变量类型
str(data)

#在action_type中将<20次的action列为others
data %>% count(action_type) %>% arrange(desc(n)) %>% filter(n < 20) -> actions
data$action_type[data$action_type %in% actions$action_type] <- "Others"


#新建特征变量为出手时间距离比赛开始有多少分钟和多少秒
minutes_starting <- (data$period-1)*12+(12-data$minutes_remaining)
seconds_starting <- (data$period-1)*12*60+(12-data$minutes_remaining)*60+(60-data$seconds_remaining)
data <- data.frame(data,minutes_starting,seconds_starting)


#把总数据集分为测试集与训练集
train <- data[!is.na(data$shot_made_flag),]
test <- data[is.na(data$shot_made_flag),]


#将shot_made_flag##转化为因子
train$shot_made_flag <- as.factor(train$shot_made_flag)
train$shot_made_flag <- factor(train$shot_made_flag, levels = c("1", "0"))


##数据可视化

#画图函数，pplot为命中率与各种因素的柱状图 



##画图，出手点在篮球场上的分布图
courtplot <- function(feat) {
  feat <- substitute(feat)
  train %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_point(aes_q(color = feat), alpha = 0.7, size = 3) +
    ylim(c(33.7, 34.0883)) +
    scale_color_brewer(palette = "Set1") +
    theme_void() +
    ggtitle(paste(feat))
}
 
ggplot() +
  geom_point(data = filter(train, combined_shot_type == "Jump Shot"),
             aes(x = lon, y = lat), color = "grey", alpha = 0.3, size = 2) +
  geom_point(data = filter(train, combined_shot_type != "Jump Shot"),
             aes(x = lon, y = lat, 
                 color = combined_shot_type), alpha = 0.7, size = 3) +
  ylim(c(33.7, 34.0883)) +
  scale_color_brewer(palette = "Set1") +
  theme_void() +
  ggtitle("Shot Types")



##shot type 和shot accuracy 的关系
prop.table(table(train$action_type, train$shot_made_flag),1) -> temp
as.data.frame.matrix(temp) -> temp
temp$shot <- rownames(temp)
ggplot(temp, aes(x = reorder(shot, `1`), y = 1)) +
  geom_point(aes(y = `1`), size = 3, color = " dark blue", stat = "identity") +
  coord_flip() +
  labs(y = "Accuracy", x = "", title = "Accuracy by Shot_type")















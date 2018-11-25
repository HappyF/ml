#加入分析
setwd('f:/mtpa')
dodgers<-read.csv('Chapter_2/dodgers.csv')
library(car)
library(ggplot2)
library(ggthemes)
# for plots and data summaries
dodgers$ordered_day_of_week <- with(data=dodgers,
                                    ifelse ((day_of_week == "Monday"),1,
                                            ifelse ((day_of_week == "Tuesday"),2,
                                                    ifelse ((day_of_week == "Wednesday"),3,
                                                            ifelse ((day_of_week == "Thursday"),4,
                                                                    ifelse ((day_of_week == "Friday"),5,
                                                                            ifelse ((day_of_week == "Saturday"),6,7)))))))
#data.table的用法
head(df[,AgeDiscret := as.factor(round(Age/10,0))])               #:= 新增加一列
head(df[,AgeCat:= as.factor(ifelse(Age > 30, "Old", "Young"))])   #ifelse
df[,ID:=NULL]      


dodgers$ordered_day_of_week <- factor(dodgers$ordered_day_of_week, levels=1:7,
                                      labels=c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"))
head(dodgers)
str(dodgers)
#with(data=dodgers,plot(ordered_day_of_week, attend/1000, 
#                       xlab = "Day of Week", ylab = "Attendance (thousands)", 
#                       col = "violet", las = 1))
ggplot(dodgers,aes(x=ordered_day_of_week,y=attend/1000))+geom_boxplot()+
  xlab('Day of week')+ylab('Attendance (thousands)')+theme_economist()
with(dodgers, table(bobblehead,ordered_day_of_week))

dodgers$ordered_month <- with(data=dodgers,
                              ifelse ((month == "APR"),4,
                                      ifelse ((month == "MAY"),5,
                                              ifelse ((month == "JUN"),6,
                                                      ifelse ((month == "JUL"),7,
                                                              ifelse ((month == "AUG"),8,
                                                                      ifelse ((month == "SEP"),9,10)))))))
dodgers$ordered_month <- factor(dodgers$ordered_month, levels=4:10,
                                labels = c("April", "May", "June", "July", "Aug", "Sept", "Oct"))

ggplot(dodgers,aes(x=ordered_month,y=attend/1000))+geom_boxplot()+
  xlab('Day of week')+ylab('Attendance (thousands)')+theme_economist()

# exploratory data analysis displaying many variables
# looking at attendance and conditioning on day/night
# the skies and whether or not fireworks are displayed
#y 是上座率
ggplot(dodgers,aes(x=temp,y=attend/1000,color=fireworks))+geom_point(aes(shape=fireworks),size=4)+
  facet_wrap(~skies+day_night)

#航道图，看上座率与对手的关系，以及白天还是晚上
ggplot(dodgers,aes(x=attend/1000,y=opponent,color=bobblehead))+geom_point(size=4)

#做的粗糙一些，直接预测？
# specify a simple model with bobblehead entered last
my.model <- {attend ~ ordered_month + ordered_day_of_week + bobblehead}
set.seed(1234) # set seed for repeatability of training-and-test split
training_test <- c(rep(1,length=trunc((2/3)*nrow(dodgers))),
                   rep(2,length=(nrow(dodgers) - trunc((2/3)*nrow(dodgers)))))
training_test
dodgers$training_test <- sample(training_test) # random permutation 
dodgers$training_test <- factor(dodgers$training_test, 
                                levels=c(1,2), labels=c("TRAIN","TEST"))
dodgers.train <- subset(dodgers, training_test == "TRAIN")
print(str(dodgers.train)) # check training data frame
dodgers.test <- subset(dodgers, training_test == "TEST")
print(str(dodgers.test)) # check test data frame

# fit the model to the training set
train.model.fit <- lm(my.model, data = dodgers.train)
# obtain predictions from the training set
dodgers.train$predict_attend <- predict(train.model.fit) 

# evaluate the fitted model on the test set
dodgers.test$predict_attend <- predict(train.model.fit, 
                                       newdata = dodgers.test)

dodgers.plotting.frame <- rbind(dodgers.train,dodgers.test)
cat("\n","Proportion of Test Set Variance Accounted for: ",
    round((with(dodgers.test,cor(attend,predict_attend)^2)),
          digits=3),"\n",sep="")
ggplot(dodgers.plotting.frame,aes(x=attend/1000,y=predict_attend/1000,color=bobblehead))+geom_point()+
  facet_wrap(~training_test)

my.model.fit <- lm(my.model, data = dodgers)  # use all available data
print(summary(my.model.fit))
# tests statistical significance of the bobblehead promotion
# type I anova computes sums of squares for sequential tests
print(anova(my.model.fit))  

cat("\n","Estimated Effect of Bobblehead Promotion on Attendance: ",
    round(my.model.fit$coefficients[length(my.model.fit$coefficients)],
          digits = 0),"\n",sep="")

# standard graphics provide diagnostic plots
plot(my.model.fit)


# additional model diagnostics drawn from the car package
library(car)
residualPlots(my.model.fit)
marginalModelPlots(my.model.fit)
print(outlierTest(my.model.fit))

#也可以用随机森林等来做？
library(xgboost) 
library(caret)
#library(randomForest)
library(data.table)
library(readr)
dodgers<-data.table(dodgers)
#xgboost 需要什么
library(Matrix)
#形成哑变量
dummies<-dummyVars(~ordered_month+ordered_day_of_week+bobblehead,data=dodgers)
df_all_ohe <- as.data.frame(predict(dummies, newdata = dodgers))

df_all_combine<-cbind(dodgers[,.(attend,training_test)],df_all_ohe)

#训练
y<-dodgers[training_test=='TRAIN']$attend
x<-df_all_combine[training_test=='TRAIN']
x1<-x[,-c(1,2)]
xgb<-xgboost(data=data.matrix(x1),label=y,eta=0.3,max_depth=15,nround=100,
             colsample_bytree = 0.5, 
             seed = 2
)

#测试
x_test<-df_all_combine[training_test=='TEST']
y_pred <- predict(xgb, data.matrix(x_test[,-c(1,2)]))
y_test<-dodgers[training_test=='TEST']$attend
y_test
y_pred

model<-xgb.dump(xgb,with_stats=T)
model[1:10] #This statement prints top 10 nodes of the model

# 获得特征的真实名称
names <- dimnames(data.matrix(x1))[[2]]
names

# 计算特征重要性矩阵
importance_matrix <- xgb.importance(names, model = xgb)
# 制图
xgb.plot.importance(importance_matrix[1:10,])

test <- chisq.test(train$Age, output_vector)
print(test)
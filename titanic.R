setwd("C:/Users/My/Desktop/data science/statistics/lecture 7")
install.packages("csv")
library(csv)
df<-read.csv("titanic.csv")
View(df)
str(df)
summary(df)
#histogram
hist(df$age)
hist(df$fare)
#fare is not normally distributed
df$fare_sqrt<-sqrt(df$fare)
summary(df)
#missing value treatment
df$age[is.na(df$age)]=29
summary(df)
#creation of dummy variable sex and embarked
df$sex_dummy<-ifelse(df$sex=="female",1,0)
df$embarked_c<-ifelse(df$embarked=="C",1,0)
df$embarked_q<-ifelse(df$embarked=="q",1,0)
df$embarked_s<-ifelse(df$embarked=="S",1,0)
#removing the unwanted coloumn 
names(df)
df2<-df[ -c(3,9) ]
names(df2)
View(df2)
#outlier for age
bx=boxplot(df2$age)
bx$stats
quantile(df2$age,seq(0,1,0.02))
df2$age<-ifelse(df2$age>=52,52,df2$age)
df2$age<-ifelse(df2$age<=4,4,df$age)
#outlier for fare
bx2=boxplot(df2$fare_sqrt)
bx2$stats
quantile(df2$fare_sqrt,seq(0,1,0.2))
df2$fare_sqrt<-ifelse(df2$fare_sqrt>=9.66,9.66,df2$fare_sqrt)
#now check bivariate analysis and multicollinearity
install.packages("car")
library(car)
names(df2)
df3<-df2[-c(7,11,3)]
names(df3)
Lod<-lm(survived ~ .,data = df3)
View(Lod)
View(df3)
str(df3)
t = vif(Lod)
sort(t,decreasing = T )
#lets divide the data in test and train 
set.seed(222)
t=sample(1:nrow(df3),0.7*nrow(df3))
t_train=df3[t,]
t_test=df3[-t,]
#modelbuilding
mod1<-glm(as.factor(survived)~.,family = "binomial",data = t_train)
summary(mod1)
#removing based on the p value 
#instead removing all this variables one by one we use stepmethod which automatically calculated the best equation 
stpmod=step(mod1,direction = "both")
formula(stpmod)
summary(stpmod)
mod2<-glm(as.factor(survived)~pclass+age+sibsp+sex_dummy+embarked_c,family="binomial",data=t_train)
summary(mod2)          
t_train$score=predict(mod2,newdata = t_train,type="response")
head(t_train$score)
tail(t_train$score)
#lets try to analyse the confusion matrix and model accuracy
install.packages("lattice")
install.packages("ggplot2")
install.packages("caret")
install.packages("e1071")
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
summary(as.factor(df3$survived))
prediction<-ifelse(t_train$score>=0.6,1,0)
View(prediction)
confusionMatrix(as.factor(prediction),as.factor(t_train$survived),positive = "1")
#lets check the auc and roc
install.packages("InformationValue")
library(InformationValue)
plotROC(actuals = t_train$survived,predictedScores = as.numeric(fitted(mod2)))
#K-s plot
ks_plot(actuals = t_train$survived,predictedScores = as.numeric(fitted(mod2)))
#k-stats
ks_stat(actuals=t_train$survived,predictedScores = as.numeric(fitted(mod2)))
#prediction on test data
t_test$score2=predict(mod2,t_test,type='response')
View(t_test)
plotROC(actuals = t_test$survived,predictedScores = t_test$score2)

# ------------ Detecting diabetes using Supervised learning --------------------
diabetes<-read.csv("diabetes.csv")

m1<-apply(diabetes[diabetes$Outcome==1,-9],2,mean)
m2<-apply(diabetes[diabetes$Outcome==0,-9],2,mean)
l1<-length(diabetes$Outcome[diabetes$Outcome==1])
l2<-length(diabetes$Outcome[diabetes$Outcome==0])
x1<-diabetes[diabetes$Outcome==1,-9]
x2<-diabetes[diabetes$Outcome==0,-9]
S123<-((l1-1)*var(x1)+(l2-1)*var(x2))/(l1+l2-2)
a<-solve(S123)%*%(m1-m2)
a 

#Finding the threshold for classification
y1<-t(a)%*%m1
y2<-t(a)%*%m2
m<-(y1+y2)/2
m

#using in built function
library(MASS)
dis<-lda(Outcome~Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age,data=diabetes,prior=c(0.5,0.5))
dis$scaling # a is 1.382317 times of dis$scaling

newdata<-rbind(c(5,150,90,20,100,35,0.5,35))
colnames(newdata)<-colnames(diabetes[-9])
newdata
# prediction of classes for the new observations

newdata<-data.frame(newdata)
t(a)%*%t(newdata)
predict(dis,newdata=newdata)$class

# Confusion Matrix - to get "plug-in" estimate of misclassification rate
# Prediction of classes for observations in the sample
pred.group<-predict(dis,method="plug-in")$class
cbind(diabetes$Outcome, pred.group)
table(diabetes$Outcome, pred.group)

# Leave-one-out estimate of misclassification rate: use CV = TRUE option
dis.cv<-lda(Outcome~Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age,data=diabetes,prior=c(0.5,0.5), CV=TRUE)
names(dis.cv)
dis.cv$class
cbind(diabetes$Outcome, dis.cv$class)
table(diabetes$Outcome, dis.cv$class)


#Logistic regression

logfit <- glm(Outcome~Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age, family=binomial, data=diabetes)
summary(logfit)

table(diabetes$Outcome,(predict(logfit, type="response")>0.5))

predict(logfit,newdata=newdata,type="response")

#Cross-Validation (Leave-one-out method)
newpred1 <- numeric(length(diabetes$Outcome))

for (i in 1:length(diabetes$Outcome))
{
  newdat1 <- diabetes[-i,]
  newfit1 <- glm(Outcome~Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age, family=binomial, data=newdat1)
  newpred1[i] <- predict(newfit1, newdat1= data.frame(diabetes[i,-1]), type="response")
}

table(diabetes$Outcome,(newpred1>0.5))
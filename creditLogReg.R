library(ROCR)
library(dplyr)
windows()
creditcard <- read.csv(choose.files())
View(creditcard)
creditcard <- creditcard[,-1]
summary(creditcard)
str(creditcard)
sum(is.na(creditcard))

boxplot(creditcard$age)
barplot(creditcard$age)
hist(creditcard$age)
boxplot(creditcard$income)
barplot(creditcard$income)
hist(creditcard$income)
boxplot(creditcard$expenditure)
barplot(creditcard$expenditure)
hist(creditcard$expenditure)
boxplot(creditcard$share)
barplot(creditcard$share)
hist(creditcard$share)
boxplot(creditcard$months)
barplot(creditcard$months)
hist(creditcard$months)
boxplot(creditcard$active)
barplot(creditcard$active)
hist(creditcard$active)

cor(creditcard[-c(1,7,8)])

creditcard$card <- as.numeric(as.factor(creditcard$card))
creditcard$card <- factor(creditcard$card)
creditcard$owner <- as.numeric(factor(creditcard$owner))
creditcard$selfemp <- as.numeric(factor(creditcard$selfemp))

pairs(creditcard)
str(creditcard)
prop.table(table(creditcard$card))

model1 <- glm(card~., data = creditcard, family = "binomial")
pred1 <- predict(model1, creditcard, type = "response")
summary(model1)
exp(coef(model1))
confusion <- table(pred1>0.5, creditcard$card)
accuracy <- sum(diag(confusion)/sum(confusion))
accuracy

pred_values <- NULL
yes_no <- NULL
pred_values <- ifelse(pred1>=0.5,1,0)
yes_no <- ifelse(pred1>=0.5,"yes","no")

creditcard[,"pred1"] <- pred1
creditcard[,"pred_values"] <- pred_values
creditcard[,"yes_no"] <- yes_no

View(creditcard[,c(1,13:15)])
table(creditcard$card, creditcard$pred_values)

rocrpred<-prediction(pred1,creditcard$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
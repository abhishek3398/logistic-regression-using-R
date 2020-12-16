library(ROCR)
library(dplyr)
windows()
bank.full <- read.csv("E:/study material/abhishek excelr study material/data science study material/Redo assignments/logistic regression/bank-full.csv", sep=";")
View(bank.full)
summary(bank.full)
str(bank.full)
sum(is.na(bank.full))

boxplot(bank.full$age)
barplot(bank.full$age)
hist(bank.full$age)
boxplot(bank.full$balance)
barplot(bank.full$balance)
hist(bank.full$balance)
boxplot(bank.full$duration)
barplot(bank.full$duration)
hist(bank.full$duration)

cor(bank.full[c(1,6,10,12,13,14,15)])

bank.full$y <- as.numeric(factor(bank.full$y))
bank.full$y <- factor(bank.full$y)
str(bank.full)
prop.table(table(bank.full$y))*100

pairs.panels(bank.full)

model1 <- glm(y~., data = bank.full, family = "binomial")
pred1 <- predict(model1, bank.full, type = "response")
summary(model1)
exp(coef(model1))
confusion1 <- table(pred1>0.5, bank.full$y)
accuracy1 <- sum(diag(confusion1)/sum(confusion1))
accuracy1

model2 <- glm(y~.-job, data = bank.full, family = "binomial")
pred2 <- predict(model2, bank.full, type = "response")
summary(model2)
exp(coef(model2))
confusion2 <- table(pred2>0.5, bank.full$y)
accuracy2 <- sum(diag(confusion2)/sum(confusion2))
accuracy2

##model1 has the best fit value##

pred_values <- NULL
yes_no <- NULL
pred_values <- ifelse(pred1>=0.5,1,0)
yes_no <- ifelse(pred1>=0.5,"yes","no")

bank.full[,"pred1"] <- pred1
bank.full[,"pred_values"] <- pred_values
bank.full[,"yes_no"] <- yes_no

View(bank.full[,c(17:20)])
table(bank.full$y, bank.full$pred_values)

rocrpred<-prediction(pred1,bank.full$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)

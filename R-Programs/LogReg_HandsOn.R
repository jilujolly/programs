
## Universal data set
## Personal.LOan is Target variable

setwd("D:\\R\\Srinath_Regression")
data1 <- read.csv("UniversalBank.csv")

summary(data1)
str(data1)


View(data1)
hist(data1$Mortgage)

set.seed(123)
rows = 1:nrow(data1)
trainRows = sample(rows,round(0.7*nrow(data1)))

trainData = data1[trainRows,]
testData = data1[-trainRows,]

model = glm(Personal.Loan~.,
            data=trainData,
            family = binomial(link ="logit"))

summary(model)

#remove Id,

#trainData$ID1 = as.factor(trainData$ID)

#length(trainData$ID1)

model = glm(Personal.Loan~.-ID,
            data=trainData,
            family = binomial(link ="logit"))

summary(model)

# Experience has 36 -ve values 

trainData = trainData[trainData$Experience >= 0,]

summary(trainData) 

trainData$Personal.Loan = as.factor(trainData$Personal.Loan)

model = glm(Personal.Loan~.-ID-ZIP.Code,
            data=trainData,
            family = binomial(link ="logit"))

str(trainData)

View(trainData)

#trainData$ZIP.Code = as.factor(trainData$ZIP.Code)

summary(model)

cor(trainData)
library(corrplot)

corrplot(cor(trainData),method='number')


corrplot(cor(trainData[,-c(1,3,5,7,10)]),method='number')

#Mortgage isin skewed

hist(trainData$Mortgage)

trainData$Mortgage = ifelse(trainData$Mortgage > 0,1,0)


model = glm(Personal.Loan~.-ID-ZIP.Code-Experience-CCAvg,
            data=trainData,
            family = binomial(link ="logit"))


#Predictions

preds = predict(model,testData,type ="response")
testData$preds = preds

View(testData)
testData$preds = ifelse(testData$pred > 0.5,1,0)   ## reduce it to 0.4 to reduce the precision if you want to increase precision make it 0.6

table(testData$Personal.Loan,testData$preds,dnn=c('actuals','preds'))


#### ROC
library(ROCR)
pred = prediction(preds , testData$Personal.Loan)
perf= performance(pred, "tpr","fpr")
# perf= performance(pred, "tnr","fnr")
perf= performance(pred, "prec","rec")
# plot(perf,colorize = T)

plot(perf, colorize=T, print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(1.2,1.2), avg="threshold", lwd=3,
     main= "ROC")


AUC_1 = performance(pred, measure = 'auc')@y.values[[1]]
str(AUC_1)

AUC_1

# Explaination of above line
#AUC_1 = performance(pred, measure = 'auc')
#AUC_1@y.values[[1]]



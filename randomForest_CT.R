# get the data
# set working directory
setwd(dirname(file.choose()))
getwd()

#-----Section 02-------------------------------------------
# import data file bank.csv and put relevant variables in a data frame
bkdata <- read.csv("bankc.csv", stringsAsFactors = TRUE)

#-----Section 03-------------------------------------------
#################### exploration of dataset#################################

# examining the structure of the bkdata data frame
str(bkdata)

# checking for missing values
apply(bkdata, MARGIN = 2, FUN = function(x) sum(is.na(x)))
library(Amelia)
missmap(bkdata, col = c("black", "orange"), legend = FALSE)

bkdata2 <- bkdata
# applying casewise deletion 
#bkdata <- bkdata2[rowSums(is.unknown(bkdata)) == unknown, ]

#dropping the id feature
bkdata2 <- bkdata2[-1]

# summarizing the variables - see ranges, outliers? approx normal?
summary(bkdata2)

bkdata2$response <- factor(bkdata2$response, levels = c("no", "yes"))
str(bkdata2$response)
bkdata2$job <- factor(bkdata2$job, levels = c("admin.", "blue-collar", "entrepreneur", "housemaid",
                                              "management","retired", "self-employed", "services", 
                                              "student", "technician", "unemployed"))
bkdata2$marital <- factor(bkdata2$marital, levels = c("single", "married", "divorced"))
bkdata2$education <- factor(bkdata2$education, levels = c("primary", "secondary", "tertiary"))
bkdata2$credit_def <- factor(bkdata2$credit_def, levels = c("no", "yes"))
bkdata2$personal_loan <- factor(bkdata2$personal_loan, levels = c("no", "yes"))
bkdata2$communication <- factor(bkdata2$communication, levels = c("cellular", "telephone"))
bkdata2$poutcome <- factor(bkdata2$poutcome, levels = c("success", "failure"))
bkdata2$month <- factor(bkdata2$month, levels = c("apr", "aug", "dec", "feb", "jan", "jul", 
                                                  "jun", "mar", "may", "nov", "oct", "sep"))

#-------------------------section 4---------------------------------------------
#performing random forest
#randomizing, creating and testing datasets
library(Boruta)
library(mlbench)
library(caTools)
set.seed(1234)
boruta_model <- Boruta(response ~., data=bkdata2, doTrace=2, maxRuns=100)
print(boruta_model)
plot(boruta_model, cex.axis=0.7)
plotImpHistory(boruta_model)
attStats(boruta_model)
#tentative attributes
boruntatent <- TentativeRoughFix(boruta_model)
print(boruntatent)
getNonRejectedFormula(boruta_model)
getConfirmedFormula(boruta_model)

set.seed(1234)
bkdata2_random <- bkdata2[order(runif(6092)), ]
# Splitting dataset
split <- sample.split(bkdata2_random, SplitRatio = 0.8)
split

train_sample <- subset(bkdata2_random, split == "TRUE") #80%
test_sample <- subset(bkdata2_random, split == "FALSE") #20%

# check for the proportion of target variable
round(prop.table(table(train_sample$response)) *100,1)
round(prop.table(table(test_sample$response)) *100,1)


library(randomForest)
library(C50)

set.seed(1234)
bkdata2_RF <- randomForest(response ~ ., data = train_sample)

print(bkdata2_RF)
summary(bkdata2_RF)

# variable importance plot
varImpPlot(bkdata2_RF, main = "bkdata2_RF - variable importance")

# apply the model to make predictions
bkpred <- predict(bkdata2_RF, test_sample)

# evaluate
library(gmodels)
CrossTable(test_sample$response, bkpred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual response', 'predicted response'))
library(caret)
confusionMatrix(bkpred, test_sample$response, positive = "yes")

# obtain ROC and AUC
library(ROCR)
bkprob <- as.data.frame(predict(bkdata2_RF, test_sample, type = "prob"))
bkres <- as.data.frame(cbind(bkpred, bkprob))
head(bkres)
bkpred2 <- prediction(predictions = bkres$yes, labels = test_sample$response)
# ROC
bkperf1 <- performance(bkpred2, measure = "tpr", x.measure = "fpr")
plot(bkperf1, main = "ROC curve for term deposit prediction",
     col = "blue", lwd = 3)
abline(a = 0, b = 1, lty = 2)

#AUC
bkperf2 <- performance(bkpred2,  measure ="auc") 
bkperf2@y.values
#--------------------------section 5--------------------------------------------
#random forest for boruta model
set.seed(1234)
rfboruta <- randomForest(response ~ age + job + marital + education + credit_def + Avey_bal + 
                           house_loan + personal_loan + day + month + call_duration + 
                           campaign + pdays + previous + poutcome, data = train_sample)
print(rfboruta)
bkborpred <- predict(rfboruta, test_sample)

# evaluate
library(gmodels)
CrossTable(test_sample$response, bkborpred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual response', 'predicted response'))
library(caret)
confusionMatrix(bkborpred, test_sample$response, positive = "yes")

# obtain ROC and AUC
library(ROCR)
bkborprob <- as.data.frame(predict(rfboruta, test_sample, type = "prob"))
bkborres <- as.data.frame(cbind(bkborpred, bkborprob))
head(bkborres)
bkborpred2 <- prediction(predictions = bkborres$yes, labels = test_sample$response)
# ROC
bkborperf1 <- performance(bkborpred2, measure = "tpr", x.measure = "fpr")
plot(bkborperf1, main = "ROC curve for term deposit prediction",
     col = "blue", lwd = 3)
abline(a = 0, b = 1, lty = 2)

#AUC
bkborperf2 <- performance(bkborpred2,  measure ="auc") 
bkborperf2@y.values
#-------------------------------------------------------------------------------
#using get formula model
rfgetboruta <- randomForest(response ~ age + job + marital + education + Avey_bal
                            + house_loan + personal_loan + day + month + 
                              call_duration + campaign + pdays + previous +
                              poutcome, data = train_sample)

print(rfgetboruta)
bkgetpred <- predict(rfgetboruta, test_sample)

# evaluate
library(gmodels)
CrossTable(test_sample$response, bkgetpred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual response', 'predicted response'))
library(caret)
confusionMatrix(bkgetpred, test_sample$response, positive = "yes")
# obtain ROC and AUC
library(ROCR)
bkgetprob <- as.data.frame(predict(rfgetboruta, test_sample, type = "prob"))
bkgetres <- as.data.frame(cbind(bkgetpred, bkgetprob))
head(bkgetres)
bkgetpred2 <- prediction(predictions = bkgetres$yes, labels = test_sample$response)
# ROC
bkgetperf1 <- performance(bkgetpred2, measure = "tpr", x.measure = "fpr")
plot(bkgetperf1, main = "ROC curve for term deposit prediction",
     col = "blue", lwd = 3)
abline(a = 0, b = 1, lty = 2)

#AUC
bkgetperf2 <- performance(bkgetpred2,  measure ="auc") 
bkgetperf2@y.values


#-----------------------section 6-----------------------------------------------
#dropping less performing features manually
set.seed(1234)
bkdata2_rfv <- randomForest(response ~ call_duration + poutcome + month + pdays
                            +Avey_bal + job + day + age + house_loan + previous
                            + campaign, data = train_sample)
bkdata2_rfv
summary(bkdata2_rfv)
varImpPlot(bkdata2_rfv, main = "bkdata2_RFv - variable importance")
# apply the model to make predictions
bkpredv <- predict(bkdata2_rfv, test_sample)

# evaluate
library(gmodels)
CrossTable(test_sample$response, bkpredv,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual response', 'predicted response'))
library(caret)
confusionMatrix(bkpredv, test_sample$response, positive = "yes")

# obtain ROC and AUC
library(ROCR)
bkprobv <- as.data.frame(predict(bkdata2_rfv, test_sample, type = "prob"))
bkresv <- as.data.frame(cbind(bkpredv, bkprobv))
head(bkresv)
bkpred2v <- prediction(predictions = bkresv$yes, labels = test_sample$response)
# ROC
bkperf1v <- performance(bkpred2v, measure = "tpr", x.measure = "fpr")
plot(bkperf1v, main = "ROC curve for term deposit prediction",
     col = "green", lwd = 3)
abline(a = 0, b = 1, lty = 2)

#AUC
bkperf2v <- performance(bkpred2v,  measure ="auc") 
bkperf2v@y.values

#-------------------------Section 7-------------------------------------------
# auto-tune a random forest (TAKES TIME TO PROCESS)

bkctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

grid_rf <- expand.grid(.mtry = c(4, 8, 16, 32))
grid_rf

set.seed(1234)
bkdata2_RF2 <- train(response ~ ., data = train_sample, method = "rf",
                    metric = "Kappa", trControl = bkctrl,
                    tuneGrid = grid_rf)
# summary of model
bkdata2_RF2   

# apply the model to make predictions
bkpR <- predict(bkdata2_RF2, test_sample)

# evaluate
CrossTable(test_sample$response, bkpR,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual response', 'predicted response'))
confusionMatrix(bkpR, test_sample$response, positive = "yes")


# obtain ROC and AUC
prob <- as.data.frame(predict(bkdata2_RF2, test_sample, type = "prob"))
res <- as.data.frame(cbind(bkpR, prob))
head(res)
pred <- prediction(predictions = res$yes, labels = test_sample$response)
# ROC
perf1 <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf1, col="red", lwd = 2)
abline(a = 0, b = 1, lty = 2)
#AUC
perf2 <- performance(pred,  measure ="auc")
perf2@y.values

#----------------------------section 8----------------------------------------
# from the auto trained sample of random forest
set.seed(1234)
bkdata2_RF <- randomForest(response ~ ., data = train_sample, mtry=8, 
                           nodesize = 4, cutoff = c(.6,.1))
# summary of model
bkdata2_RF

# apply the model to make predictions
p1 <- predict(bkdata2_RF, test_sample)

# variable importance plot
varImpPlot(bkdata2_RF, main = "bkrf - variable importance")

# evaluate
CrossTable(test_sample$response, p1,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual response', 'predicted response'))
confusionMatrix(p1, test_sample$response, positive = "yes")

# obtain ROC and AUC
prob1 <- as.data.frame(predict(bkdata2_RF, test_sample, type = "prob"))
res1 <- as.data.frame(cbind(p1, prob1))
head(res1)
pred3 <- prediction(predictions = res1$yes, labels = test_sample$response)
# ROC
perf3 <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf3, main = "ROC curve for term deposit",
     col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2)
#AUC
perf3 <- performance(pred3,  measure ="auc")
perf3@y.values

#------------------------section 9----------------------------------------------
# Remove all variables from the environment
rm(list=ls())

  
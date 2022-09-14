# set working directory
setwd(dirname(file.choose()))
getwd()

library(MASS)
library(DMwR2)
library(polycor)
library(kernlab)
library(caret)

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
missmap(bkdata, col = c("black", "blue"), legend = FALSE)

bkdata2 <- bkdata

#dropping the id feature
bkdata2 <- bkdata2[-1]

# summarizing the variables - see ranges, outliers? approx normal?
summary(bkdata2)


#conversion of categorical into factors
bkdata2$education = factor(bkdata2$education)
bkdata2$response <- factor(bkdata2$response, levels = c("no", "yes"),
                           labels = c("0", "1"))
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

#----------------------------------
# create train and test data sets and randomize order of the data
# Splitting dataset
library(caTools)
set.seed(1234)
bkdata2_random <- bkdata2[order(runif(6092)), ]

split <- sample.split(bkdata2_random, SplitRatio = 0.8)
split

train_sample <- subset(bkdata2_random, split == "TRUE") #80%
test_sample <- subset(bkdata2_random, split == "FALSE")
bkdata2_Test <- test_sample$response#20%

# check for the proportion of target variable
round(prop.table(table(train_sample$response)) *100,1)
round(prop.table(table(test_sample$response)) *100,1)

#--------------------------Section 3-------------------------------------------
library(e1071) 
library(tidyverse)
library(RColorBrewer)
#bksvm <-svm(formula = response ~ ., data = train_sample, type = 'C-classification', 
# kernel = 'linear') 

library(kernlab)
bksvm <- ksvm(response ~ ., data = train_sample, cost=100, gamma=1,
                            kernel = "vanilladot")
bksvm

plot(bksvm, train_sample, fill = TRUE)

summary(bksvm)

# Predicting the Test set results 
bksvmpred <- predict(bksvm, newdata = test_sample[-17]) 

# Making the Confusion Matrix 
bksvmcm <- table(test_sample[, 17], bksvmpred)
bksvmcm

library(caret)
confusionMatrix(bksvmpred, test_sample$response, 
                dnn = c("bksvmpred", "response"), positive = "1")


library(pROC)
par(pty="s")
#VanillaDot Kernel Plot
MyPlot<- predict(bksvm, test_sample, type="response")
MyPP<-roc(test_sample$response ~ as.numeric(MyPlot))
plot(MyPP, plot=TRUE, print.auc=TRUE,
                  col="red", lwd=4, legacy.axes=TRUE,main="ROC Curve")
#------------------------------------------------------------------------------
#improving SVM model
bksvm_rbf <- ksvm(response ~ ., data = train_sample,
                              kernel = "rbfdot")
bksvm_rbf

bksvmrbf_prediction <- predict(bksvm_rbf, test_sample)
head(bksvmrbf_prediction)
table(bksvmrbf_prediction, test_sample$response)

agreement <- bksvmrbf_prediction == test_sample$response
table(agreement)
prop.table(table(agreement))

library(caret)
confusionMatrix(bksvmrbf_prediction, test_sample$response, 
                dnn = c("bksvmrbf_prediction", "response"), positive = "1")


library(pROC)
par(pty="s")
#VanillaDot Kernel Plot
MyPlot2<- predict(bksvm_rbf, test_sample, type="response")
MyPP2<-roc(test_sample$response ~ as.numeric(MyPlot2))
plot(MyPP2, plot=TRUE, print.auc=TRUE,
     col="red", lwd=4, legacy.axes=TRUE,main="ROC Curve")
#-----------------section 2----------------------------------------------------
#library(dplyr)
#train_sample %>% mutate_at(c(1), funs(c(scale(.))))  #performing scaling 
# add a train control and grid of cost values
# (C = trade-off between training error and flatness, but risk of over-fit)
trnctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
grid <- expand.grid(C = c(0.01, 0.03, 0.125, 0.5, 2, 4, 8, 16))  # exponential growth
set.seed(1234)
svm3 <- train(bksvmcm ~ ., data = train_sample, method = "svmLinear",
              trControl=trnctrl, tuneGrid = grid)




# Remove all variables from the environment
rm(list=ls())


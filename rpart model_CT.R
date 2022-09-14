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
missmap(bkdata, col = c("black", "green"), legend = FALSE)

bkdata2 <- bkdata
bkdata2 <- bkdata2[-1]
bkdata3 <-bkdata2
# summarizing the variables - see ranges, outliers? approx normal?

summary(bkdata2)

library(tidyverse)
library(caret)
library(C50)
library(plyr)
library(gmodels)
library(ROCR)
library(RWeka)
library(Fselector)
library(rpart)
library(rpart.plot)
library(dplyr)
library(xlsx)
library(data.tree)
library(caTools)

#converting to factors
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

#randomizing, creating and testing datasets
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

#or (-17 means that the target variable is excluded from the training)

library(rpart)
set.seed(1234)
bkdata2_model<- rpart(response ~ ., method="class", data = train_sample)
bkdata2_model
summary(bkdata2_model)
  #--------using CART model to visualize the model
library(rpart.plot)
rpart.plot(bkdata2_model)

set.seed(1234)
bank_pred <- predict(bkdata2_model, test_sample, type = "class")
bank_pred_table <- table(test_sample$response, bank_pred)
bank_pred_table

confusionMatrix(bank_pred, test_sample$response, positive = "yes")
plotcp(bkdata2_model)

set.seed(1234)
bkdata2_model2 = rpart(response ~ poutcome + call_duration + month + pdays
                       + job + age, cp = 0.09, data = train_sample)
bkdata2_model2
summary(bkdata2_model2)

rpart.plot(bkdata2_model2, main = "model 2")
bank_pred2 <- predict(bkdata2_model2, test_sample, type = "class")
bank_pred_table2 <- table(test_sample$response, bank_pred2)
bank_pred_table2

confusionMatrix(bank_pred2, test_sample$response, positive = "yes")
library(pROC)
?multiclass.roc
p1 <- predict(bkdata2_model2, test_sample, type = 'prob')
p1 <- p1[,2]
r <- multiclass.roc(test_sample$response, p1, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')

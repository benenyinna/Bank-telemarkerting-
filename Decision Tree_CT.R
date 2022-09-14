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
library(xlsx)
library(data.tree)
library(caTools)


#table summary for certain features
table(bkdata2$response) #for the target variable
table(bkdata2$job) #for job
table(bkdata2$education) #for education
table(bkdata2$marital)
table(bkdata2$credit_def)
table(bkdata2$personal_loan)
table(bkdata2$communication)
table(bkdata2$month)
table(bkdata2$poutcome)

summarize(bkdata2, mean_Avey_bal = mean(Avey_bal)) # can be used to observe the mean of a column
rename(bkdata3, averagebal= Avey_bal)     # can be used to change the name of a column 
browseVignettes("ggplot2")    # this can help you locate a web page for the package

library(dplyr)
filtered_bkdata3 <- filter(bkdata3, day==17)
View(filtered_bkdata3)
arrange(filtered_bkdata3, len)



filtered1_bkdata3 <- bkdata3 %>%
  filter(day==17) %>% 
  group_by(response) 

#%>%
  #summarise(mean_len =(len, na.rm=T),. group="drop")
  


#viewing dataset using cross table
cross_tr <- table(bkdata2$response) 
round (prop.table(cross_tr), digits = 2)
round(100*prop.table(cross_tr),digits=0)
cross_t1 <- xtabs(~ response+job, data=bkdata2) 
100*prop.table(cross_t1)
round(100*prop.table(cross_t1),digits=0)
cross_t2 <- xtabs(~ response+marital, data=bkdata2) 
100*prop.table(cross_t2)
cross_t3 <- xtabs(~ response+education, data=bkdata2) 
100*prop.table(cross_t3)
cross_t4 <- xtabs(~ response+credit_def, data=bkdata2) 
100*prop.table(cross_t4)
cross_t5 <- xtabs(~ response+personal_loan, data=bkdata2) 
100*prop.table(cross_t5)
cross_t6 <- xtabs(~ response+poutcome, data=bkdata2) 
100*prop.table(cross_t6)
cross_t7 <- xtabs(~ response+month, data=bkdata2) 
100*prop.table(cross_t7)

#visualizing the dataset using contingency tables
library(gmodels)
library(dplyr)
library(descr)
CrossTable(bkdata2$job, bkdata2$response)
CrossTable(cross_t3)
CrossTable(cross_t4)
CrossTable(cross_t5)
CrossTable(cross_t6)

#visualizing the variables using barchart

barplot(table(bkdata2$response), main = "term deposit response rate",
        xlab = "Response", ylab = "Frequency", col = c("#56B4E9", "#E69F00"))
barplot(table(bkdata2$job))
barplot(table(bkdata2$education))
barplot(table(bkdata2$marital), col = c("#999999", "#E69F00", "#56B4E9"))
barplot(table(bkdata2$credit_def))
barplot(table(bkdata2$poutcome))
barplot(table(bkdata2$month))

library(lessR)
BarChart(response, data=bkdata2,  main = "term deposit response rate")
BarChart(job, data=bkdata2, by=response, main= "job vs response rate",
         fill=getColors(c=90, l=80), values_color="gray20", quiet=TRUE)
BarChart(education, data=bkdata2, by=response, main= "Education vs response rate")
BarChart(communication, data=bkdata2, by=response)
BarChart(marital, data=bkdata2, by=response, main= "Martial Status vs response rate") 
BarChart(month, data=bkdata2, by=response,  main= "Months vs response rate", 
         fill=getColors(c=90, l=80), values_color="gray20", quiet=TRUE) 
BarChart(poutcome, data=bkdata2, by=response) 
BarChart(house_loan, data=bkdata2, by=response, main= "House Loans vs response rate")
BarChart(personal_loan, data=bkdata2, by=response, main= "Personal Loans vs response rate")
BarChart(credit_def, data=bkdata2, by=response, main= "Credit default vs response rate")
Histogram(Avey_bal, data=bkdata2)
Histogram(day, data = bkdata2)
Histogram(age, data=bkdata2, main = "Customers Age", fill=getColors(c=90, l=80, n=5), 
          values_color="gray20", quiet=TRUE)
Histogram(day, data=bkdata2, main = "day", fill=getColors(c=90, l=80, n=5), 
          values_color="gray20", quiet=TRUE)

library(ggplot2) 
ggplot (data= bkdata2,aes(x=month, y=Avey_bal )) + geom_bin2d(colour= "blue")
ggplot (data= bkdata2,aes(x=month, y=Avey_bal )) + geom_hex(colour= "blue")
ggplot(data=bkdata2, aes(x=month)) + geom_bar(colour="blue")
ggplot(data=bkdata2, aes(x=education, fill=response)) + geom_bar()
ggplot(data=bkdata2, aes(x=job, fill=response)) + geom_bar()
ggplot(data=bkdata2, aes(x=poutcome, fill=response)) + geom_bar()
ggplot(data=bkdata2, aes(x=response)) + geom_bar()
ggplot(data=bkdata2, aes(x=age, fill=response)) + geom_bar()
ggplot(data=bkdata2, aes(x=age)) + geom_bar()
ggplot (data= bkdata2,aes(age, Avey_bal)) + geom_point(colour= "blue")


#performing an interval measurement scale on age, Avey_bal
library(lessR)
bkdata3 = bkdata3 %>% 
 mutate(age = if_else(age > 65, "old", if_else(age > 30, "midage", "young")))
BarChart(age, data=bkdata3, by=response, fill=getColors(c=50, l=30), quiet=TRUE)
bkdata3 = bkdata3 %>% 
  mutate(Avey_bal = if_else(Avey_bal > 10000, "Rich", if_else(Avey_bal > 5000, 
                                                              "Average", "Lowclass")))
BarChart(Avey_bal, data=bkdata3, by=response, main = "Account balance vs response rate",
         fill=getColors(c=90, l=80), values_color="gray20", quiet=TRUE)
table(bkdata3$Avey_bal)
cross_t8 <- xtabs(~ response+Avey_bal, data=bkdata3) 
100*prop.table(cross_t8)
#------------------------section 4 (Decision Tree)------------------------------
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

head(bkdata2$age)
head(bkdata2_random$age)

# Splitting dataset
split <- sample.split(bkdata2_random, SplitRatio = 0.8)
split

train_sample <- subset(bkdata2_random, split == "TRUE") #80%
test_sample <- subset(bkdata2_random, split == "FALSE") #20%

# check for the proportion of target variable
round(prop.table(table(train_sample$response)) *100,1)
round(prop.table(table(test_sample$response)) *100,1)

#or (-17 means that the target variable is excluded from the training)
bkdata2_model<- C5.0(train_sample[-17], train_sample$response)
bkdata2_model
summary(bkdata2_model)
plot(bkdata2_model, subtree=16)

C5imp(bkdata2_model, metric = "usage", pct = TRUE)  # performing variable of importance in DT

library(rpart)   #--------using CART model to visualize the model
library(rpart.plot)
fit <- rpart(response~., data = train_sample, method = 'class')
rpart.plot(fit, extra = "auto", main= "decsion tree for train model")

#evaluating model performance 
set.seed(1234)
bkdata2_pred <- predict(bkdata2_model, test_sample)
library(gmodels)
CrossTable(test_sample$response, bkdata2_pred, prop.chisq = FALSE, prop.c = FALSE,
           dnn = c("actual response", "predicted response"))
#applying diagnostics
library(caret)
confusionMatrix(bkdata2_pred, test_sample$response, positive ="yes" )

#--------------------------------------------------------------------------------
# pruning the tree to simplify and/or avoid over-fitting
set.seed(1234)
bkdata2_prune <- C5.0(train_sample[-17], train_sample$response,
                     control = C5.0Control(minCases = 10)) # 10% training obs.
bkdata2_prune
summary(bkdata2_prune)
plot(bkdata2_prune)

bkdata2_prune_pred <- predict(bkdata2_prune, test_sample)
CrossTable(test_sample$response, bkdata2_prune_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual response', 'predicted response'))

confusionMatrix(bkdata2_prune_pred, test_sample$response, positive = "yes")
#-------------------------------------------------------------------------------
#improving model performance by boosting the accuracy using parameter tuning 
#applying diagnostics

set.seed(1234)
bkdata2_boost10 <-C5.0(train_sample[-17], train_sample$response, 
                      control = C5.0Control(minCases = 10),trials = 10) #9 trials
bkdata2_boost10
summary(bkdata2_boost10)
bkdata2_boost10_pred10 <- predict(bkdata2_boost10, test_sample)
CrossTable(test_sample$response, bkdata2_boost10_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual response', 'predicted reponse'))
#applying diagnostics
library(caret)
confusionMatrix(bkdata2_boost10_pred10, test_sample$response, positive ="yes" )

#--------------------
set.seed(1234)
bkdata2_boost7 <-C5.0(train_sample[-17], train_sample$response, 
                      control = C5.0Control(minCases = 10), trials = 7) #7 trials
bkdata2_boost7
summary(bkdata2_boost7)
bkdata2_boost7_pred7 <- predict(bkdata2_boost7, test_sample)
CrossTable(test_sample$response, bkdata2_boost7_pred7,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual response', 'predicted reponse'))
#applying diagnostics
library(caret)
confusionMatrix(bkdata2_boost7_pred7, test_sample$response, positive ="yes" )

#----------------section 5------------------------------------------------------
# applying cost matrix
matrix_dimensions <-list(c("no", "yes"), c("no","yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions$predicted
matrix_dimensions$actual
error_cost <- matrix(c(0,1,4,0), nrow=2, dimnames = matrix_dimensions)
error_cost
summary(error_cost)

set.seed(1234)
bkdata2_cost <- C5.0(train_sample[-17], train_sample$response, 
                     control = C5.0Control(minCases = 10), costs = error_cost)
bkdata2_cost
summary(bkdata2_cost)
bkdata2_cost_pred <- predict(bkdata2_cost, test_sample)
CrossTable(test_sample$response, bkdata2_cost_pred,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual response', 'predicted response'))

#applying diagnostics
library(caret)
confusionMatrix(bkdata2_cost_pred, test_sample$response, positive ="yes" )

#---------------section 6-------------------------------------------------------
#using rulesets instead of tree
set.seed(1234)
bkdata2_rules <- C5.0(train_sample[-17], train_sample$response, 
                      costs = error_cost,
                     control = C5.0Control(minCases = 10), rules = TRUE)
bkdata2_rules
summary(bkdata2_rules)

bkdata2_rules_pred <- predict(bkdata2_rules, test_sample)
CrossTable(test_sample$response, bkdata2_rules_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual response', 'predicted response'))

confusionMatrix(bkdata2_rules_pred, test_sample$response, positive = "yes")

#---------------section 7-------------------------------------------------------
# applying ROC curve and AUC and preparing probability data for outcomes
library(ROCR)
bkdata2_probability <- predict(bkdata2_model, test_sample, type = "prob")
# bind with test and earlier predicted data
bkdata2_bind <- cbind(test_sample, bkdata2_pred, bkdata2_probability)
head(bkdata2_bind)
# create a prediction object
bkdata2_pred2 <- prediction(predictions = bkdata2_bind$yes, 
                            labels = bkdata2_bind$response)
# plot ROC curve
bkdata2_perf1 <- performance(bkdata2_pred2, measure = "tpr", x.measure = "fpr")
plot(bkdata2_perf1, main = "ROC curve for model 1",
     col = "blue", lwd = 3)
abline(a = 0, b = 1, lty = 2)

# calculate the area under the curve (AUC)
bkdata2_perf2 <- performance(bkdata2_pred2,  measure ="auc")
bkdata2_perf2@y.values
#------------------------------------------------------------------------------
# applying ROC and AUC on prunned model
library(ROCR)
bkdata2_probabilityp <- predict(bkdata2_prune, test_sample, type = "prob")
# bind with test and earlier predicted data
bkdata2_bindp <- cbind(test_sample, bkdata2_prune_pred, bkdata2_probabilityp)
head(bkdata2_bindp)
# create a prediction object
bkdata2_predp <- prediction(predictions = bkdata2_bindp$yes, 
                            labels = bkdata2_bindp$response)
# plot ROC curve
bkdata2_perfp <- performance(bkdata2_predp, measure = "tpr", x.measure = "fpr")
plot(bkdata2_perfp, main = "ROC curve for prunned model prediction",
     col = "blue", lwd = 3)
abline(a = 0, b = 1, lty = 2)

# calculate the area under the curve (AUC)
bkdata2_perfp.1 <- performance(bkdata2_predp,  measure ="auc")
bkdata2_perfp.1@y.values
#-------------------------------------------------------------------------------
#applying ROC and AUC on boosted model of 10 trials
library(ROCR)
bkdata2_probability10 <- predict(bkdata2_boost10, test_sample, type = "prob")
# bind with test and earlier predicted data
bkdata2_bind10 <- cbind(test_sample, bkdata2_boost10_pred10, bkdata2_probability10)
head(bkdata2_bind10)
# create a prediction object
bkdata2_pred10 <- prediction(predictions = bkdata2_bind10$yes, 
                             labels = bkdata2_bind10$response)
# plot ROC curve
bkdata2_perf10 <- performance(bkdata2_pred10, measure = "tpr", x.measure = "fpr")
plot(bkdata2_perf10, main = "ROC curve for boosted model prediction",
     col = "blue", lwd = 3)
abline(a = 0, b = 1, lty = 2)

# calculate the area under the curve (AUC)
bkdata2_perf10.1 <- performance(bkdata2_pred10,  measure ="auc")
bkdata2_perf10.1@y.values
#-------------------------------------------------------------------------------
# applying ROC and AUC on boosted model of 7 trials
library(ROCR)
bkdata2_probability7 <- predict(bkdata2_boost7, test_sample, type = "prob")
# bind with test and earlier predicted data
bkdata2_bind7 <- cbind(test_sample, bkdata2_boost7_pred7, bkdata2_probability7)
head(bkdata2_bind7)
# create a prediction object
bkdata2_pred7 <- prediction(predictions = bkdata2_bind7$yes, 
                            labels = bkdata2_bind7$response)
# plot ROC curve
bkdata2_perf7 <- performance(bkdata2_pred7, measure = "tpr", x.measure = "fpr")
plot(bkdata2_perf7, main = "ROC curve for boosted model prediction",
     col = "blue", lwd = 3)
abline(a = 0, b = 1, lty = 2)

# calculate the area under the curve (AUC)
bkdata2_perf7.1 <- performance(bkdata2_pred7,  measure ="auc")
bkdata2_perf7.1@y.values

#-------------------------section 8------------------------------------
#Customizing tuning process to use cross validation

# using trainControl() to alter resampling strategy; cv = cross-validation
bkdata2_control <- trainControl(method = "cv", number = 10, selectionFunction = "oneSE")

# use expand.grid() to create grid of tuning parameters (winnow allows removal of low predictor variables)
grid <- expand.grid(.model = "rules",
                    .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    .winnow = "FALSE")

# look at the result of expand.grid()
grid

# customize train() with the control list and grid of parameters
set.seed(1234)
bkdata2_m <- train(response ~ ., data = train_sample, method = "C5.0",
           metric = "Kappa", trControl = bkdata2_control, tuneGrid = grid)
# summary of tuning results
bkdata2_m     #--------------------------------------------takes time to load

# apply the best C5.0 candidate model to make predictions
bkdata2_pp <- predict(bkdata2_m, test_sample)

# evaluate
CrossTable(test_sample$response, bkdata2_pp,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual response', 'predicted response'))
confusionMatrix(bkdata2_pp, test_sample$response, positive = "yes")

#-----------------section 12---------------------------------------------------
# checking for future performance of the 1st model using cross validation 
bkfolds <- createFolds(bkdata2$response, k = 10)
str(bkfolds)
bk01_test <- bkdata2[bkfolds$Fold01, ]
bk01_train <- bkdata2[-bkfolds$Fold01, ]

library(caret)
library(C50)
library(irr)

set.seed(1234)
bkfolds <- createFolds(bkdata2$response, k = 10) ############### error cost model
cv_resultsboost10 <- lapply(bkfolds, function(x) {
  train_sample <- bkdata2[-x, ]
  test_sample<- bkdata2[x, ]
  bkdata2_boost10 <- C5.0(response ~ ., data = train_sample)
  bkdata2_boost10_pred10 <- predict(bkdata2_boost10, test_sample)  
  bk_actual10 <- test_sample$response
  kappa <- kappa2(data.frame(bk_actual10, bkdata2_boost10_pred10))$value
  return(kappa)
})
str(cv_resultsboost10)
mean(unlist(cv_resultsboost10))

set.seed(1234)
bkfolds <- createFolds(bkdata2$response, k = 10) ###################### boost 10
cv_resultsboost7 <- lapply(bkfolds, function(x) {
  train_sample <- bkdata2[-x, ]
  test_sample<- bkdata2[x, ]
  bkdata2_boost7 <- C5.0(response ~ ., data = train_sample)
  bkdata2_boost7_pred7 <- predict(bkdata2_boost7, test_sample)
  bk_actualboost7 <- test_sample$response
  kappa <- kappa2(data.frame(bk_actualboost7, bkdata2_boost7_pred7))$value
  return(kappa)
})
str(cv_resultsboost7)
mean(unlist(cv_resultsboost7))

#------------------------------section 14---------------------------------------
#Improving model performance further using auto tune (best model approach (20 trials)
library(caret)
modelLookup("C5.0")
set.seed(300)
m <- train(response ~ ., data = bkdata2, method = "C5.0")
m
summary(m)
str(m)

m$finalModel    #--------------you can use this or run the code below
p <- predict(m, bkdata2)
table(p, bkdata2$response)
head(predict(m, bkdata2))
head(predict(m, bkdata2, type = "prob"))

#   Customizing the tuning process using the below process (oneSE approach which involves one trial )
ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "oneSE")
grid <- expand.grid(.model = "tree",
                    .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    .winnow = "FALSE")
set.seed(300)
onem <- train(response ~ ., data = bkdata2, method = "C5.0",metric = "Kappa",
             trControl = ctrl, tuneGrid = grid)
onem
summary(onem)
onem$finalModel
predonem <- predict(onem, bkdata2)
table(predonem, bkdata2$response)

#-----------------section 13----------------------------------------------------
#using bagging to improve model 
library(ipred)  ##### bagging for decision tree model################
set.seed(300)
mybag <- bagging(response ~ ., data = bkdata2, nbagg = 25)
bk_predbg <- predict(mybag, bkdata2)
table(bk_predbg, bkdata2$response)

library(caret)
set.seed(300)
bgctrl <- trainControl(method = "cv", number = 10)
train(response ~ ., data = bkdata2, method = "treebag",
        trControl = ctrl)


#---------------------------section 16------------------------------------------

# remove all variables from the environment
rm(list=ls())



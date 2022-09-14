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
missmap(bkdata, col = c("black", "blue"), legend = FALSE)

bkdata2 <- bkdata

#dropping the id feature
bkdata2 <- bkdata2[-1]

# summarizing the variables - see ranges, outliers? approx normal?
summary(bkdata2)

#----------------------------------section 4------------------------------------
library(caret)
cor(bkdata2[c("age", "Avey_bal", "call_duration", "campaign", "pdays", "previous")])

library(psych)
pairs.panels(bkdata2[c("age", "Avey_bal", "call_duration", "campaign", "pdays", "previous")])

#----------------------------------section 5------------------------------------
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


apply(bkdata2, MARGIN = 2, FUN = function(x) sum(is.na(x)))
library(Amelia)
#----------------------------------
# create train and test data sets and randomize order of the data
set.seed(1234)
bkdata2_model <- bkdata2[order(runif(6092)), ]

# Loading package
library(caTools)  # For Logistic regression
library(ROCR)      # For ROC curve to evaluate model

# Splitting dataset
split <- sample.split(bkdata2_model, SplitRatio = 0.8)
split

bkdata2.train <- subset(bkdata2_model, split == "TRUE")
bkdata2.test <- subset(bkdata2_model, split == "FALSE")
bkdata2_Test <- bkdata2.test$response

# Check proportions of low bith weight in train and test
round(prop.table(table(bkdata2.train$response))*100,1)
round(prop.table(table(bkdata2_Test))*100,1)

#---------------------------Section 7-------------------------------------------
# First round use all variables

bklogis = glm(response ~ age + job + marital+ education + credit_def + Avey_bal
              + house_loan + personal_loan + communication + day + month +
                call_duration + campaign + pdays + previous + poutcome, 
               data = bkdata2.train, family = "binomial")
summary(bklogis)

# Calculate Odds Ratio - Exp(b) with 95% confidence intervals (2 tail)
exp(cbind(OR = coef(bklogis), confint(bklogis)))

library(pscl)
pscl::pR2(bklogis)["McFadden"]
#---------------------------Section 8-------------------------------------------
# Second round including very significant variables 
set.seed(1234)
bklogis2 = glm(response ~ age + marital+ education + credit_def + Avey_bal
               + house_loan + personal_loan + communication + day + month +
                 call_duration + campaign + pdays + previous + poutcome, 
               data = bkdata2.train, family = "binomial")
summary(bklogis2)

# Calculate Odds Ratio - Exp(b) with 95% confidence intervals (2 tail)
exp(cbind(OR = coef(bklogis2), confint(bklogis2)))
caret::varImp(bklogis2) 
car::vif(bklogis2)

#--------------------------Section 9-------------------------------------------
# Predicting with model 1
bkt_test1 <- bkdata2.test[c("age",  "job", "marital", "education", "credit_def",  "Avey_bal",
                            "house_loan", "personal_loan",  "communication",  "day", "month",
                              "call_duration", "campaign", "pdays", "previous", "poutcome")]

bk2_pred <- predict.glm(bklogis, bkt_test1)
summary(bk2_pred)
head(bk2_pred)

bk2_pred <- ifelse(exp(bk2_pred) > 0.5,1,0)
bk2_pred <- as.factor(bk2_pred)


# Assess accuracy
library(gmodels)
CrossTable(x = bkdata2_Test, y = bk2_pred, prop.chisq = FALSE)

table(bk2_pred, bkdata2_Test)

library(caret)
confusionMatrix(bk2_pred, bkdata2_Test, positive = "1" )

caret::varImp(bklogis)     #performing variable importance to confirm level of importance

car::vif(bklogis)
#----------------------------------------------------- Predicting with model 2
bk2_test2 <- bkdata2.test[c("age", "marital", "education", "credit_def",  "Avey_bal",
                            "house_loan", "personal_loan",  "communication",  "day", "month",
                            "call_duration", "campaign", "pdays", "previous", "poutcome")]
               
bk2_pred2 <- predict.glm(bklogis2, bk2_test2)
head(bk2_pred2)
summary(bk2_pred2)
bk2_pred2 <- ifelse(exp(bk2_pred2) > 0.5,1,0)
bk2_pred2 <- as.factor(bk2_pred2)
head(bk2_pred2)

# Assess accuracy
library(gmodels)
CrossTable(x = bkdata2_Test, y = bk2_pred2, prop.chisq = FALSE)
table(bk2_pred2, bkdata2_Test)

library(caret)
confusionMatrix(bk2_pred2, bkdata2_Test, positive = "1" )
library(pscl)
pscl::pR2(bklogis2)["McFadden"] 
caret::varImp(bklogis2)     #performing variable importance to confirm level of importance

car::vif(bklogis2)

library(pROC)
par(pty="s") 
bkroc1 <- roc(bkdata2.test$response ~ predict(bklogis, bkdata2.test, type = "response")
              ,plot=TRUE,print.auc=TRUE,
              col="green",lwd =4,legacy.axes=TRUE,main="ROC curve for model 1 LG")
#-------------------------------section 11--------------------------------------
par(pty="s") 
bkroc2 <- roc(bkdata2.test$response ~ predict(bklogis2, bkdata2.test, type = "response")
                                                         ,plot=TRUE,print.auc=TRUE,
                          col="blue",lwd =4,legacy.axes=TRUE,main="ROC curve for model 2 LG")


#for exploring kappa statistics of a model
library(vcd)                #the unweighted is the value of focus
Kappa(table(bk2_pred2, bkdata2_Test))

#--------------------------Section 11-------------------------------------------
# Putting model3 into transportable xml using predictive model markup language
# McFadden's R squared (for logistic models)
bkMcd <- glm(response ~ 1, data = bkdata2, family = "binomial")
bcf <- 1 - logLik(bklogis2) / logLik(bkMcd)
bcf

# Export to pmml
library(pmml)
bk.pmml <- pmml(bklogis2)
bk.pmml
?save_pmml
save_pmml(bk.pmml, "LRMODEL.pmml")


#-----------------section 12----------------------------------------------------
# checking for future performance of the 1st model using cross validation



#--------------------------Section 12-------------------------------------------

# Remove all variables from the environment
rm(list=ls())


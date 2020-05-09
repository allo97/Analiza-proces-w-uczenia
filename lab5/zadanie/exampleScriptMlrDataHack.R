library(mlr)

## 1. Load the data
train <- read.csv("train_ctrUa4K.csv", na.strings = c(""," ",NA))
test <- read.csv("test_lAUu6dG.csv", na.strings = c(""," ",NA))


## 2. View of data
summarizeColumns(train)

summarizeColumns(test)


## check presence of skewness - wartości odstające, koncentracja wiekszości danych po jednej stronie
hist(train$ApplicantIncome, breaks = 300, main = "Applicant Income Chart",xlab = "ApplicantIncome")

hist(train$CoapplicantIncome, breaks = 100,main = "Coapplicant Income Chart",xlab = "CoapplicantIncome")


## Visualize outliers - wartosci odstające
boxplot(train$ApplicantIncome)

## Change class of CreditHistory to factor
train$Credit_History <- as.factor(train$Credit_History)

test$Credit_History <- as.factor(test$Credit_History)

class(train$Credit_History)

summary(train)

summary(test)


## change factor 3+ to 3 in Dependents
levels(train$Dependents)[4] <- "3"

levels(test$Dependents)[4] <- "3"


## 3. Missing Value Imputation - uzupelniam brakujace wartosci

#impute missing values by mean and mode
## Adding missing values where is NA

imp <- impute(train, classes = list(factor = imputeMode(), integer = imputeMean()), dummy.classes = c("integer","factor"), dummy.type = "numeric")
imp1 <- impute(test, classes = list(factor = imputeMode(), integer = imputeMean()), dummy.classes = c("integer","factor"), dummy.type = "numeric")

imp_train <- imp$data
imp_test <- imp1$data

summarizeColumns(imp_train)
summarizeColumns(imp_test)


listLearners("classif", check.packages = TRUE, properties = "missings")[c("class","package")]


## 4. Feature engineering

## Remove outliers from ApplicantIncome, CoapplicantIncome, LoanAmount

#for train data set
cd <- capLargeValues(imp_train, target = "Loan_Status",cols = c("ApplicantIncome"),threshold = 40000)
cd <- capLargeValues(cd, target = "Loan_Status",cols = c("CoapplicantIncome"),threshold = 21000)
cd <- capLargeValues(cd, target = "Loan_Status",cols = c("LoanAmount"),threshold = 520)

#rename the train data as cd_train
cd_train <- cd

#add a dummy Loan_Status column in test data
imp_test$Loan_Status <- sample(0:1,size = 367,replace = T)

cde <- capLargeValues(imp_test, target = "Loan_Status",cols = c("ApplicantIncome"),threshold = 33000)
cde <- capLargeValues(cde, target = "Loan_Status",cols = c("CoapplicantIncome"),threshold = 16000)
cde <- capLargeValues(cde, target = "Loan_Status",cols = c("LoanAmount"),threshold = 470)

#renaming test data
cd_test <- cde

#convert numeric to factor - train
for (f in names(cd_train[, c(14:20)])) {
  if( class(cd_train[, c(14:20)] [[f]]) == "numeric"){
    levels <- unique(cd_train[, c(14:20)][[f]])
    cd_train[, c(14:20)][[f]] <- as.factor(factor(cd_train[, c(14:20)][[f]], levels = levels))
  }
}

#convert numeric to factor - test
for (f in names(cd_test[, c(13:18)])) {
  if( class(cd_test[, c(13:18)] [[f]]) == "numeric"){
    levels <- unique(cd_test[, c(13:18)][[f]])
    cd_test[, c(13:18)][[f]] <- as.factor(factor(cd_test[, c(13:18)][[f]], levels = levels))
  }
}

summarizeColumns(cd_train)

summarizeColumns(cd_test)


#Total_Income
cd_train$Total_Income <- cd_train$ApplicantIncome + cd_train$CoapplicantIncome
cd_test$Total_Income <- cd_test$ApplicantIncome + cd_test$CoapplicantIncome

#Income by loan
cd_train$Income_by_loan <- cd_train$Total_Income/cd_train$LoanAmount
cd_test$Income_by_loan <- cd_test$Total_Income/cd_test$LoanAmount

#change variable class
cd_train$Loan_Amount_Term <- as.numeric(cd_train$Loan_Amount_Term)
cd_test$Loan_Amount_Term <- as.numeric(cd_test$Loan_Amount_Term)

#Loan amount by term
cd_train$Loan_amount_by_term <- cd_train$LoanAmount/cd_train$Loan_Amount_Term
cd_test$Loan_amount_by_term <- cd_test$LoanAmount/cd_test$Loan_Amount_Term


## Check correlation with existing variables

#splitting the data based on class
az <- split(names(cd_train), sapply(cd_train, function(x){ class(x)}))

#creating a data frame of numeric variables
xs <- cd_train[az$numeric]

#check correlation, there exists a very high correlation of Total_Income with ApplicantIncome, it means that the new variable is not providing any new information
cor(xs)

# Remove Total_income because too high correlation with ApplicantIncome
cd_train$Total_Income <- NULL
cd_test$Total_Income <- NULL

summarizeColumns(cd_train)
summarizeColumns(cd_test)

## 5. Machine Learning

#create a task
trainTask <- makeClassifTask(data = cd_train,target = "Loan_Status")
testTask <- makeClassifTask(data = cd_test, target = "Loan_Status")

## It is positive class
trainTask <- makeClassifTask(data = cd_train,target = "Loan_Status", positive = "Y")


## Normalize Data
#normalize the variables
trainTask <- normalizeFeatures(trainTask,method = "standardize")
testTask <- normalizeFeatures(testTask,method = "standardize")

## Remove not required variables
trainTask <- dropFeatures(task = trainTask,features = c("Loan_ID","Married.dummy"))

#Feature importance, check which variable are important
im_feat <- generateFilterValuesData(trainTask, method = c("information.gain"))
plotFilterValues(im_feat,n.show = 20)

im_feat <- generateFilterValuesData(trainTask, method = c("chi.squared"))
plotFilterValues(im_feat,n.show = 20)

## 1. Quadratic Discriminant Anaysis

#load qda 
qda.learner <- makeLearner("classif.qda", predict.type = "response")

#train model
qmodel <- train(qda.learner, trainTask)

#predict on test data
qpredict <- predict(qmodel, testTask)

#create submission file
submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = qpredict$data$response)

## 2. Logistic Regression

logistic.learner <- makeLearner("classif.logreg",predict.type = "response")

#cross validation (cv) accuracy
cv.logistic <- crossval(learner = logistic.learner,task = trainTask,iters = 3,stratify = TRUE,measures = acc,show.info = F)

#cross validation accuracy
cv.logistic$aggr
#acc.test.mean
#0.7947553

#train model
fmodel <- train(logistic.learner,trainTask)
getLearnerModel(fmodel)

#predict on test data
fpmodel <- predict(fmodel, testTask)

#create submission file
submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = fpmodel$data$response)


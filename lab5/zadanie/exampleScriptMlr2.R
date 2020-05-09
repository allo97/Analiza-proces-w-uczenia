# Przyklad klasyfikacji złamania kości

library(mlr)

df <- read.table("Dane.csv", na.strings=c("", "NA"),header = TRUE, dec=",", sep = ";")

df <- df[,!(names(df) %in% c("last_name","first_name","BirthTme","DateOfExam","Gluc","LF","Ca_plus_plus","Mn"))]

df1 <- na.omit(df)

df1[] <- lapply(df1, as.numeric)

df1$Fractures <- factor(df1$Fractures)

## Define the task
task = makeClassifTask(id = "fractures_classification",data = df1, target = "Fractures")

## Define the learner from package MASS
library(MASS)
lrn = makeLearner("classif.lda")

## Define the resampling strategy
rdesc = makeResampleDesc(method = "CV", stratify = TRUE)

## Do the resampling
r = resample(learner = lrn, task = task, resampling = rdesc,show.info = FALSE)
## Get the mean misclassification error
r$aggr
#> mmce.test.mean
#> 0.02
lrns <- makeLearners(c("lda","rpart", "C50","rFerns","randomForestSRC"), type = "classif")

porownanie <- benchmark(learners = lrns,tasks = task,resampling = cv5)
#reper

save(porownanie, file = "porownanie.rda")

load("porownanie.rda")

porownanie

plotBMRBoxplots(porownanie)